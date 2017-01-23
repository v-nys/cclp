; MIT License
;
; Copyright (c) 2016 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#lang racket
(require "abstract-substitution.rkt")
(require "data-utils.rkt")
(require "abstract-multi-domain.rkt")
(require "abstraction-inspection-utils.rkt")

;(: maximum-var-index-in-substitution (-> (-> AbstractVariable Boolean) AbstractSubstitution (Opt Integer)))
(define (maximum-var-index-in-substitution right-variable-type? substitution)
  (foldl (λ (eq acc)
           (let ([max-aeq (maximum-var-index-in-equality right-variable-type? eq)])
             (cond [(none? acc) max-aeq]
                   [(none? max-aeq) acc]
                   [else (some (max (some-v acc) (some-v max-aeq)))]))) (none) substitution))
(provide maximum-var-index-in-substitution)

;(: maximum-var-index-in-equality (-> (-> AbstractVariable Boolean) abstract-equality (Opt Integer)))
(define (maximum-var-index-in-equality right-variable-type? aeq)
  (let ([max-lhs (maximum-var-index (abstract-equality-term1 aeq) right-variable-type?)]
        [max-rhs (maximum-var-index (abstract-equality-term2 aeq) right-variable-type?)])
    (cond [(none? max-lhs) max-rhs]
          [(none? max-rhs) max-lhs]
          [else (some (max (some-v max-lhs) (some-v max-rhs)))])))

; TODO: some equalities are irrelevant for resolution
; e.g. a.../a... is provably redundant because this gives us a result in solved form
; removing these will lead to fewer index renamings and more readable output
(define (abstract-unify subst additional-g-offset)
  (match subst
    [(list) (some (list))]
    [(list-rest (abstract-equality t1 t2) tail)
     #:when (equal? t1 t2)
     (abstract-unify tail additional-g-offset)]
    [(list-rest (abstract-equality v t) tail)
     #:when (and (abstract-variable? v) (occurs v t))
     (none)]
    [(list-rest (abstract-equality (a i) t) tail) #:when (abstract-term? t) ; we can't unify a conjunct or conjunction with a term
                                                  (let* ([max-nested-g (maximum-var-index t g?)]
                                                         [substituted (substitute-in-substitution t (a i) tail)]
                                                         ; the if-expression in the recursion is a little tricky
                                                         ; suppose a_i does not occur in the tail and t contains a nested g, larger than the additional offset
                                                         ; then the additional offset could be too small for the recursion, so we take max if necessary
                                                         [recursion (abstract-unify substituted (if (some? max-nested-g) (max (some-v max-nested-g) additional-g-offset) additional-g-offset))])
                                                    (cond [(none? recursion) recursion]
                                                          [else (some (cons (abstract-equality (a i) t) (some-v recursion)))]))]
    [(list-rest (abstract-equality (abstract-atom sym1 args1) (abstract-atom sym2 args2)) tail)
     (if (and (equal? sym1 sym2) (equal? (length args1) (length args2)))
         (abstract-unify (append (for/list ([arg1 args1] [arg2 args2]) (abstract-equality arg1 arg2)) tail) additional-g-offset)
         (none))]
    ; note: unification of conjunctions isn't explicitly mentioned in the paper, but this is it
    [(list-rest (abstract-equality lst1 lst2) tail)
     #:when (and (list? lst1) (list? lst2))
     (abstract-unify
      (for/list
          ([atom1 lst1] [atom2 lst2])
        (abstract-equality atom1 atom2))
      additional-g-offset)]
    [(list-rest (abstract-equality (abstract-function sym1 args1) (abstract-function sym2 args2)) tail)
     (if (and (equal? sym1 sym2) (equal? (length args1) (length args2)))
         (abstract-unify (append (for/list ([arg1 args1] [arg2 args2]) (abstract-equality arg1 arg2)) tail) additional-g-offset)
         (none))]
    [(list-rest (abstract-equality t (a i)) tail) (abstract-unify (cons (abstract-equality (a i) t) tail) additional-g-offset)]
    [(list-rest (abstract-equality (abstract-function sym args) (g i)) tail) (abstract-unify (cons (abstract-equality (g i) (abstract-function sym args)) tail) additional-g-offset)]
    [(list-rest (abstract-equality (g i) t) tail) #:when (abstract-term? t)
                                                  (let* ([max-g (maximum-var-index-in-substitution g? (cons (abstract-equality (g i) t) tail))]
                                                         [g-offset (+ additional-g-offset (if (some? max-g) (some-v max-g) 1))]
                                                         [nested-any-indices (assemble-var-indices a? t)]
                                                         [equalities (map (λ (idx) (abstract-equality (a idx) (g (+ idx g-offset)))) nested-any-indices)]
                                                         [substituted-substitution (substitute-in-substitution t (g i) tail)])
                                                    (if (empty? nested-any-indices)
                                                        (let ([rest (abstract-unify substituted-substitution additional-g-offset)]) (if (none? rest) rest (some (cons (abstract-equality (g i) t) (some-v rest)))))
                                                        (abstract-unify (append equalities (cons (abstract-equality (g i) t) substituted-substitution)) additional-g-offset)))]
    [else (none)]))
(provide (contract-out [abstract-unify (-> (listof abstract-equality?) exact-nonnegative-integer? (maybe (listof abstract-equality?)))]))

(define (occurs avar aterm)
  (match aterm
    [(abstract-function symbol args) (ormap (λ (elem) (occurs avar elem)) args)]
    [(abstract-atom symbol args) (ormap (λ (elem) (occurs avar elem)) args)]
    [else (equal? avar aterm)]))
(provide occurs)

(module+ test
  (require rackunit)
  (require (for-syntax syntax/parse))
  (require "cclp-interpreter.rkt")

  (require (for-syntax (only-in "abstract-substitution.rkt" asubst)))

  ; the occurs check
  (check-true (occurs (g 1) (interpret-abstract-term "γ1")))
  (check-false (occurs (g 1) (interpret-abstract-term "γ2")))
  (check-true (occurs (a 1) (interpret-abstract-term "α1")))
  (check-false (occurs (a 1) (interpret-abstract-term "α2")))

  (check-false (occurs (a 1) (interpret-abstract-term "someconstant")))
  (check-false (occurs (g 1) (interpret-abstract-term "someconstant")))

  (check-true (occurs (a 1) (interpret-abstract-term "foo(bar(α1))")))
  (check-false (occurs (a 1) (interpret-abstract-term "foo(bar(α2))")))
  (check-true (occurs (g 1) (interpret-abstract-term "foo(bar(γ1))")))
  (check-false (occurs (g 1) (interpret-abstract-term "foo(bar(γ2))")))

  (check-true (occurs (a 1) (interpret-abstract-atom "foo(bar(α1))")))
  (check-false (occurs (a 1) (interpret-abstract-atom "foo(bar(α2))")))
  (check-true (occurs (g 1) (interpret-abstract-atom "foo(bar(γ1))")))
  (check-false (occurs (g 1) (interpret-abstract-atom "foo(bar(γ2))")))

  (check-equal? (abstract-unify (asubst (foo bar)) 0) (none) "unification of different functions")
  (check-equal? (abstract-unify (asubst (foo foo)) 0) (some (list)) "unification of identical functions")
  (check-equal? (abstract-unify (asubst ((a 1) (a 2))) 0) (some (asubst ((a 1) (a 2)))) "unification of equivalent variables")
  (check-equal? (abstract-unify (asubst ((a 1) (g 1))) 0) (some (asubst ((a 1) (g 1)))) "unification of most general with more specific variable")
  (check-equal? (abstract-unify (asubst ((foo [(bar [(a 1)])]) (foo [(bar [(g 1)])]))) 0) (some (asubst ((a 1) (g 1)))) "unification of nested terms")
  ;(check-equal? (abstract-unify (term-equality-list ("γ1" "node(α1)")) 0) (some (term-equality-list ("α1" "γ2") ("γ1" "node(γ2)"))) "unification introducing new variable")

  (check-equal? (abstract-unify (asubst ((cons [(g 1) nil]) (cons [(g 2) nil]))) 0) (some (asubst ((g 1) (g 2)))) "unification of lists")
  (check-equal? (abstract-unify (asubst ((append [(a 13) (a 14) (a 12)]) (append [(a 19) (a 20) (a 18)]))) 0) (some (asubst ((a 13) (a 19)) ((a 14) (a 20)) ((a 12) (a 18)))) "unification of renamings respects the order of the arguments")
  ;(check-equal? (abstract-unify (term-equality-list ("collect(γ3,α3)" "collect(node(α1),cons(α1,γ4))")) 0) (some (term-equality-list ("α1" "γ5") ("γ3" "node(γ5)") ("α3" "cons(γ5,γ4)"))))

  (test-case "unification of more complex terms"
             (check-true
              (some?
               (abstract-unify
                (asubst ((collect [(g 1) (a 4)]) (collect [(tree [(a 5) (a 6)]) (a 7)]))) 0)))
             (check-equal? (list->set (some-v (abstract-unify (asubst ((collect [(g 1) (a 4)]) (collect [(tree [(a 5) (a 6)]) (a 7)]))) 0)))
                           (list->set (asubst ((a 5) (g 6)) ((a 6) (g 7)) ((g 1) (tree [(g 6) (g 7)])) ((a 4) (a 7)))))))
