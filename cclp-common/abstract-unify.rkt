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

#lang alpha-gamma racket
(require cclp-common-data/abstract-substitution
         cclp-common/abstract-substitution-application)
(require "data-utils.rkt")
(require cclp-common-data/abstract-multi-domain)
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
; additional-g-offset is for newly introduced g's (e.g. when t(a) = g)
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
     #:when (and (list? lst1) (list? lst2) (equal? (length lst1) (length lst2)))
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
    [(list-rest (abstract-equality (g i) t) tail)
     #:when (abstract-term? t)
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

  ; the occurs check
  (check-true (occurs (g 1) (g 1)))
  (check-false (occurs (g 1) (g 2)))
  (check-true (occurs (a 1) (a 1)))
  (check-false (occurs (a 1) (a 2)))

  (check-false (occurs (a 1) β(someconstant)))
  (check-false (occurs (g 1) β(someconstant)))

  (check-true (occurs (a 1) β(foo(bar(a1)))))
  (check-false (occurs (a 1) β(foo(bar(a2)))))
  (check-true (occurs (g 1) β(foo(bar(g1)))))
  (check-false (occurs (g 1) β(foo(bar(g2)))))

  (check-true (occurs (a 1) α(foo(bar(a1)))))
  (check-false (occurs (a 1) α(foo(bar(a2)))))
  (check-true (occurs (g 1) α(foo(bar(g1)))))
  (check-false (occurs (g 1) α(foo(bar(g2)))))

  (check-equal? (abstract-unify α({foo / bar}) 0) (none) "unification of different functions")
  (check-equal? (abstract-unify α({foo / foo}) 0) (some empty) "unification of identical functions")
  (check-equal? (abstract-unify α({a1 / a2}) 0) (some α({a1 / a2})) "unification of equivalent variables")
  (check-equal? (abstract-unify α({a1 / g1}) 0) (some α({a1 / g1})) "unification of most general with more specific variable")
  (check-equal? (abstract-unify α({foo(bar(a1)) / foo(bar(g1))}) 0) (some α({a1 / g1})) "unification of nested terms")
  (check-equal? (abstract-unify α({g1 / node(a1)}) 0) (some α({a1 / g2, g1 / node(g2)})) "unification introducing new variable")

  (check-equal? (abstract-unify α({[g1] / [g2]}) 0) (some α({g1 / g2})) "unification of lists")
  (check-equal? (abstract-unify α({append(a13,a14,a12) / append(a19,a20,a18)}) 0) (some α({a13/a19, a14/a20, a12/a18})) "unification of renamings respects the order of the arguments")
  (check-equal? (abstract-unify α({collect(g3,a3)/collect(node(a1),[a1|g4])}) 0) (some α({a1/g5,g3/node(g5),a3/[g5|g4]})))

  (test-case "unification of more complex terms"
             (check-true
              (some?
               (abstract-unify
                α({collect(g1,a4)/collect(tree(a5,a6),a7)}) 0)))
             (check-equal? (list->set (some-v (abstract-unify α({collect(g1,a4)/collect(tree(a5,a6),a7)}) 0)))
                           (list->set α({a5/g6,a6/g7,g1/tree(g6,g7),a4/a7})))))
