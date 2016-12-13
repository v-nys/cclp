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
(require "abstract-multi-domain.rkt")
(require "abstract-knowledge.rkt")
(require "abstraction-inspection-utils.rkt")
(require "abstract-multi-domain.rkt")
(require "data-utils.rkt")
(require racket/serialize)

(require scribble/srcdoc)

(define (write-abstract-equality obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:abstract-equality ~s ~s)" (abstract-equality-term1 obj) (abstract-equality-term2 obj))
      (fprintf port "~v/~v" (abstract-equality-term1 obj) (abstract-equality-term2 obj))))

; terms are really any abstract domain elements
(serializable-struct abstract-equality (term1 term2) #:transparent #:methods gen:custom-write [(define write-proc write-abstract-equality)])
(provide (struct-out abstract-equality))

(define (abstract-substitution? l) (andmap abstract-equality? l))
(provide abstract-substitution?)

; note: can only substitute for an abstract variable, and there is never any reason to substitute an atom or conjunction for something, so use terms
;(: substitute-in-term (-> AbstractTerm AbstractVariable AbstractTerm AbstractTerm))
(define (substitute-in-term substituter substitutee input-elem)
  (match (list substituter substitutee input-elem)
    [(list _ (a i) (a j)) (if (equal? i j) substituter input-elem)]
    [(list _ (g i) (g j)) (if (equal? i j) substituter input-elem)]
    [(list _ t (abstract-function symbol args)) (abstract-function symbol (map (λ (a) (substitute-in-term substituter substitutee a)) args))]
    [else input-elem]))

;(: substitute-in-conjunct (-> AbstractTerm AbstractVariable abstract-atom abstract-atom))
(define (substitute-in-conjunct substituter substitutee input-atom)
  (match input-atom [(abstract-atom sym args) (abstract-atom sym (map (λ (a) (substitute-in-term substituter substitutee a)) args))]))

;(: substitute-in-conjunction (-> AbstractTerm AbstractVariable AbstractConjunction AbstractConjunction))
(define (substitute-in-conjunction substituter substitutee conjunction) (map (λ (a) (substitute-in-conjunct substituter substitutee a)) conjunction))

;(: substitute-in-domain-elem (-> AbstractTerm AbstractVariable AbstractDomainElem AbstractDomainElem))
(define (substitute-in-domain-elem substituter substitutee elem)
  (cond [(abstract-atom? elem) (substitute-in-conjunct substituter substitutee elem)]
        [(abstract-term? elem) (substitute-in-term substituter substitutee elem)]
        [(list? elem) (substitute-in-conjunction substituter substitutee elem)]))

;(: substitute-in-substitution (-> AbstractTerm AbstractVariable AbstractSubstitution AbstractSubstitution))
(define (substitute-in-substitution substituter substitutee input-subst)
  (map (λ (aeq)
         (abstract-equality (substitute-in-domain-elem substituter substitutee (abstract-equality-term1 aeq))
                            (substitute-in-domain-elem substituter substitutee (abstract-equality-term2 aeq)))) input-subst))
(provide substitute-in-substitution)

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

; TODO can probably clean this up significantly now that this is no longer in TR

;(: apply-substitution-to-term (-> AbstractSubstitution AbstractTerm AbstractTerm))
(define (apply-substitution-to-term subst t)
  (foldl (λ (el acc)
           (substitute-in-term (abstract-equality-term2 el)
                               (abstract-equality-term1 el)
                               acc))
         t subst))
(provide apply-substitution-to-term)

; pretty much the same as applying to term - if I knew how to express a more flexible type, I could probably merge the two
;(: apply-substitution-to-conjunct (-> AbstractSubstitution AbstractConjunct AbstractConjunct))
(define (apply-substitution-to-conjunct subst t)
  (foldl (λ (el acc)
           (substitute-in-conjunct (abstract-equality-term2 el)
                                   (abstract-equality-term1 el)
                                   acc))
         t subst))
(provide apply-substitution-to-conjunct)

;(: apply-substitution-to-conjunction (-> AbstractSubstitution AbstractConjunction AbstractConjunction))
(define (apply-substitution-to-conjunction subst conjunction)
  (map (λ (conjunct) (apply-substitution-to-conjunct subst conjunct)) conjunction))
(provide apply-substitution-to-conjunction)

;(: apply-substitution-to-rule (-> AbstractSubstitution rule rule))
(define (apply-substitution-to-abstract-rule subst r)
  (abstract-rule (apply-substitution-to-conjunct subst (abstract-rule-head r)) (apply-substitution-to-conjunction subst (abstract-rule-body r))))

;(: apply-substitution-to-full-evaluation (-> AbstractSubstitution full-evaluation full-evaluation))
(define (apply-substitution-to-full-evaluation subst fe)
  (full-evaluation
   (apply-substitution-to-conjunct subst (full-evaluation-input-pattern fe))
   (apply-substitution-to-conjunct subst (full-evaluation-output-pattern fe))))
(provide apply-substitution-to-full-evaluation)

(define (apply-substitution subst substitution-object)
  (cond [(abstract-term? substitution-object) (apply-substitution-to-term subst substitution-object)]
        [(abstract-atom? substitution-object) (apply-substitution-to-conjunct subst substitution-object)]
        [(list? substitution-object) (apply-substitution-to-conjunction subst substitution-object)]
        [(abstract-rule? substitution-object) (apply-substitution-to-abstract-rule subst substitution-object)]
        [(full-evaluation? substitution-object) (apply-substitution-to-full-evaluation subst substitution-object)]))
(provide
 (proc-doc/names
  apply-substitution
  (-> abstract-substitution?
      (or/c abstract-domain-elem?
            abstract-knowledge?)
      (or/c abstract-domain-elem?
            abstract-knowledge?))
  (subst substitution-object)
  ("One documentation-time expression" "Another documentation-time expression")))

(module+ test
  (require rackunit)
  (require "cclp-interpreter.rkt")
  (check-equal? (maximum-var-index (interpret-abstract-term "γ1") g?) (some 1))
  (check-equal? (maximum-var-index (interpret-abstract-term "γ1") a?) (none))
  (check-equal? (maximum-var-index (interpret-abstract-term "α2") a?) (some 2))
  (check-equal? (maximum-var-index (interpret-abstract-term "α2") g?) (none))

  (check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,α2)") g?) (some 1))
  (check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,α2)") a?) (some 2))
  (check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,γ2)") a?) (none))
  (check-equal? (maximum-var-index (interpret-abstract-term "foo(α1,α2)") g?) (none))

  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,α2)") g?) (some 1))
  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,α2)") a?) (some 2))
  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,γ2)") a?) (none))
  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(α1,α2)") g?) (none))

  (check-equal? (substitute-in-substitution (interpret-abstract-term "γ5") (interpret-abstract-term "α1") (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))
                (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,γ5,α2))"))))
  (check-equal? (substitute-in-substitution (interpret-abstract-term "γ5") (interpret-abstract-term "α4") (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))
                (list (abstract-equality (interpret-abstract-term "γ5") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))

  (check-equal? (apply-substitution-to-term (list (abstract-equality (g 1) (interpret-abstract-term "quux")) (abstract-equality (a 2) (g 4)))
                                            (interpret-abstract-term "foo(bar(γ1,α1),baz(γ2,α2,α3))"))
                (interpret-abstract-term "foo(bar(quux,α1),baz(γ2,γ4,α3))"))

  (check-equal? (apply-substitution-to-conjunct (list (abstract-equality (g 1) (interpret-abstract-term "quux")) (abstract-equality (a 2) (g 4)))
                                                (interpret-abstract-atom "foo(bar(γ1,α1),baz(γ2,α2,α3))"))
                (interpret-abstract-atom "foo(bar(quux,α1),baz(γ2,γ4,α3))"))

  (check-equal? (apply-substitution-to-conjunction (list (abstract-equality (g 1) (interpret-abstract-term "quux")) (abstract-equality (a 2) (g 4)))
                                                   (list (interpret-abstract-atom "foo(bar(γ1,α1),baz(γ2,α2,α3))") (interpret-abstract-atom "zip(zoom(γ1,α1),kweh(α2,γ2,α5))")))
                (list (interpret-abstract-atom "foo(bar(quux,α1),baz(γ2,γ4,α3))") (interpret-abstract-atom "zip(zoom(quux,α1),kweh(γ4,γ2,α5))")))

  (check-equal? (apply-substitution-to-full-evaluation
                 (list (abstract-equality (a 2) (a 16))
                       (abstract-equality (a 1) (a 15))
                       (abstract-equality (g 2) (g 21))
                       (abstract-equality (g 1) (g 20)))
                 (full-evaluation
                  (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                  (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
                (full-evaluation
                 (interpret-abstract-atom "del(α15,[γ20|γ21],α16)")
                 (interpret-abstract-atom "del(γ3,[γ20|γ21],γ4)"))))