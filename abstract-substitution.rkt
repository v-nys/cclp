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
(require "data-utils.rkt")
(require (for-syntax syntax/parse))
(require racket/serialize)

(require scribble/srcdoc)

(define (write-abstract-equality obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:abstract-equality ~s ~s)" (abstract-equality-term1 obj) (abstract-equality-term2 obj))
      (fprintf port "~v/~v" (abstract-equality-term1 obj) (abstract-equality-term2 obj))))

; terms are really any abstract domain elements
(serializable-struct abstract-equality (term1 term2) #:transparent #:methods gen:custom-write [(define write-proc write-abstract-equality)])
(provide (struct-out abstract-equality))

(define (abstract-substitution? l) (and (list? l) (andmap abstract-equality? l)))
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
(define (substitute-in-conjunct substituter substitutee input-conjunct)
  (match input-conjunct
    [(abstract-atom sym args) (abstract-atom sym (map (λ (a) (substitute-in-term substituter substitutee a)) args))]
    [(multi c1 a i c2 f rta)
     (multi c1 a (init (map (match-lambda [(cons lhs rhs) (cons lhs (substitute-in-term substituter substitutee rhs))]) (init-constraints i))) c2 (final (map (match-lambda [(cons lhs rhs) (cons lhs (substitute-in-term substituter substitutee rhs))]) (final-constraints f))) rta)]))

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
   (apply-substitution-to-conjunct subst (full-evaluation-output-pattern fe))
   (full-evaluation-idx fe)))
(provide apply-substitution-to-full-evaluation)

(define (apply-substitution subst substitution-object)
  (cond [(abstract-term? substitution-object) (apply-substitution-to-term subst substitution-object)]
        [(abstract-conjunct? substitution-object) (apply-substitution-to-conjunct subst substitution-object)]
        [(list? substitution-object) (apply-substitution-to-conjunction subst substitution-object)]
        [(abstract-rule? substitution-object) (apply-substitution-to-abstract-rule subst substitution-object)]
        [(full-evaluation? substitution-object) (apply-substitution-to-full-evaluation subst substitution-object)]))
(provide
 (proc-doc/names
  apply-substitution
  (-> abstract-substitution?
      (or/c abstract-domain-elem*?
            abstract-knowledge?
            (listof abstract-conjunct?))
      (or/c abstract-domain-elem*?
            abstract-knowledge?
            (listof abstract-conjunct?)))
  (subst substitution-object)
  ("One documentation-time expression" "Another documentation-time expression")))

(define-syntax (t stx)
  (syntax-parse stx
    [(_ ((~literal a) NUM))
     #'(a NUM)]
    [(_ ((~literal g) NUM))
     #'(g NUM)]
    [(_ id:id)
     #'(abstract-function (quote id) (list))]
    [(_ (id:id [ARG ...]))
     #'(abstract-function (quote id) (list (t ARG) ...))]))
(define-syntax (aeq stx)
  (syntax-parse stx
    [(_ (TERM1 TERM2))
     #'(abstract-equality (t TERM1) (t TERM2))]))
(define-syntax (asubst stx)
  (syntax-parse stx
    [(_ SUBST-PAIR ...)
     #'(list (aeq SUBST-PAIR) ...)]))
(provide asubst)

;(module+ test
;  (require rackunit)
;  (require "cclp-interpreter.rkt")
;  (require "abstraction-inspection-utils.rkt")
;  (check-equal? (substitute-in-substitution (interpret-abstract-term "γ5") (interpret-abstract-term "α1") (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))
;                (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,γ5,α2))"))))
;  (check-equal? (substitute-in-substitution (interpret-abstract-term "γ5") (interpret-abstract-term "α4") (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))
;                (list (abstract-equality (interpret-abstract-term "γ5") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))
;  (check-equal? (apply-substitution-to-term (asubst ((g 1) quux) ((a 2) (g 4)))
;                                            (interpret-abstract-term "foo(bar(γ1,α1),baz(γ2,α2,α3))"))
;                (interpret-abstract-term "foo(bar(quux,α1),baz(γ2,γ4,α3))"))
;  (check-equal? (apply-substitution-to-conjunct (asubst ((g 1) quux) ((a 2) (g 4)))
;                                                (interpret-abstract-atom "foo(bar(γ1,α1),baz(γ2,α2,α3))"))
;                (interpret-abstract-atom "foo(bar(quux,α1),baz(γ2,γ4,α3))"))
;  (check-equal? (apply-substitution-to-conjunction (asubst ((g 1) quux) ((a 2) (g 4)))
;                                                   (list (interpret-abstract-atom "foo(bar(γ1,α1),baz(γ2,α2,α3))") (interpret-abstract-atom "zip(zoom(γ1,α1),kweh(α2,γ2,α5))")))
;                (list (interpret-abstract-atom "foo(bar(quux,α1),baz(γ2,γ4,α3))") (interpret-abstract-atom "zip(zoom(quux,α1),kweh(γ4,γ2,α5))")))
;  (check-equal? (apply-substitution-to-full-evaluation
;                 (asubst
;                  ((a 2) (a 16))
;                  ((a 1) (a 15))
;                  ((g 2) (g 21))
;                  ((g 1) (g 20)))
;                 (full-evaluation
;                  (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
;                  (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
;                (full-evaluation
;                 (interpret-abstract-atom "del(α15,[γ20|γ21],α16)")
;                 (interpret-abstract-atom "del(γ3,[γ20|γ21],γ4)"))))