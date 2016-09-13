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

(define (write-abstract-equality obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:abstract-equality ~s ~s)" (abstract-equality-term1 obj) (abstract-equality-term2 obj))
      (fprintf port "~v=~v" (abstract-equality-term1 obj) (abstract-equality-term2 obj))))

; terms are really any abstract domain elements
(struct abstract-equality (term1 term2) #:transparent #:methods gen:custom-write [(define write-proc write-abstract-equality)])
(provide (struct-out abstract-equality))

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
(define (apply-substitution-to-rule subst r)
  (rule (apply-substitution-to-conjunct subst (rule-head r)) (apply-substitution-to-conjunction subst (rule-body r))))

;(: apply-substitution-to-full-evaluation (-> AbstractSubstitution full-evaluation full-evaluation))
(define (apply-substitution-to-full-evaluation subst fe)
  (full-evaluation (apply-substitution-to-conjunct subst (full-evaluation-input-pattern fe)) (apply-substitution-to-conjunct subst (full-evaluation-output-pattern fe))))

;(: apply-substitution-to-knowledge (-> AbstractSubstitution AbstractKnowledge AbstractKnowledge))
(define (apply-substitution-to-knowledge subst knowledge)
  (if (rule? knowledge)
      (apply-substitution-to-rule subst knowledge)
      (apply-substitution-to-full-evaluation subst knowledge)))
(provide apply-substitution-to-knowledge)

;(: apply-substitution (-> AbstractSubstitution AbstractDomainElem AbstractDomainElem))
(define (apply-substitution subst domain-elem)
  (cond [(abstract-term? domain-elem) (apply-substitution-to-term subst domain-elem)]
        [(abstract-atom? domain-elem) (apply-substitution-to-conjunct subst domain-elem)]
        [(list? domain-elem) (apply-substitution-to-conjunction subst domain-elem)]))
(provide apply-substitution)