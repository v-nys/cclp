#lang typed/racket
(require "abstract-multi-domain.rkt")

(struct abstract-equality ([term1 : AbstractDomainElem] [term2 : AbstractDomainElem]) #:transparent)
(provide (struct-out abstract-equality))
(define-type AbstractSubstitution (Listof abstract-equality))
(provide AbstractSubstitution)

; note: can only substitute for an abstract variable, and there is never any reason to substitute an atom or conjunction for something
(: substitute-in-term (-> AbstractTerm AbstractVariable AbstractTerm AbstractTerm))
(define (substitute-in-term substituter substitutee input-elem)
  (match (list substituter substitutee input-elem)
    [(list _ (a i) (a j)) (if (equal? i j) substituter input-elem)]
    [(list _ (g i) (g j)) (if (equal? i j) substituter input-elem)]
    [(list _ t (abstract-function symbol args)) (abstract-function symbol (map (λ ([a : AbstractTerm]) (substitute-in-term substituter substitutee a)) args))]
    [else input-elem])); TODO: multi

(: substitute-in-atom (-> AbstractTerm AbstractVariable abstract-atom abstract-atom))
(define (substitute-in-atom substituter substitutee input-atom)
  (match input-atom [(abstract-atom sym args) (abstract-atom sym (map (λ ([a : AbstractTerm]) (substitute-in-term substituter substitutee a)) args))]))

(: substitute-in-conjunction (-> AbstractTerm AbstractVariable AbstractConjunction AbstractConjunction))
(define (substitute-in-conjunction substituter substitutee conjunction) (map (λ ([a : abstract-atom]) (substitute-in-atom substituter substitutee a)) conjunction))

(: substitute-in-domain-elem (-> AbstractTerm AbstractVariable AbstractDomainElem AbstractDomainElem))
(define (substitute-in-domain-elem substituter substitutee elem)
  (cond [(abstract-atom? elem) (substitute-in-atom substituter substitutee elem)]
        [(AbstractTerm? elem) (substitute-in-term substituter substitutee elem)]
        [(AbstractConjunction? elem) (substitute-in-conjunction substituter substitutee elem)]))

(: substitute-in-substitution (-> AbstractTerm AbstractVariable AbstractSubstitution AbstractSubstitution))
(define (substitute-in-substitution substituter substitutee input-subst)
  (map (λ ([aeq : abstract-equality])
         (abstract-equality (substitute-in-domain-elem substituter substitutee (abstract-equality-term1 aeq))
                            (substitute-in-domain-elem substituter substitutee (abstract-equality-term2 aeq)))) input-subst))
(provide substitute-in-substitution)