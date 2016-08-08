#lang typed/racket
(require "abstract-multi-domain.rkt")

(struct abstract-equality ([term1 : AbstractDomainElem] [term2 : AbstractDomainElem]) #:transparent)
(provide (struct-out abstract-equality))
(define-type AbstractSubstitution (Listof abstract-equality))
(provide AbstractSubstitution)

; note: can only substitute for an abstract variable!
(: substitute-in-domain-elem (-> AbstractDomainElem AbstractVariable AbstractDomainElem AbstractDomainElem))
(define (substitute-in-domain-elem substituter substitutee input-elem)
  (match (list substituter substitutee input-elem)
    [(list _ (a i) (a j)) (if (equal? i j) substituter input-elem)]
    [(list _ (g i) (g j)) (if (equal? i j) substituter input-elem)]
    [(list _ (a i) (abstract-function symbol args)) (abstract-function symbol (map (λ (a) (substitute-in-domain-elem substituter substitutee a)) args))] 
    [else input-elem])); TODO implement

(: substitute-in-substitution (-> AbstractDomainElem AbstractVariable AbstractSubstitution AbstractSubstitution))
(define (substitute-in-substitution substituter substitutee input-subst)
  input-subst) ; TODO implement

;
;substitute_in_term substituter substitutee@(FuncTerm (AConst (A i))) (FuncTerm (ADomainFunc symbol terms)) = FuncTerm (ADomainFunc symbol newterms)
;  where newterms = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) terms
;substitute_in_term substituter substitutee@(FuncTerm (AConst (UG i))) (FuncTerm (ADomainFunc symbol terms)) = FuncTerm (ADomainFunc symbol newterms)
;  where newterms = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) terms
;
;substitute_in_term substituter substitutee@(FuncTerm (AConst (A i))) (ConjunctTerm (CAtom (Atom symbol terms))) = ConjunctTerm (CAtom (Atom symbol newterms))
;  where newterms = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) terms
;substitute_in_term substituter substitutee@(FuncTerm (AConst (UG i))) (ConjunctTerm (CAtom (Atom symbol terms))) = ConjunctTerm (CAtom (Atom symbol newterms))
;  where newterms = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) terms
;
;substitute_in_term substituter substitutee@(FuncTerm (AConst (A i))) (ConjunctTerm (Multi gram init f)) = ConjunctTerm $ Multi gram init2 f2
;  where init2 = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) init
;        f2 = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) f
;substitute_in_term substituter substitutee@(FuncTerm (AConst (UG i))) (ConjunctTerm (Multi gram init f)) = ConjunctTerm $ Multi gram init2 f2
;  where init2 = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) init
;        f2 = map (functerm . (substitute_in_term substituter substitutee) . FuncTerm) f