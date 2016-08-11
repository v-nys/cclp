#lang typed/racket

; cannot define these functions in the domain module, would cause circular import between domain module and unification module

(: >=-extension (-> AbstractDomainElem AbstractDomainElem Boolean))
(define (>=-extension domain-elem1 domain-elem2)
  (or (and (a? domain-elem1) (AbstractTerm? domain-elem2))
      (and (g? domain-elem1) (g? domain-elem2))
      (and (g? domain-elem1) (and (abstract-function? domain-elem2) (andmap (λ (arg) (>=-extension domain-elem1 arg)) (abstract-function-args domain-elem2))))
      ; due to how unification keeps more general terms on the left, this is an easy, correct way to check generality *and* aliasing
      (and ((abstract-function? domain-elem1)
            (abstract-function? domain-elem2)
            (equal? (abstract-function-functor domain-elem1) (abstract-function-functor domain-elem2))
            (let ([unification (abstract-unify domain-elem1 domain-elem2)])
              (and (some? unification) (equal? (apply-substitution-to-term unification domain-elem1) domain-elem2)))))
      (and ((abstract-atom? domain-elem1)
            (abstract-atom? domain-elem2)
            (equal? (abstract-atom-symbol domain-elem1) (abstract-atom-symbol domain-elem2))
            (let ([unification (abstract-unify domain-elem1 domain-elem2)])
              (and (some? unification) (equal? (apply-substitution-to-atom unification domain-elem1) domain-elem2)))))
      ))