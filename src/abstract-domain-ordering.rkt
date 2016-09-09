#lang typed/racket

; cannot define these functions in the domain module, would cause circular import between domain module and unification module
(require "typed-abstract-multi-domain.rkt")
(require "abstract-unify.rkt")
(require "data-utils.rkt")
(require "typed-abstract-substitution.rkt")
(require "abstract-substitution-functions.rkt")

(: >=-extension (-> AbstractDomainElem AbstractDomainElem Boolean))
(define (>=-extension domain-elem1 domain-elem2)
  (let ([unifier (abstract-unify (list (abstract-equality domain-elem1 domain-elem2)) 0)])
    (and (some? unifier) (equal? (apply-substitution (some-v unifier) domain-elem1) domain-elem2))))
(provide >=-extension)