#lang typed/racket
(require "abstract-multi-domain.rkt")

(struct abstract-equality ([term1 : AbstractDomainElem] [term2 : AbstractDomainElem]) #:transparent)
(provide (struct-out abstract-equality))
(define-type AbstractSubstitution (Listof abstract-equality))
(provide AbstractSubstitution)

(require "data-utils.rkt") ; for Opt

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

(: maximum-var-index-in-substitution (-> (-> AbstractVariable Boolean) AbstractSubstitution (Opt Integer)))
(define (maximum-var-index-in-substitution right-variable-type? substitution)
  (foldl (λ ([eq : abstract-equality] [acc : (Opt Integer)])
           (let ([max-aeq (maximum-var-index-in-equality right-variable-type? eq)])
             (cond [(none? acc) max-aeq]
                   [(none? max-aeq) acc]
                   [else (some (max (some-v acc) (some-v max-aeq)))]))) (none) substitution))

(: maximum-var-index-in-equality (-> (-> AbstractVariable Boolean) abstract-equality (Opt Integer)))
(define (maximum-var-index-in-equality right-variable-type? aeq)
  (let ([max-lhs (maximum-var-index (abstract-equality-term1 aeq) right-variable-type?)]
        [max-rhs (maximum-var-index (abstract-equality-term2 aeq) right-variable-type?)])
    (cond [(none? max-lhs) max-rhs]
          [(none? max-rhs) max-lhs]
          [else (some (max (some-v max-lhs) (some-v max-rhs)))])))

(: maximum-var-index (-> AbstractDomainElem (-> AbstractVariable Boolean) (Opt Integer)))
(define (maximum-var-index domain-elem right-variable-type?)
  (define max-of-args-accumulator (λ ([el : AbstractDomainElem] [acc : (Opt Integer)])
                                                   (let ([subterm-max (maximum-var-index el right-variable-type?)])
                                                     (cond [(none? acc) subterm-max]
                                                           [(none? subterm-max) acc]
                                                           [else (some (max (some-v acc) (some-v subterm-max)))]))))
  (cond [(AbstractVariable? domain-elem) (if (right-variable-type? domain-elem) (some (avar-index domain-elem)) (none))]
        [(abstract-function? domain-elem) (foldl max-of-args-accumulator (none) (abstract-function-args domain-elem))]
        [(abstract-atom? domain-elem) (foldl max-of-args-accumulator (none) (abstract-atom-args domain-elem))]
        [(AbstractConjunction? domain-elem) (foldl max-of-args-accumulator (none) domain-elem)]))
(provide maximum-var-index)