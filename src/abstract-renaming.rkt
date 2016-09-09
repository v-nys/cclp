#lang typed/racket
(require "typed-abstract-multi-domain.rkt" (prefix-in ak: "typed-abstract-knowledge.rkt") "data-utils.rkt" "typed-abstract-substitution.rkt" "abstract-substitution-functions.rkt" "abstraction-inspection-utils.rkt")
(: opt-max (-> (Opt Integer) (Opt Integer) Integer Integer))
(define (opt-max opt1 opt2 default)
  (cond [(and (some? opt1) (some? opt2)) (max (some-v opt1) (some-v opt2))]
        [(some? opt1) (some-v opt1)]
        [(some? opt2) (some-v opt2)]
        [else default]))

(: rename-apart (-> ak:AbstractKnowledge AbstractConjunction ak:AbstractKnowledge))
(define (rename-apart knowledge conjunction)
  (let* ([g-max-conjunction (maximum-var-index conjunction g?)]
         [a-max-conjunction (maximum-var-index conjunction a?)]
         [g-max-knowledge (maximum-var-index knowledge g?)]
         [a-max-knowledge (maximum-var-index knowledge a?)]
         [g-offset (opt-max g-max-conjunction g-max-knowledge 0)]
         [a-offset (opt-max a-max-conjunction a-max-knowledge 0)]
         [a-indices (assemble-var-indices a? knowledge)]
         [g-indices (assemble-var-indices g? knowledge)]
         [subst (append (map (λ ([index : Integer]) (abstract-equality (a index) (a (+ a-offset index)))) (set->list a-indices)) (map (λ ([index : Integer]) (abstract-equality (g index) (g (+ g-offset index)))) (set->list g-indices)))])
  (apply-substitution-to-knowledge subst knowledge)))
(provide rename-apart)