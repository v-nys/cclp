#lang racket
(require "abstract-multi-domain.rkt"
         (prefix-in ak: "abstract-knowledge.rkt")
         "data-utils.rkt"
         "abstract-substitution.rkt"
         "abstraction-inspection-utils.rkt")
;(: opt-max (-> (Opt Integer) (Opt Integer) Integer Integer))
(define (opt-max opt1 opt2 default)
  (cond [(and (some? opt1) (some? opt2)) (max (some-v opt1) (some-v opt2))]
        [(some? opt1) (some-v opt1)]
        [(some? opt2) (some-v opt2)]
        [else default]))

;(: rename-apart (-> ak:AbstractKnowledge AbstractConjunction ak:AbstractKnowledge))
; modified so that it works for all abstract domain elements
(define (rename-apart renamee non-renamee)
  (let* ([g-max-non-renamee (maximum-var-index non-renamee g?)]
         [a-max-non-renamee (maximum-var-index non-renamee a?)]
         [g-max-renamee (maximum-var-index renamee g?)]
         [a-max-renamee (maximum-var-index renamee a?)]
         [g-offset (opt-max g-max-non-renamee g-max-renamee 0)]
         [a-offset (opt-max a-max-non-renamee a-max-renamee 0)]
         [a-indices (assemble-var-indices a? renamee)]
         [g-indices (assemble-var-indices g? renamee)]
         [subst (append (map (λ (index) (abstract-equality (a index) (a (+ a-offset index)))) (set->list a-indices))
                        (map (λ (index) (abstract-equality (g index) (g (+ g-offset index)))) (set->list g-indices)))])
  (apply-substitution subst renamee)))
(provide rename-apart)