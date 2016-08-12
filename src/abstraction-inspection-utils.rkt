#lang typed/racket
(require "abstract-multi-domain.rkt")
(require "abstract-knowledge.rkt")
(require "data-utils.rkt")

(: assemble-var-indices (-> (-> AbstractVariable Boolean) (U AbstractDomainElem AbstractKnowledge) (Setof Integer)))
(define (assemble-var-indices right-variable-type? abstract-data)
  (cond [(AbstractVariable? abstract-data) (if (right-variable-type? abstract-data) (set (avar-index abstract-data)) (set))]
        [(abstract-atom? abstract-data) (apply optional-set-union (map (λ ([arg : AbstractTerm]) (assemble-var-indices right-variable-type? arg)) (abstract-atom-args abstract-data)))]
        [(abstract-function? abstract-data) (apply optional-set-union (map (λ ([arg : AbstractTerm]) (assemble-var-indices right-variable-type? arg)) (abstract-function-args abstract-data)))]
        [(AbstractConjunction? abstract-data) (apply optional-set-union (map (λ ([arg : abstract-atom]) (assemble-var-indices right-variable-type? arg)) abstract-data))]
        [(rule? abstract-data) (set-union (assemble-var-indices right-variable-type? (rule-head abstract-data)) (assemble-var-indices right-variable-type? (rule-body abstract-data)))]
        [(full-evaluation? abstract-data) (set-union (assemble-var-indices right-variable-type? (full-evaluation-input-pattern abstract-data)) (assemble-var-indices right-variable-type? (full-evaluation-input-pattern abstract-data)))]))
(provide assemble-var-indices)

(: maximum-var-index (-> (U AbstractDomainElem AbstractKnowledge) (-> AbstractVariable Boolean) (Opt Integer)))
(define (maximum-var-index abstraction right-variable-type?)
  (define max-of-args-accumulator (λ ([el : AbstractDomainElem] [acc : (Opt Integer)])
                                                   (let ([subterm-max (maximum-var-index el right-variable-type?)])
                                                     (cond [(none? acc) subterm-max]
                                                           [(none? subterm-max) acc]
                                                           [else (some (max (some-v acc) (some-v subterm-max)))]))))
  (cond [(AbstractVariable? abstraction) (if (right-variable-type? abstraction) (some (avar-index abstraction)) (none))]
        [(abstract-function? abstraction) (foldl max-of-args-accumulator (none) (abstract-function-args abstraction))]
        [(abstract-atom? abstraction) (foldl max-of-args-accumulator (none) (abstract-atom-args abstraction))]
        [(AbstractConjunction? abstraction) (foldl max-of-args-accumulator (none) abstraction)]
        [(rule? abstraction) (maximum-var-index (cons (rule-head abstraction) (rule-body abstraction)) right-variable-type?)]
        [(full-evaluation? abstraction) (maximum-var-index (list (full-evaluation-input-pattern abstraction) (full-evaluation-output-pattern abstraction)) right-variable-type?)]))
(provide maximum-var-index)