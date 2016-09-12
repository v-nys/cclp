#lang racket
(require "abstract-multi-domain.rkt")
(require "abstract-knowledge.rkt")
(require "data-utils.rkt")

(define (assemble-var-indices right-variable-type? abstract-data)
  (cond [(abstract-variable? abstract-data) (if (right-variable-type? abstract-data) (set (avar-index abstract-data)) (set))]
        [(abstract-atom? abstract-data) (apply optional-set-union (map (λ (arg) (assemble-var-indices right-variable-type? arg)) (abstract-atom-args abstract-data)))]
        [(abstract-function? abstract-data) (apply optional-set-union (map (λ (arg) (assemble-var-indices right-variable-type? arg)) (abstract-function-args abstract-data)))]
        [(list? abstract-data) (apply optional-set-union (map (λ (arg) (assemble-var-indices right-variable-type? arg)) abstract-data))]
        [(rule? abstract-data) (set-union (assemble-var-indices right-variable-type? (rule-head abstract-data)) (assemble-var-indices right-variable-type? (rule-body abstract-data)))]
        [(full-evaluation? abstract-data) (set-union (assemble-var-indices right-variable-type? (full-evaluation-input-pattern abstract-data)) (assemble-var-indices right-variable-type? (full-evaluation-input-pattern abstract-data)))]))
(provide assemble-var-indices)

(define (maximum-var-index abstraction right-variable-type?)
  (define max-of-args-accumulator
    (λ (el acc)
      (let ([subterm-max (maximum-var-index el right-variable-type?)])
        (cond [(none? acc) subterm-max]
              [(none? subterm-max) acc]
              [else (some (max (some-v acc) (some-v subterm-max)))]))))
  (cond [(abstract-variable? abstraction) (if (right-variable-type? abstraction) (some (avar-index abstraction)) (none))]
        [(abstract-function? abstraction) (foldl max-of-args-accumulator (none) (abstract-function-args abstraction))]
        [(abstract-atom? abstraction) (foldl max-of-args-accumulator (none) (abstract-atom-args abstraction))]
        [(list? abstraction) (foldl max-of-args-accumulator (none) abstraction)]
        [(rule? abstraction) (maximum-var-index (cons (rule-head abstraction) (rule-body abstraction)) right-variable-type?)]
        [(full-evaluation? abstraction) (maximum-var-index (list (full-evaluation-input-pattern abstraction) (full-evaluation-output-pattern abstraction)) right-variable-type?)]))
(provide maximum-var-index)