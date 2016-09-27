#lang racket
(require "abstract-multi-domain.rkt")
(require "abstract-multi-domain-sexp-conversion.rkt")
(require "abstract-domain-ordering.rkt")
(require (only-in racket-list-utils/utils findf-index))
(require "fullai-domain.rkt")
(require parenlog)

(define (is-valid? prior)
  (let ([counter-examples (query-model prior (violates_partial_order))])
    (< (length counter-examples) 1)))
(provide (contract-out [is-valid? (-> model? boolean?)]))

(define (selected-index conjunction prior full-ai-rules)
  (define full-eval-index (foldl
                           (λ (r acc)
                             (if acc
                                 acc
                                 (findf-index (λ (atom) (>=-extension (full-ai-rule-input-pattern r) atom)) conjunction)))
                           #f
                           full-ai-rules))
  (if full-eval-index
      full-eval-index
      (let* ([sexp-conjunction (abstract-conjunction->sexp conjunction)]
             [query (list 'member_reaches_all_under_consistency 'X sexp-conjunction)]
             [outcomes (query-model-dynamic prior query)])
        (if (null? outcomes)
            (error "Partial order is underspecified.")
            (let ([sexp-renaming (hash-ref (car outcomes) 'X)])
              (findf-index (λ (atom) (renames? atom (sexp->abstract-atom sexp-renaming))) conjunction))))))

; contract could be more specific (range is from 0 to length of the list...), but can wait
(provide (contract-out [selected-index (-> (listof abstract-atom?) model? (listof full-ai-rule?)natural-number/c)]))