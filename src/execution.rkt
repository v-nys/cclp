#lang racket
(require "abstract-multi-domain.rkt")
(require "abstract-multi-domain-sexp-conversion.rkt")
(require "abstract-domain-ordering.rkt")
(require (only-in racket-list-utils/utils findf-index))
(require parenlog)

(define (is-valid? prior)
  (let ([counter-examples (query-model prior (violates_partial_order))])
    (< (length counter-examples) 1)))
(provide (contract-out [is-valid? (-> model? boolean?)]))

(define (selected-index conjunction prior)
  (let* ([sexp-conjunction (abstract-conjunction->sexp conjunction)] ; this has to happen at runtime...
         [query (list 'member_reaches_all_under_consistency 'X sexp-conjunction)]
         [outcomes (query-model-dynamic prior query)]) ; but this is expanded at compile time, when query is not bound
    (if (null? outcomes)
        (error "Partial order is underspecified.")
        (let ([sexp-renaming (hash-ref (car outcomes) 'X)])
          (findf-index (λ (atom) (renames? atom (sexp->abstract-atom sexp-renaming))) conjunction)))))
    
; contract could be more specific (range is from 0 to length of the list...), but can wait
(provide (contract-out [selected-index (-> (listof abstract-atom?) model? natural-number/c)]))

;(query-model-dynamic 'model-placeholder 'id-placeholder)