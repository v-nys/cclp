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
         [query (list 'reaches_all_under_consistency 'X sexp-conjunction)]
         [outcomes (query-model-dynamic prior query)])
    (if (null? outcomes)
        (error "Partial order is underspecified.")
        (let ([sexp-renaming (hash-ref (car outcomes) 'X)])
          (findf-index (λ (atom) (renames? atom (sexp->abstract-atom sexp-renaming))) conjunction)))))
    
; contract could be more specific (range is from 0 to length of the list...), but can wait
(provide (contract-out [selected-index (-> (listof abstract-atom?) model? natural-number/c)]))

; we need this:
; (query-model model-id query-id) starts a search with the literal query 'query-id
; this is annoying if we have a complex query which is generated programmatically
;(define-syntax-rule (query-model-dynamic model query) )
(define-syntax (query-model-dynamic stx)
  (syntax-case stx ()
    [(_ prior-stx id-stx)
     (with-syntax ([raw-value-syntax (datum->syntax #'id-stx (eval-syntax #'id-stx))])
       ; dit is het nog niet helemaal: hoe splice ik prior-stx en raw-value-syntax in deze nieuwe syntax?
       (datum->syntax stx (list 'query-model #'prior-stx #'raw-value-syntax)))]))