#lang racket
(require "abstract-multi-domain.rkt")
(require parenlog)

(define (is-valid? prior)
  (< (length (query-model prior #:limit 1 (violates_partial_order))) 1))
(provide (contract-out [is-valid? (-> model? boolean?)]))

(define (selected-index conjunction prior) 0)
; contract could be more specific (range is from 0 to length of the list...), but can wait
(provide (contract-out [selected-index (-> (listof abstract-atom?) model? natural-number/c)]))