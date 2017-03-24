#lang racket
(require "abstract-multi-domain.rkt")

(define (remove-multi-subscripts abs [mapping #f])
  (define aux (λ (e) (remove-multi-subscripts e mapping)))
  (match abs
    [(? list?)
     (map aux abs)]
    [(abstract-atom* sym args)
     (abstract-atom sym (map aux args))]
    [(abstract-function* sym args)
     (abstract-function sym (map aux args))]
    [(g* id 'i local-idx)
     (if mapping
         (hash-ref mapping abs)
         (g local-idx))]
    [(a* _ 'i local-idx)
     (if mapping
         (hash-ref mapping abs)
         (a local-idx))]))
(provide remove-multi-subscripts)