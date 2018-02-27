#lang racket
(require cclp-common-data/abstract-multi-domain)

;; note: mapping distinguishes between a* id 'i j and a* id 1 j and a* id 'L j!
(define (remove-multi-subscripts abs [mapping #f])
  (define aux (λ (e) (remove-multi-subscripts e mapping)))
  (match abs
    [(? list?)
     (map aux abs)]
    [(abstract-atom* sym args)
     (abstract-atom sym (map aux args))]
    [(abstract-function* sym args)
     (abstract-function sym (map aux args))]
    [(g* _ _ local-idx)
     (if mapping
         (hash-ref mapping abs)
         (g local-idx))]
    [(a* _ _ local-idx)
     (if mapping
         (hash-ref mapping abs)
         (a local-idx))]))
(provide remove-multi-subscripts)