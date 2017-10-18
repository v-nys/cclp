#lang racket
(require pict cclp/gen-graph-structs cclp/genealogical-graph-visualization)
(define (cluster->pict cluster encoding->id id->conjunct)
  (define (rec c) (cluster->pict c encoding->id id->conjunct))
  (match cluster
    [(? gen-node?)
     (gen-node->pict cluster)]
    [(cons st gcd-encoding)
      (vr-append
      5
      (rectangle 0 5)
      (hc-append
       (text "gcd: ")
       (id->pict
        (hash-ref
         encoding->id
         gcd-encoding))
       (abstract-conjunct->pict
        (hash-ref
         id->conjunct
         (hash-ref
          encoding->id
          gcd-encoding))))
      (frame
       (apply
       hc-append
       10
       (map rec (set->list st)))))]))
(provide cluster->pict)