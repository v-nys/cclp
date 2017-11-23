#lang racket
(require pict cclp/clustering-structs cclp/gen-graph-structs cclp/genealogical-graph-visualization)
(define (cluster->pict cluster encoding->id id->conjunct)
  (define (rec c) (cluster->pict c encoding->id id->conjunct))
  (match cluster
    [(clustering (and (? gen-node?) gn) gcd)
     (gen-node->pict gn)]
    [(clustering subclusters gcd)
     (vr-append
      5
      (rectangle 0 5)
      (colorize
       (hc-append
        (id->pict
         (hash-ref
          encoding->id
          gcd))
        (abstract-conjunct->pict
         (hash-ref
          id->conjunct
          (hash-ref
           encoding->id
           gcd))))
       "orange")
      (frame
       (apply
        hc-append
        10
        (map rec (set->list subclusters)))))]))
(provide cluster->pict)