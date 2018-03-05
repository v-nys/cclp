#lang racket
(require
  repeated-application
  cclp-analysis/interaction
  cclp-analysis/synthesis
  cclp-analysis/new-mi-map
  cclp-programs/primes)
(define complete-analysis (apply↑* proceed initial-program-analysis))
;(define segments ((compose sort-segments set->list synthesizable-segments analysis-tree) complete-analysis))
(display-mi-map (analysis-tree complete-analysis))