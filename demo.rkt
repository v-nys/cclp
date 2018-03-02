#lang racket
(require
  repeated-application
  cclp-analysis/interaction
  cclp-analysis/synthesis
  cclp-programs/permutation-sort)
(define complete-analysis (apply↑* proceed initial-program-analysis))
(define segments ((compose sort-segments set->list synthesizable-segments analysis-tree) complete-analysis))