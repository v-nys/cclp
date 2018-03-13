#lang racket
(require
  repeated-application
  cclp-analysis/interaction
  cclp-analysis/synthesis
  cclp-analysis/new-mi-map
  cclp-programs/primes
  ;cclp-programs/permutation-sort
  positional-tree-utils/printer)
(define complete-analysis (apply↑* proceed initial-program-analysis))
(tree-display (analysis-tree complete-analysis) print-tree-label)
;(display-mi-map (analysis-tree complete-analysis))
