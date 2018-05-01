#lang racket
(require
  repeated-application
  cclp-analysis/interaction
  cclp-analysis/synthesis
  cclp-analysis/new-mi-map
  cclp-programs/graph-coloring
  positional-tree-utils
  positional-tree-utils/printer
  cclp-common/abstract-analysis)
(define complete-analysis (apply↑* proceed initial-program-analysis))
;(define segments
;  (segments-for-postprocessing
;   ((compose sort-segments set->list synthesizable-segments analysis-tree) complete-analysis)
;   (analysis-tree complete-analysis)))
;(define endpoints (map (λ (l) (cons (label-index (first l)) (last l))) segments))
;(for-each
; displayln
; endpoints)

(tree-display (analysis-tree complete-analysis) print-tree-label)
;(display-mi-map (analysis-tree complete-analysis))
