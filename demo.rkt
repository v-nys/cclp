#lang racket
(require
  repeated-application
  cclp-analysis/interaction
  cclp-analysis/synthesis
  cclp-analysis/new-mi-map
  cclp-programs/queens
  positional-tree-utils
  positional-tree-utils/printer
  cclp-common/abstract-analysis
  (only-in graph graphviz))

; every result is an analysis
; (active-branch analysis) provides the active branch
; how do I go from active branch to genealogical graph?
; analysis->current-genealogical-graph
(apply↑* (λ (a) (begin (tree-display (analysis-tree a) print-tree-label) (displayln (graphviz (analysis->current-genealogical-graph a))) (proceed a))) initial-program-analysis)

;(define complete-analysis (apply↑* proceed initial-program-analysis))
;(visualize-partial-order complete-analysis)
;(displayln (graphviz (analysis-partial-order complete-analysis)))
;(define segments
;  (segments-for-postprocessing
;   ((compose sort-segments set->list synthesizable-segments analysis-tree) complete-analysis)
;   (analysis-tree complete-analysis)))
;(define endpoints (map (λ (l) (cons (label-index (first l)) (last l))) segments))
;(for-each
; displayln
; endpoints)
;
;(tree-display (analysis-tree complete-analysis) print-tree-label)
;(display-mi-map (analysis-tree complete-analysis))
