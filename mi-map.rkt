#lang racket
(require racket-tree-utils/src/tree)
(require "abstract-analysis.rkt")

(define (visit-from idx n)
  (match n
    [(node (tree-label _ _ _ rule idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,~a)." idx idx2 rule))]
    [(node (generalization _ _ _ _ _) _)
     (displayln (format "generalization_from_to()"))]
    [(node (cycle cycle-idx) _)
     (displayln (format "cycle_from_to(~a,~a)." idx cycle-idx))]
    [else
     (displayln "don't know how to be visited yet")]))

(define (visitor n)
  ;; TODO: match is not a great fit here
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (for ([ch children]) (visit-from (label-index label) ch))]
    [_ (displayln "don't know how to visit yet")]))

(define (display-mi-map tree)
  (visit visitor tree))
(provide display-mi-map)