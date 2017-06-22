#lang racket
(require racket-tree-utils/src/tree)
(require "abstract-analysis.rkt")
(require (prefix-in ak: "abstract-knowledge.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))

(define (visit-from idx n)
  (match n
    [(node (tree-label _ _ _ (ck:rule _ _ rule-idx) idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,rule~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ (ak:full-evaluation _ _ rule-idx) idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,fullai~a)." idx idx2 rule-idx))]
    [(node (generalization _ _ _ _ _) _)
     (displayln (format "generalization_from_to()"))]
    [(node (cycle cycle-idx) _)
     (displayln (format "cycle_from_to(~a,~a)." idx cycle-idx))]
    [else
     (displayln "don't know how to be visited yet")]))

(define (visitor n)
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (for ([ch children]) (visit-from (label-index label) ch))]
    [(node (cycle idx) (list)) (void)]
    [_ (displayln (format "don't know how to visit ~a yet" n))]))

(define (display-mi-map tree)
  (visit visitor tree)
  tree)
(provide display-mi-map)