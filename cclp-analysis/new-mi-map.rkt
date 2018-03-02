#lang at-exp racket

(require scribble/srcdoc
         (for-doc scribble/manual)
         positional-tree-utils
         cclp-common/abstract-analysis
         (prefix-in ak: cclp-common-data/abstract-knowledge)
         (prefix-in ck: cclp-common-data/concrete-knowledge)
         cclp-common/data-utils)

(define (mi-map-visit-from idx n)
  (match n
    ;i ending up with empty goal
    [(node (tree-label (list) _ _ (ck:rule _ _ rule-idx) #f) _)
     (displayln (format "state_transition(~a,empty,rule~a)." idx rule-idx))]
    [(node (tree-label (list) _ _ (ak:full-evaluation _ _ rule-idx) #f) _)
     (displayln (format "state_transition(~a,empty,fullai~a)." idx rule-idx))]
    ;; ending up with nonempty goal, which cycles back
    [(node (tree-label _ _ _ (ck:rule _ _ rule-idx) _) (list (node (cycle cycle-idx) _)))
     (displayln (format "state_transition(~a,~a,rule~a)." idx cycle-idx rule-idx))]
    [(node (tree-label _ _ _ (ak:full-evaluation _ _ rule-idx) _) (list (node (cycle cycle-idx) _)))
     (displayln (format "state_transition(~a,~a,fullai~a)." idx cycle-idx rule-idx))]
    ;; ending up with nonempty goal
    [(node (tree-label _ _ _ (ck:rule _ _ rule-idx) idx2) _)
     (displayln (format "state_transition(~a,~a,rule~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ (ak:full-evaluation _ _ rule-idx) idx2) _)
     (displayln (format "state_transition(~a,~a,fullai~a)." idx idx2 rule-idx))]
    ;; multi unfolding
    [(node (tree-label _ _ _ 'one idx2) _)
     (displayln (format "state_transition(~a,~a,one)." idx idx2))]
    [(node (tree-label _ _ _ 'many idx2) _)
     (displayln (format "state_transition(~a,~a,many)." idx idx2))]
    ;; generalization
    ;; TODO: need the correct ranges
    [(node (generalization _ _ idx2 _ _) _)
     (displayln (format "grouping(~a,~a,ListOfRanges)" idx idx2))]
    [(node (cycle _) _)
     (void)]
    [else
     (displayln (format "don't know how to be visited yet"))]))

(define (display-selected-index l)
  (when (some? (label-selection l))
    (displayln
     (format
      "selected_index(~a,~a)."
      (label-index l)
      (some-v (label-selection l))))))

(define (mi-map-visitor n)
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (display-selected-index label)
     (for ([ch children])
       (mi-map-visit-from (label-index label) ch))]
    [(node (cycle idx) (list)) (void)]))

(define (display-mi-map tree)
  (begin
    (visit mi-map-visitor tree)
    tree))
(provide
 (proc-doc/names
  display-mi-map
  (->
   node?
   node?)
  (tree)
  @{Summarizes the transitions between nodes in an abstract tree @racket[tree] in terms of the node numbers involved and knowledge applied, as well as the atom selections and groupings performed in states.}))