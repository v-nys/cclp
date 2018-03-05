#lang at-exp racket

(require scribble/srcdoc
         (for-doc scribble/manual)
         positional-tree-utils
         cclp-common/abstract-analysis
         (prefix-in ak: cclp-common-data/abstract-knowledge)
         (prefix-in ck: cclp-common-data/concrete-knowledge)
         cclp-common/gen-graph-structs ; for index-range, TODO this could actually go somewhere else
         cclp-common/data-utils)

(define (groupings->bb-ranges groupings)
  ;; there are two list levels in groupings
  ;; outermost because several multis can appear during generalization
  ;; innermost because a single multi is made up of N grouped building blocks
  (define same-multi-groupings (group-by cdr groupings))
  ;; group-by adds another list level, which was implicit in the exact-nonnegative-integer? above
  ;; an application of (λ (g) ...) produces a (listof (listof index-range))
  (define (inner-sort index-ranges)
    (sort
     index-ranges
     (λ (rng1 rng2)
       (< (index-range-start rng1)
          (index-range-start rng2)))))
  (map
   (λ (g)
     (flatten
      (sort
       (map (compose inner-sort car) g)
       (λ (ll1 ll2) (< (index-range-start (first (first ll1))) (index-range-start (first (first ll2))))))))
   same-multi-groupings))

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
    [(node (generalization _ _ idx2 _ groupings) _)
     (let* ([ranges (groupings->bb-ranges groupings)] ; gives a list of list of ranges
            [range->str (λ (r) (format "(~a,~a)" (index-range-start r) (index-range-end-before r)))]
            [l-of-rng->str (λ (lr) (string-join (map range->str lr) "," #:before-first "[" #:after-last "]"))]
            [l-of-l-of-rng->str (λ (llr) (string-join (map l-of-rng->str llr) "," #:before-first "[" #:after-last "]"))]
            [displayed-ranges (l-of-l-of-rng->str ranges)])
       (displayln (format "grouping(~a,~a,~a)" idx idx2 displayed-ranges)))]
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