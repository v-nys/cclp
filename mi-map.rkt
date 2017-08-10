#lang racket
(require racket-tree-utils/src/tree)
(require "abstract-analysis.rkt")
(require (prefix-in ak: "abstract-knowledge.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))

(define (mi-map-visit-from idx n)
  (match n
    [(node (tree-label (list) _ _ (ck:rule _ _ rule-idx) #f _) _)
     (displayln (format "transition_from_to_via(~a,empty,rule~a)." idx rule-idx))]
    [(node (tree-label (list) _ _ (ak:full-evaluation _ _ rule-idx) #f _) _)
     (displayln (format "transition_from_to_via(~a,empty,fullai~a)." idx rule-idx))]
    [(node (tree-label _ _ _ (ck:rule _ _ rule-idx) idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,rule~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ (ak:full-evaluation _ _ rule-idx) idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,fullai~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ 'one idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,one)." idx idx2))]
    [(node (tree-label _ _ _ 'many idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,many)." idx idx2))]
    [(node (generalization _ _ idx2 _ _) _)
     (displayln (format "generalization_from_to(~a,~a)" idx idx2))]
    [(node (cycle cycle-idx) _)
     (displayln (format "cycle_from_to(~a,~a)." idx cycle-idx))]
    [else
     (displayln (format "don't know how to be visited yet"))]))

(define (mi-map-visitor n)
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (for ([ch children])
       (mi-map-visit-from (label-index label) ch))]
    [(node (cycle idx) (list)) (void)]
    [_ (displayln (format "don't know how to visit ~a yet" n))]))

(define (display-mi-map tree)
  (visit mi-map-visitor tree)
  tree)
(provide display-mi-map)



(module+ test
  (require rackunit)
  (check-equal?
   (untangle
    (interpret-abstract-conjunction "integers(γ1,α1),filter(γ2,α1,α2),filter(γ3,α2,α3),filter(γ4,α3,α4),sift(α4,α5),length(α5,γ5)"))
   (cons
     (interpret-abstract-conjunction "integers(γ1,α1),filter(γ2,α6,α2),filter(γ3,α7,α3),filter(γ4,α8,α4),sift(α9,α5),length(α10,γ5)")
     (list ((a 1) . (a 6)) ((a 2) . (a 7)) ((a 3) . (a 8)) ((a 4) . (a 9)) ((a 5) . (a 10)))))
  (check-equal?
   (untangle
    (list
     (abstract-atom 'integers (list (g 1) (a 1)))
     (abstract-atom 'filter (list (g 2) (a 1) (a 2)))
     (multi
      (list
       (abstract-atom*
        'filter
        (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init (list (cons (a* 1 1 1) (a 2))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final (list (cons (a* 1 'L 2) (a 3)))))
     (abstract-atom 'filter (list (g 3) (a 3) (a 4)))
     (abstract-atom 'sift (list (a 4) (a 5)))
     (abstract-atom 'length (list (a 5) (g 4)))))
   (cons
     (list
      (abstract-atom 'integers (list (g 1) (a 1)))
      (abstract-atom 'filter (list (g 2) (a 6) (a 2)))
      (multi
      (list
       (abstract-atom*
        'filter
        (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init (list (cons (a* 1 1 1) (a 7))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final (list (cons (a* 1 'L 2) (a 3)))))
      (abstract-atom 'filter (list (g 3) (a 8) (a 4)))
      (abstract-atom 'sift (list (a 9) (a 5)))
      (abstract-atom 'length (list (a 10) (g 4))))
     (list ((a 1) . (a 6)) ((a 2) . (a 7)) ((a 3) . (a 8)) ((a 4) . (a 9)) ((a 5) . (a 10)))))
  (check-equal?
   (untangle
    (list
     (abstract-atom 'collect (list (g 1) (a 1)))
     (multi
      (list
       (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
       (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
      (init (list (cons (a* 1 1 2) (a 1))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 3))))
      (final (list (cons (a* 1 'L 3) (a 2)))))
     (abstract-atom 'collect (list (g 2) (a 3)))
     (abstract-atom 'eq (list (a 2) (a 3))))
   (cons
    (list
     (abstract-atom 'collect (list (g 1) (a 1)))
     (multi
      (list
       (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
       (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
      (init (list (cons (a* 1 1 2) (a 4))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 3))))
      (final (list (cons (a* 1 'L 3) (a 2)))))
     (abstract-atom 'collect (list (g 2) (a 3)))
     (abstract-atom 'eq (list (a 5) (a 6))))
    (list ((a 1) . (a 4)) ((a 2) . (a 5)) ((a 3) . (a 6)))))
  ;; TODO: needs more tests for the annoying scenarios, e.g. multiple multis, multi like in graph coloring,...
  ))

(define (generate-generalization-clause x y)
  (display "not implemented yet"))

(define (generalization-clause-visit-from conjunction1 n)
  (match n
    [(node (generalization conjunction2 _ _ _ _) _)
     (display
      (generate-generalization-clause
       conjunction1
       conjunction2))]
    [_ (void)]))

(define (generalization-clause-visitor n)
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (for ([ch children])
       (generalization-clause-visit-from (label-conjunction label) ch))]
    [(node (cycle idx) (list)) (void)]
    [_ (displayln (format "don't know how to visit ~a yet" n))]))

(define (display-generalization-clauses tree)
  (visit generalization-clause-visitor tree)
  tree)
(provide display-generalization-clauses)