#lang at-exp racket
(require graph)
(require "abstract-multi-domain.rkt" "abstract-renaming.rkt" "depth-k-abstraction.rkt")
(module+ test (require rackunit))

; how to deal with equivalence classes?
; graph elements are actually part of those, not just abstract atoms...
; TODO add proper normalization to abstract domain!!
(struct preprior-graph (prior)
  #:methods gen:graph
  [(define (has-vertex? g v)
     (has-vertex?
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define (has-edge? g u v)
     (has-edge?
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define (vertex=? g u v)
     (equal? (normalize-abstract-atom u) (normalize-abstract-atom v)))
   (define (add-vertex! g v)
     ; TODO add more specific and more general vertices, as well
     ; will need to make sure a max depth parameter is defined so that this can be done
     (add-vertex! (preprior-graph-prior g) (normalize-abstract-atom v)))
   (define (remove-vertex! g v)
     (remove-vertex!
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define (rename-vertex! g old new)
     (rename-vertex!
      (preprior-graph-prior)
      (normalize-abstract-atom old)
      (normalize-abstract-atom new)))
   (define (add-edge! g u v [weight 'default-value])
     (add-edge!
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define (add-directed-edge! g u v [weight 'default-value])
     (add-directed-edge!
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define (remove-edge! g u v)
     (remove-edge!
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define (get-vertices g)
     (get-vertices (preprior-graph-prior g)))
   (define (in-vertices g)
     (in-vertices (preprior-graph-prior g)))
   (define (get-neighbors g v)
     (get-neighbors
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define (in-neighbors g v)
     (in-neighbors
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define (get-edges g)
     (get-edges (preprior-graph-prior g)))
   (define (in-edges g)
     (get-edges (preprior-graph-prior g)))
   (define (edge-weight g u v)
     (edge-weight
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define (transpose g)
     (preprior-graph (transpose (preprior-graph-prior g))))
   (define (graph-copy g)
     (preprior-graph (graph-copy (preprior-graph-prior g))))])
(define (mk-preprior-graph) (preprior-graph (unweighted-graph/directed '())))
(provide mk-preprior-graph)

(module+ test
  (parameterize
      ([max-depth 2]
       [concrete-constants (list (abstract-function 'nil '()))])
    (void)))