#lang at-exp racket
(require racket/generic)
(require graph)
(require "abstract-multi-domain.rkt" "abstract-renaming.rkt" "depth-k-abstraction.rkt" "cclp-interpreter.rkt")
(module+ test (require rackunit))

; how to deal with equivalence classes?
; graph elements are actually part of those, not just abstract atoms...
; TODO add proper normalization to abstract domain!!
(struct preprior-graph (prior)
  #:methods gen:graph
  [(define/generic component-has-vertex? has-vertex?)
   (define (has-vertex? g v)
     (component-has-vertex?
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define/generic component-has-edge? has-edge?)
   (define (has-edge? g u v)
     (component-has-edge?
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define (vertex=? g u v)
     (equal? (normalize-abstract-atom u) (normalize-abstract-atom v)))
   (define/generic component-add-vertex! add-vertex!)
   (define (add-vertex! g v)
     ; TODO add more specific and more general vertices, as well
     ; will need to make sure a max depth parameter is defined so that this can be done
     (component-add-vertex! (preprior-graph-prior g) (normalize-abstract-atom v)))
   (define/generic component-remove-vertex! remove-vertex!)
   (define (remove-vertex! g v)
     (component-remove-vertex!
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define/generic component-rename-vertex! rename-vertex!)
   (define (rename-vertex! g old new)
     (component-rename-vertex!
      (preprior-graph-prior)
      (normalize-abstract-atom old)
      (normalize-abstract-atom new)))
   (define/generic component-add-edge! add-edge!)
   (define (add-edge! g u v [weight 'default-value])
     (component-add-edge!
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define/generic component-add-directed-edge! add-directed-edge!)
   (define (add-directed-edge! g u v [weight 'default-value])
     (component-add-directed-edge!
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)
      weight))
   (define/generic component-remove-edge! remove-edge!)
   (define (remove-edge! g u v)
     (component-remove-edge!
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define/generic component-get-vertices get-vertices)
   (define (get-vertices g)
     (component-get-vertices (preprior-graph-prior g)))
   (define/generic component-in-vertices in-vertices)
   (define (in-vertices g)
     (component-in-vertices (preprior-graph-prior g)))
   (define/generic component-get-neighbors get-neighbors)
   (define (get-neighbors g v)
     (component-get-neighbors
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define/generic component-in-neighbors in-neighbors)
   (define (in-neighbors g v)
     (component-in-neighbors
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define/generic component-get-edges get-edges)
   (define (get-edges g)
     (component-get-edges (preprior-graph-prior g)))
   (define/generic component-in-edges in-edges)
   (define (in-edges g)
     (component-in-edges (preprior-graph-prior g)))
   (define/generic component-edge-weight edge-weight)
   (define (edge-weight g u v)
     (component-edge-weight
      (preprior-graph-prior g)
      (normalize-abstract-atom u)
      (normalize-abstract-atom v)))
   (define/generic component-transpose transpose)
   (define (transpose g)
     (preprior-graph (component-transpose (preprior-graph-prior g))))
   (define/generic component-graph-copy graph-copy)
   (define (graph-copy g)
     (preprior-graph (component-graph-copy (preprior-graph-prior g))))])
(define (mk-preprior-graph) (preprior-graph (unweighted-graph/directed '())))
(provide mk-preprior-graph)

(module+ test
  (parameterize
      ([max-depth 2]
       [domain-symbols
        (list
         (abstract-atom 'foo '()) ; this is foo/0
         (abstract-atom 'bar '())
         (abstract-atom 'quux '())
         (abstract-atom 'foo (list (a 1))) ; this is foo/1
         (abstract-function 'cons (list (a 1) (a 2)))
         (abstract-function 'nil '()))])
    (let ([g (mk-preprior-graph)]
          [u (abstract-atom 'foo (list))]
          [v (abstract-atom 'bar (list))])
      (begin (add-directed-edge! g u v)
             (check-true (hash-ref (transitive-closure g) (list u v) #f))))
    (let ([g (mk-preprior-graph)]
          [u (abstract-atom 'foo (list))]
          [v (abstract-atom 'bar (list))]
          [w (abstract-atom 'quux (list))])
      (begin (add-directed-edge! g u v)
             (add-directed-edge! g v w)
             (check-true (hash-ref (transitive-closure g) (list u w) #f))))
    (let ([g (mk-preprior-graph)]
          [u (abstract-atom 'foo (list))]
          [v (abstract-atom 'bar (list))]
          [w (abstract-atom 'quux (list))])
      (begin (add-directed-edge! g u v)
             (add-directed-edge! g v w)
             (check-true (hash-ref (transitive-closure g) (list u w) #f))))
    (let ([g (mk-preprior-graph)]
          [u (interpret-abstract-atom "foo(nil)")])
      (begin (add-vertex! g u)
             (check-true (has-edge? g u u))))
    (let ([g (mk-preprior-graph)]
          [u (interpret-abstract-atom "foo(nil)")]
          [v (interpret-abstract-atom "foo(γ1)")])
      (begin (add-vertex! g u)
             (check-true (has-edge? g u v))))
    (let ([g (mk-preprior-graph)]
          [u (interpret-abstract-atom "foo(nil)")]
          [v (interpret-abstract-atom "foo(γ1)")]
          [w (interpret-abstract-atom "foo(α1)")])
      (begin (add-vertex! g v)
             (check-true (has-edge? g u v))
             (check-true (has-edge? g v w))))))