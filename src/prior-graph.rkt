#lang at-exp racket
(require racket/generic)
(require graph)
(require "abstract-multi-domain.rkt" "abstract-renaming.rkt" "cclp-interpreter.rkt" "abstract-domain-ordering.rkt")
(module+ test (require rackunit))

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
     (component-add-vertex!
      (preprior-graph-prior g)
      (normalize-abstract-atom v))
     (for ([xv (in-component-vertices (preprior-graph-prior g))]
           #:when (>=-extension v xv)
           #:unless (renames? xv v))
       (component-add-directed-edge! (preprior-graph-prior g) xv v))
     (for ([xv (in-component-vertices (preprior-graph-prior g))]
           #:when (>=-extension xv v)
           #:unless (renames? xv v))
       (component-add-directed-edge! (preprior-graph-prior g) v xv)))
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
   (define/generic in-component-vertices in-vertices)
   (define (in-vertices g)
     (in-component-vertices (preprior-graph-prior g)))
   (define/generic component-get-neighbors get-neighbors)
   (define (get-neighbors g v)
     (component-get-neighbors
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define/generic in-component-neighbors in-neighbors)
   (define (in-neighbors g v)
     (in-component-neighbors
      (preprior-graph-prior g)
      (normalize-abstract-atom v)))
   (define/generic component-get-edges get-edges)
   (define (get-edges g)
     (component-get-edges (preprior-graph-prior g)))
   (define/generic in-component-edges in-edges)
   (define (in-edges g)
     (in-component-edges (preprior-graph-prior g)))
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
           (check-false (has-edge? g u u))))
  (let ([g (mk-preprior-graph)]
        [u (interpret-abstract-atom "foo(nil)")]
        [v (interpret-abstract-atom "foo(γ1)")])
    (begin (add-vertex! g u)
           (add-vertex! g v)
           (check-true (has-edge? g u v))))
  (let ([g (mk-preprior-graph)]
        [u (interpret-abstract-atom "foo(nil)")]
        [v (interpret-abstract-atom "foo(γ1)")])
    (begin (add-vertex! g v)
           (add-vertex! g u)
           (check-true (has-edge? g u v)))))

(define (strict-partial-order? g)
  (define transitive-g (transitive-closure g))
  (not (ormap (λ (v) (hash-ref transitive-g (list v v) #f)) (get-vertices g))))
(module+ test
  (check-true (strict-partial-order? (mk-preprior-graph)))
  (let ([g (mk-preprior-graph)]
        [v (abstract-atom 'a '())])
    (begin
      (add-vertex! g v)
      (add-directed-edge! g v v)
      (check-false (strict-partial-order? g))))
  (let ([g (mk-preprior-graph)]
        [u (abstract-atom 'a '())]
        [v (abstract-atom 'b '())])
    (begin
      (add-vertex! g u)
      (add-vertex! g v)
      (add-directed-edge! g u v)
      (add-directed-edge! g v u)
      (check-false (strict-partial-order? g))))
  (let ([g (mk-preprior-graph)]
        [u (abstract-atom 'a '())]
        [v (abstract-atom 'b '())]
        [w (abstract-atom 'c '())])
    (begin
      (add-vertex! g u)
      (add-vertex! g v)
      (add-vertex! g w)
      (add-directed-edge! g u v)
      (add-directed-edge! g v w)
      (check-true (strict-partial-order? g))))
  (let ([g (mk-preprior-graph)]
        [u (abstract-atom 'a '())]
        [v (abstract-atom 'b '())]
        [w (abstract-atom 'c '())])
    (begin
      (add-vertex! g u)
      (add-vertex! g v)
      (add-vertex! g w)
      (add-directed-edge! g u v)
      (add-directed-edge! g v w)
      (add-directed-edge! g w u)
      (check-false (strict-partial-order? g)))))