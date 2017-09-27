#lang at-exp racket
(require racket/generic)
(require graph)
(require "abstract-multi-domain.rkt" "abstract-renaming.rkt" "cclp-interpreter.rkt" "abstract-domain-ordering.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/manual))
(module+ test (require rackunit))
(require racket/serialize)

(define (adj-list g)
  (map
   (λ (v)
     (cons
      v
      (map
       second
       (filter
        (λ (e)
          (equal? (first e) v))
        (get-edges g)))))
   (get-vertices g)))

(define ds-info
  (make-deserialize-info
   (λ (edges)
     (preprior-graph
      (unweighted-graph/adj edges)))
   (λ ()
     (values
      (error "should not be required")
      (error "should not be required")))))
(provide ds-info)

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
     (define normalized-v (normalize-abstract-atom v))
     (define existing-vs (component-get-vertices (preprior-graph-prior g)))
     (when (not (member normalized-v existing-vs))
       (component-add-vertex!
        (preprior-graph-prior g)
        normalized-v)
       (define reaching
         (filter
          (λ (xv) (and (>=-extension normalized-v xv) (not (renames? xv normalized-v))))
          existing-vs))
       (define reached
         (filter
          (λ (xv) (and (>=-extension xv normalized-v) (not (renames? xv normalized-v))))
          existing-vs))
       (for ([r reaching]) (component-add-directed-edge! (preprior-graph-prior g) r normalized-v))
       (for ([r reached]) (component-add-directed-edge! (preprior-graph-prior g) normalized-v r))
       (when (not (strict-partial-order? g))
         (component-remove-vertex! (preprior-graph-prior g) v)
         (error (format "Adding vertex ~a would break partial ordering" normalized-v)))))
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
   (define/generic component-remove-directed-edge! remove-directed-edge!)
   (define (add-directed-edge! g u v [weight 'default-value])
     (define n-u (normalize-abstract-atom u))
     (define n-v (normalize-abstract-atom v))
     (component-add-directed-edge!
      (preprior-graph-prior g)
      n-u
      n-v
      weight)
     (when (not (strict-partial-order? g))
       (component-remove-directed-edge!
        (preprior-graph-prior g)
        n-u
        n-v)
       (error (format "Adding edge from ~a to ~a would break partial ordering" n-u n-v))))
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
     (preprior-graph (component-graph-copy (preprior-graph-prior g))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur)
     (equal?-recur (preprior-graph-prior g1) (preprior-graph-prior g2)))
   (define (hash-proc g hash-recur)
     (hash-recur (preprior-graph-prior g)))
   (define (hash2-proc g hash2-recur)
     (hash2-recur (preprior-graph-prior g)))]
  #:property
  prop:serializable
  (make-serialize-info
   (λ (s) (make-vector 1 (adj-list (preprior-graph-prior s))))
   ; FIXME assumes fixed directory structure :-S
   ; may be fixed simply by using cclp/...
   (cons 'ds-info (module-path-index-join "../cclp/preprior-graph.rkt" #f))
   #f
   (or (current-load-relative-directory) (current-directory))))
(define (mk-preprior-graph) (preprior-graph (unweighted-graph/directed '())))
(provide
 (proc-doc/names
  mk-preprior-graph
  (-> preprior-graph?) ()
  @{Creates a mutable graph suitable for representing a strict partial order, without any vertices.}))
(provide
 (proc-doc/names
  preprior-graph?
  (-> any/c boolean?)
  (val)
  @{Checks whether @racket[val] is a mutable graph suitable for representing a strict partial order.}))

(module+ test
  (test-case
   "transitive closure with extra instantiation constraints works as expected"
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
            (check-true (has-edge? g u v))))))

(define (strict-partial-order? g)
  ; note: transitive closure always returns #t for self loops
  ; (see racket/graph source code)
  ; therefore, check for self *edges* or non-self loops
  (define transitive-g (transitive-closure g))
  (not
   (ormap
    (λ (v)
      (or (has-edge? g v v)
          (ormap
           (λ (v2)
             (and
              (not (vertex=? g v v2))
              (hash-ref transitive-g (list v v2) #f)
              (hash-ref transitive-g (list v2 v) #f)))
           (get-vertices g))))
    (get-vertices g))))
(provide
 (proc-doc/names
  strict-partial-order?
  (-> preprior-graph? boolean?)
  (g)
  @{Checks whether @racket[g] represents a strict partial order.
 If the transitive closure (without implied reachability of self) contains loops,
 including edges from an atom to itself, this is not the case.}))

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
      (check-false (strict-partial-order? g))))
  (let ([g (mk-preprior-graph)]
        [u (interpret-abstract-atom "foo(α1)")]
        [v (abstract-atom 'bar '())]
        [w (interpret-abstract-atom "foo(γ1)")])
    (begin
      (add-vertex! g u)
      (add-vertex! g v)
      (add-vertex! g w)
      (add-directed-edge! g u v)
      (add-directed-edge! g v w)
      (check-false (strict-partial-order? g)))))