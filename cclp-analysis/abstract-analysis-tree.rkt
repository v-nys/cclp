#lang at-exp racket
(require
  racket/match
  racket/logging
  scribble/srcdoc)
(require
  graph
  positional-tree-utils)
(require
  cclp-common/abstract-analysis
  (only-in cclp-common/abstract-domain-ordering renames? >=-extension)
  cclp-common-data/abstract-knowledge
  (only-in cclp-common-data/abstract-multi-domain abstract-atom? abstract-conjunct? multi?)
  cclp-analysis/abstract-resolve
  (only-in cclp-common-data/concrete-domain function?)
  (prefix-in ck: cclp-common-data/concrete-knowledge)
  (only-in cclp-common/control-flow aif it)
  cclp-common/data-utils
  (only-in "execution.rkt" selected-index)
  (only-in "generalize.rkt" generalize/bu)
  (only-in "genealogical-graph.rkt" active-branch)
  cclp-common/preprior-graph
  (only-in cclp-common/multi-unfolding unfold-multi-bounded unfold-multi*)
  (only-in cclp-common/abstraction-inspection-utils assemble-var-indices)
  (only-in cclp-analysis/depth-k-abstraction abstract))
(require (for-doc scribble/manual))

(module+ test
  (require rackunit))

(define (largest-node-index t)
  (match t
    [(node label (list))
     #:when (not (label-selection label))
     #f]
    [(node label (list)) ;; selection and no children = explicit failure
     (label-index label)]
    [(node label children)
     (or
      (foldr
       (λ (c acc)
         (if (and
              (not acc)
              (or
               (generalization? (node-label c))
               (tree-label? (node-label c))))
             (largest-node-index c)
             acc))
       #f
       children)
      (label-index label))]))
(provide
 (proc-doc/names
  largest-node-index
  (-> node? (or/c exact-nonnegative-integer? #f))
  (top)
  @{Finds the largest index currently used in the abstract analysis tree @racket[top].
 This assumes @racket[top] conforms to the conventions for assigning indices.}))
;(module+ test
;  (require (prefix-in primes-five: "analysis-trees/primes-five.rkt")
;           (prefix-in l4lt: "analysis-trees/linear-four-level-tree.rkt"))
;  (check-equal? (largest-node-index (node 'irrelevant (list))) #f)
;  (check-equal? (largest-node-index l4lt:val) 3)
;  (check-equal? (largest-node-index primes-five:val) 5))

(define (candidate-and-predecessors t acc)
  (match t
    [(node (tree-label '() sel sub r i) '()) (cons #f acc)]
    [(node 'fail '()) (cons #f acc)]
    [(node (cycle _) '()) (cons #f acc)]
    [(node (widening '() _ _ _) '()) (cons #f acc)] ; odd, but not impossible...
    [(or (node (tree-label c (none) _ _ #f) '())
         (node (generalization c (none) #f _ _) '()))
     (cons t acc)]
    [(node (widening c (none) msg i) '())
     (cons (node (widening c (none) msg i) '()) acc)]
    ; children but no selection = widened tree-label (or widened widening) or cycle
    [(or (node (tree-label c (none) _ _ i) (list single-child))
         (node (generalization c (none) i _ _) (list single-child))
         (node (widening c (none) _ i) (list single-child)))
     (candidate-and-predecessors single-child (cons (cons c i) acc))]
    [(or (node (tree-label c (some v) _ _ i) children)
         (node (generalization c (some v) i _ _) children)
         (node (widening c (some v) _ i) children))
     (foldl
      (λ (child acc2)
        (if (car acc2)
            acc2
            (candidate-and-predecessors
             child
             (cdr acc2))))
      (cons #f (cons (cons c i) acc))
      (node-children t))]))
;(module+ test
;  (require
;    (prefix-in primes0: "analysis-trees/primes-zero.rkt")
;    (prefix-in primes2: "analysis-trees/primes-two.rkt")
;    (prefix-in primes2cand: "analysis-trees/primes-two-candidate.rkt")
;    (prefix-in primes2cp: "analysis-trees/primes-two-candidate-post.rkt")
;    (prefix-in primes4: "analysis-trees/primes-four.rkt")
;    (prefix-in primes4cand: "analysis-trees/primes-four-candidate.rkt")
;    (prefix-in primes4cp: "analysis-trees/primes-four-candidate-post.rkt")
;    (prefix-in permsort: "analysis-trees/permsort-tree.rkt"))
;  (check-equal? (candidate-and-predecessors primes0:val (list)) (cons primes0:val (list)))
;  (check-equal?
;   (candidate-and-predecessors primes2:val (list))
;   (cons primes2cand:val
;         (list (cons (interpret-abstract-conjunction "integers(g2,a6),sift(a6,a5),length(a5,g1)") 2)
;               (cons (interpret-abstract-conjunction "primes(g1,a1)") 1))))
;  (check-equal?
;   (candidate-and-predecessors primes4:val (list))
;   (cons primes4cand:val
;         (list (cons (interpret-abstract-conjunction "length([],g1)") 4)
;               (cons (interpret-abstract-conjunction "sift([],a5),length(a5,g1)") 3)
;               (cons (interpret-abstract-conjunction "integers(g2,a6),sift(a6,a5),length(a5,g1)") 2)
;               (cons (interpret-abstract-conjunction "primes(g1,a1)") 1))))
;  (check-equal?
;   (car (candidate-and-predecessors permsort:val (list)))
;   #f))
(provide
 (proc-doc/names
  candidate-and-predecessors
  (-> node? list? (cons/c (or/c #f node?) (listof (cons/c (listof abstract-conjunct?) exact-positive-integer?))))
  (tree accumulator)
  @{Find the next candidate for unfolding and conjunctions which have already been dealt with.}))

(define (advance-analysis top clauses full-evaluations concrete-constants prior #:new-edges [new-edges (list)] #:k [k #f])
  (log-debug "advancing analysis")
  (define (update-candidate candidate idx sel children)
    (match candidate
      [(node (tree-label c _ sub r _) _)
       (node (tree-label c sel sub r idx) children)]
      [(node (generalization c _ _ a-r bb) _)
       (node (generalization c sel idx a-r bb) children)]
      [(node (widening c _ m _) _)
       (node (widening c sel m idx) children)]
      [(node (case c _ _) _)
       (node (case c sel idx) children)]))
  (define (resolvent->node res)
    (node
     (tree-label
      (resolvent-conjunction res)
      (none)
      (resolvent-substitution res)
      (resolvent-knowledge res)
      #f)
     (list)))
  (define (m-unf->node m-unf type)
    (node
     (tree-label
      m-unf
      (none)
      (list)
      type
      #f)
     (list)))
  (define (full-eval-covers lst)
    (match lst
      [(list full-eval conjunct)
       (and (abstract-atom? conjunct)
            (>=-extension (full-evaluation-input-pattern full-eval) conjunct))]))
  (match-define (cons candidate predecessors) (candidate-and-predecessors top (list)))
  (if candidate
      (begin
        (match-let* ([next-index (aif (largest-node-index top) (+ it 1) 1)]
                     [conjunction (label-conjunction (node-label candidate))]
                     [first-eq-predecessor
                      (findf
                       (λ (p-and-i) (renames? (car p-and-i) conjunction))
                       (sort predecessors (λ (pi1 pi2) (< (cdr pi1) (cdr pi2)))))]
                     [fully-evaluated-atom? (ormap full-eval-covers (cartesian-product full-evaluations conjunction))]
                     [(list gen-conjunction gen-rngs bb)
                      (if
                       (or (null? (node-children top)) first-eq-predecessor fully-evaluated-atom?)
                       (list conjunction (list) (list))
                       (generalize/bu (active-branch top)))])
          (begin
            (cond [first-eq-predecessor
                   (let* ([cycle-node (node (cycle (cdr first-eq-predecessor)) '())]
                          [updated-candidate (update-candidate candidate next-index (none) (list cycle-node))]
                          [updated-top (replace-first-subtree top candidate updated-candidate)])
                     (cons updated-candidate updated-top))]
                  [(not (null? gen-rngs))
                   (let* ([gen-node (node (generalization gen-conjunction (none) #f gen-rngs bb) '())]
                          [updated-candidate (update-candidate candidate next-index (none) (list gen-node))]
                          [updated-top (replace-first-subtree top candidate updated-candidate)])
                     (cons updated-candidate updated-top))]
                  [else
                   (begin
                     (for ([conjunct conjunction])
                       (cond [(abstract-atom? conjunct) (add-vertex! prior conjunct)]
                             [(multi? conjunct)
                              (let ([one-unf (let* ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) conjunct)))]) (car (unfold-multi-bounded 1 conjunct offset offset)))])
                                (for ([multi-conjunct one-unf])
                                  (add-vertex! prior multi-conjunct)))]))
                     (for ([edge new-edges]) (add-directed-edge! prior (car edge) (cdr edge)))
                     (aif (selected-index conjunction prior full-evaluations)
                          (let* ([selected-conjunct (list-ref conjunction it)]
                                 [resolvents
                                  (if (abstract-atom? selected-conjunct)
                                      (if k
                                          (map (λ (r) (struct-copy resolvent r [conjunction (abstract (resolvent-conjunction r) k)]))
                                               (reverse (abstract-resolve conjunction it clauses full-evaluations concrete-constants)))
                                          (reverse (abstract-resolve conjunction it clauses full-evaluations concrete-constants)))
                                      (unfold-multi* it conjunction))]
                                 [child-nodes (if (abstract-atom? selected-conjunct) (map resolvent->node resolvents) (map m-unf->node resolvents '(one many)))]
                                 [updated-candidate (update-candidate candidate next-index (some it) child-nodes)]
                                 [updated-top (replace-first-subtree top candidate updated-candidate)])
                            (cons updated-candidate updated-top))
                          (cons 'underspecified-order candidate)))]))))
      'no-candidate))
(provide
 (proc-doc/names
  advance-analysis
  (->* (node? (listof ck:rule?) (listof full-evaluation?) (listof function?) preprior-graph?)
       (#:new-edges (listof (cons/c abstract-atom? abstract-atom?))
        #:k (or/c #f exact-positive-integer?))
       (or/c 'no-candidate (cons/c 'underspecified-order node?) (cons/c node? node?)))
  ((top clauses full-evaluations concrete-constants prior) ((new-edges (list)) (k #f)))
  @{Advances the analysis in @racket[top], when the knowledge base consists of concrete clauses @racket[clauses] and full evaluation rules @racket[full-evaluations].
 Functions supplied in @racket[concrete-constants] are considered to be in the abstract domain.
 The strict partial order used for atom selection is specified in @racket[prior].
 If selection requires the addition of additional precedence pairs, these are specified in @racket[new-edges].
 There are three possible outcomes: @itemlist[@item{there are no more candidates} @item{analysis requires more info about the selection rule} @item{analysis proceeds normally}]
 In the first case, a symbol is returned.
 In the second case, a symbol is returned, along with the current candidate, so that the user may be offered a choice from the current conjunction.
 In the final case, this returns a pair of values: the updated candidate followed by the updated top-level tree.}))

;(module+ test
;  (require 
;    (prefix-in primes1: "analysis-trees/primes-one.rkt")
;    (prefix-in primes1cp: "analysis-trees/primes-one-candidate-post.rkt")
;    (prefix-in primes3: "analysis-trees/primes-three.rkt")
;    (prefix-in primes3cp: "analysis-trees/primes-three-candidate-post.rkt")
;    (prefix-in primes5: "analysis-trees/primes-five.rkt")
;    (prefix-in permsortsanscy: "analysis-trees/permsort-tree-before-cycle.rkt")
;    (prefix-in pscandpost: "analysis-trees/permsort-cycle-candidate-post.rkt"))
;  (define-syntax-rule
;    (advance-permsort-analysis top)
;    (advance-analysis top permsort-clauses (map full-ai-rule->full-evaluation permsort-full-evals) permsort-consts permsort-prior))
;  (check-equal?
;   (advance-permsort-analysis permsort:val)
;   'no-candidate
;   "case without a candidate")
;  (check-equal?
;   (advance-permsort-analysis permsortsanscy:val)
;   (cons pscandpost:val permsort:val)
;   "case introducing a cycle")
;  (check-equal?
;   (advance-analysis primes2:val primes-clauses (map full-ai-rule->full-evaluation primes-full-evals) primes-consts (mk-preprior-graph))
;   (cons 'underspecified-order primes2cand:val)
;   "case of an underspecified partial order")
;  (define primes-prior (mk-preprior-graph))
;  (define-syntax-rule
;    (advance-primes-analysis top new-edges)
;    (advance-analysis top primes-clauses (map full-ai-rule->full-evaluation primes-full-evals) primes-consts primes-prior #:new-edges new-edges))
;  (define-syntax-rule
;    (test-advance top-pre cand-post top-post new-edges)
;    (let* ([cp-tp (advance-primes-analysis top-pre new-edges)])
;      (begin
;        (check-equal? (car cp-tp) cand-post)
;        (check-equal? (cdr cp-tp) top-post))))
;  ; regular steps, without updates to the selection rule
;  (test-advance primes0:val primes1:val primes1:val (list))
;  (define edge1 (cons (interpret-abstract-atom "integers(g1,a1)") (interpret-abstract-atom "sift(a1,a2)")))
;  (define edge2 (cons (interpret-abstract-atom "integers(g1,a1)") (interpret-abstract-atom "length(a1,g1)")))
;  (add-directed-edge! primes-prior (car edge1) (cdr edge1))
;  (add-directed-edge! primes-prior (car edge2) (cdr edge2))
;  (test-advance primes1:val primes1cp:val primes2:val (list edge1 edge2))
;  (define edge3 (cons (interpret-abstract-atom "sift([],a1)") (interpret-abstract-atom "length(a1,g1)")))
;  (add-directed-edge! primes-prior (car edge3) (cdr edge3))
;  (test-advance primes2:val primes2cp:val primes3:val (list edge3))
;  (test-advance primes3:val primes3cp:val primes4:val (list))
;  ; fully evaluated atom
;  (test-advance primes4:val primes4cp:val primes5:val (list)))

; TODO reintroduce code for tests advance in primes trees (see VC)

; TODO reintroduce useful candidate-and-predecessors tests (see VC)