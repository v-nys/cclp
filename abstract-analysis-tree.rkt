#lang at-exp racket
(require
  racket/match
  racket/serialize ; trees can be saved and loaded
  scribble/srcdoc)
(require
  graph
  racket-tree-utils/src/tree)
(require
  "abstract-analysis.rkt"
  (only-in "abstract-domain-ordering.rkt" renames? >=-extension)
  "abstract-knowledge.rkt"
  (only-in "abstract-multi-domain.rkt" abstract-atom?)
  "abstract-resolve.rkt"
  (only-in "concrete-domain.rkt" function?)
  (prefix-in ck: "concrete-knowledge.rkt")
  (only-in "control-flow.rkt" aif it)
  "data-utils.rkt"
  (only-in "execution.rkt" selected-index)
  "preprior-graph.rkt")
(require (for-doc scribble/manual))

(module+ test
  (require rackunit)
  (require "test-data.rkt")
  (require "cclp-interpreter.rkt"))

(define (largest-node-index t)
  (match t
    [(node label (list)) #f]
    [(node label children)
     (or
      (foldr
       (λ (c acc)
         (if (and (not acc) (tree-label? (node-label c)))
             (largest-node-index c)
             acc))
       #f
       children)
      (tree-label-index label))]))
(provide
 (proc-doc/names
  largest-node-index
  (-> node? (or/c exact-nonnegative-integer? #f))
  (top)
  @{Finds the largest index currently used in the abstract analysis tree @racket[top].
 This assumes @racket[top] conforms to the conventions for assigning indices.}))
(module+ test
  (require (prefix-in primes-five: "analysis-trees/primes-five.rkt")
           (prefix-in l4lt: "analysis-trees/linear-four-level-tree.rkt"))
  (check-equal? (largest-node-index (node 'irrelevant (list))) #f)
  (check-equal? (largest-node-index l4lt:val) 3)
  (check-equal? (largest-node-index primes-five:val) 5))

; TODO reintroduce rewind-related functionality (see VC)

(define (candidate-and-predecessors t acc)
  (match t
    [(node (tree-label '() _ _ _ _ _) '()) (cons #f acc)]
    [(node 'fail '()) (cons #f acc)]
    [(node (cycle _) '()) (cons #f acc)]
    [(node (widening '() _ _ _ _) '()) (cons #f acc)] ; odd, but not impossible...
    [(node (tree-label c (none) s r #f pp) '())
     (cons (node (tree-label c (none) s r #f pp) '()) acc)]
    [(node (widening c (none) msg i pp) '())
     (cons (node (widening c (none) msg i pp) '()) acc)]
    ; children but no selection = widened tree-label (or widened widening)
    [(or (node (tree-label c (none) _ _ i pp) (list single-child))
         (node (widening c (none) _ i pp) (list single-child)))
     (candidate-and-predecessors single-child (cons (cons c i) acc))]
    [(or (node (tree-label c (some v) _ _ i pp) children)
         (node (widening c (some v) _ i pp) children))
     (foldl
      (λ (child acc2)
        (if (car acc2)
            acc2
            (candidate-and-predecessors
             child
             (cdr acc2))))
      (cons #f (cons (cons c i) acc))
      (node-children t))]))
(module+ test
  (require
    (prefix-in primes0: "analysis-trees/primes-zero.rkt")
    (prefix-in primes2: "analysis-trees/primes-two.rkt")
    (prefix-in primes2cand: "analysis-trees/primes-two-candidate.rkt")
    (prefix-in primes2cp: "analysis-trees/primes-two-candidate-post.rkt")
    (prefix-in primes4: "analysis-trees/primes-four.rkt")
    (prefix-in primes4cand: "analysis-trees/primes-four-candidate.rkt")
    (prefix-in primes4cp: "analysis-trees/primes-four-candidate-post.rkt")
    (prefix-in permsort: "analysis-trees/permsort-tree.rkt"))
  (check-equal? (candidate-and-predecessors primes0:val (list)) (cons primes0:val (list)))
  (check-equal?
   (candidate-and-predecessors primes2:val (list))
   (cons primes2cand:val
         (list (cons (interpret-abstract-conjunction "integers(γ2,α6),sift(α6,α5),length(α5,γ1)") 2)
               (cons (interpret-abstract-conjunction "primes(γ1,α1)") 1))))
  (check-equal?
   (candidate-and-predecessors primes4:val (list))
   (cons primes4cand:val
         (list (cons (interpret-abstract-conjunction "length([],γ1)") 4)
               (cons (interpret-abstract-conjunction "sift([],α5),length(α5,γ1)") 3)
               (cons (interpret-abstract-conjunction "integers(γ2,α6),sift(α6,α5),length(α5,γ1)") 2)
               (cons (interpret-abstract-conjunction "primes(γ1,α1)") 1))))
  (check-equal?
   (car (candidate-and-predecessors permsort:val (list)))
   #f))
(provide
 (proc-doc/names
  candidate-and-predecessors
  (-> node? list? (cons/c (or/c #f node?) (listof (cons/c (listof abstract-atom?) exact-positive-integer?))))
  (tree accumulator)
  @{Find the next candidate for unfolding and conjunctions which have already been dealt with.}))

(define (advance-analysis top clauses full-evaluations concrete-constants prior #:new-edges [new-edges (list)])
  (define (update-candidate candidate idx sel new-edges children)
    (match candidate
      [(node (tree-label c _ sub r _ _) _)
       (node (tree-label c sel sub r idx new-edges) children)]
      [(node (widening c _ m _ _) _)
       (node (widening c sel m idx new-edges) children)]
      [(node (case c _ _ _) _)
       (node (case c sel idx new-edges) children)]))
  (define (resolvent->node res)
    (node
     (tree-label
      (resolvent-conjunction res)
      (none)
      (resolvent-substitution res)
      (resolvent-knowledge res)
      #f ; resolvents have not yet been visited, so no index
      (list))
     (list)))
  (match-define (cons candidate predecessors) (candidate-and-predecessors top (list)))
  (if candidate
      (let* ([next-index (aif (largest-node-index top) (+ it 1) 1)]
             [conjunction (label-conjunction (node-label candidate))]
             [equivalent-predecessor
              (findf
               (λ (p-and-i) (renames? (car p-and-i) conjunction))
               predecessors)])
        (if equivalent-predecessor
            (let* ([cycle-node (node (cycle (cdr equivalent-predecessor)) '())]
                   [updated-candidate (update-candidate candidate next-index (none) (list) (list cycle-node))]
                   [updated-top (replace-first-subtree top candidate updated-candidate)])
              (cons updated-candidate updated-top))
            ; TODO additional case here: the current conjunction can be generalized
            ; at this point, candidate is known
            ; but generalization needs the whole branch anyway
            ; ideally, candidate-and-predecessors would actually collect candidate, active branch and predecessors, so this is not the most efficient approach
            ; anyway, generalization gets the whole tree, extracts the active branch, computes the generational tree (do I need to find relevant target atoms first?), does horizontal traversal of level at the appropriate depth, groups atoms with non-zero generation into a multi abstraction
            (begin
              (for ([conjunct conjunction]) (add-vertex! prior conjunct))
              (for ([edge new-edges]) (add-directed-edge! prior (car edge) (cdr edge)))
              (unless (strict-partial-order? prior) (error "Selection rule is no longer a strict partial order!"))
              (aif (selected-index conjunction prior full-evaluations)
                   (let* ([resolvents (reverse (abstract-resolve conjunction it clauses full-evaluations concrete-constants))]
                          [child-nodes (map resolvent->node resolvents)]
                          [updated-candidate (update-candidate candidate next-index (some it) new-edges child-nodes)]
                          [updated-top (replace-first-subtree top candidate updated-candidate)])
                     (cons updated-candidate updated-top))
                   (cons 'underspecified-order candidate)))))
      'no-candidate))
(provide
 (proc-doc/names
  advance-analysis
  (->* (node? (listof ck:rule?) (listof full-evaluation?) (listof function?) preprior-graph?)
       (#:new-edges (listof (cons/c abstract-atom? abstract-atom?)))
       (or/c 'no-candidate (cons/c 'underspecified-order node?) (cons/c node? node?)))
  ((top clauses full-evaluations concrete-constants prior) ((new-edges (list))))
  @{Advances the analysis in @racket[top], when the knowledge base consists of concrete clauses @racket[clauses] and full evaluation rules @racket[full-evaluations].
 Functions supplied in @racket[concrete-constants] are considered to be in the abstract domain.
 The strict partial order used for atom selection is specified in @racket[prior].
 If selection requires the addition of additional precedence pairs, these are specified in @racket[new-edges].
 There are three possible outcomes: @itemlist[@item{there are no more candidates} @item{analysis requires more info about the selection rule} @item{analysis proceeds normally}]
 In the first case, a symbol is returned.
 In the second case, a symbol is returned, along with the current candidate, so that the user may be offered a choice from the current conjunction.
 In the final case, this returns a pair of values: the updated candidate followed by the updated top-level tree.}))

(module+ test
  (require 
    (prefix-in primes1: "analysis-trees/primes-one.rkt")
    (prefix-in primes1cp: "analysis-trees/primes-one-candidate-post.rkt")
    (prefix-in primes3: "analysis-trees/primes-three.rkt")
    (prefix-in primes3cp: "analysis-trees/primes-three-candidate-post.rkt")
    (prefix-in primes5: "analysis-trees/primes-five.rkt")
    (prefix-in permsortsanscy: "analysis-trees/permsort-tree-before-cycle.rkt")
    (prefix-in pscandpost: "analysis-trees/permsort-cycle-candidate-post.rkt"))
  (define-syntax-rule
    (advance-permsort-analysis top)
    (advance-analysis top permsort-clauses (map full-ai-rule->full-evaluation permsort-full-evals) permsort-consts permsort-prior))
  (check-equal?
   (advance-permsort-analysis permsort:val)
   'no-candidate
   "case without a candidate")
  (check-equal?
   (advance-permsort-analysis permsortsanscy:val)
   (cons pscandpost:val permsort:val)
   "case introducing a cycle")
  (check-equal?
   (advance-analysis primes2:val primes-clauses (map full-ai-rule->full-evaluation primes-full-evals) primes-consts (mk-preprior-graph))
   (cons 'underspecified-order primes2cand:val)
   "case of an underspecified partial order")
  (define primes-prior (mk-preprior-graph))
  (define-syntax-rule
    (advance-primes-analysis top new-edges)
    (advance-analysis top primes-clauses (map full-ai-rule->full-evaluation primes-full-evals) primes-consts primes-prior #:new-edges new-edges))
  (define-syntax-rule
    (test-advance top-pre cand-post top-post new-edges)
    (let* ([cp-tp (advance-primes-analysis top-pre new-edges)])
      (begin
        (check-equal? (car cp-tp) cand-post)
        (check-equal? (cdr cp-tp) top-post))))
  ; regular steps, without updates to the selection rule
  (test-advance primes0:val primes1:val primes1:val (list))
  (define edge1 (cons (interpret-abstract-atom "integers(γ1,α1)") (interpret-abstract-atom "sift(α1,α2)")))
  (define edge2 (cons (interpret-abstract-atom "integers(γ1,α1)") (interpret-abstract-atom "length(α1,γ1)")))
  (add-directed-edge! primes-prior (car edge1) (cdr edge1))
  (add-directed-edge! primes-prior (car edge2) (cdr edge2))
  (test-advance primes1:val primes1cp:val primes2:val (list edge1 edge2))
  (define edge3 (cons (interpret-abstract-atom "sift([],α1)") (interpret-abstract-atom "length(α1,γ1)")))
  (add-directed-edge! primes-prior (car edge3) (cdr edge3))
  (test-advance primes2:val primes2cp:val primes3:val (list edge3))
  (test-advance primes3:val primes3cp:val primes4:val (list))
  ; fully evaluated atom
  (test-advance primes4:val primes4cp:val primes5:val (list)))

; TODO reintroduce code for tests advance in primes trees (see VC)

; TODO reintroduce useful candidate-and-predecessors tests (see VC)