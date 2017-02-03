#lang at-exp racket
(require
  racket/match
  racket/serialize ; trees can be saved and loaded
  scribble/srcdoc)
(require racket-tree-utils/src/tree)
(require
  "abstract-analysis.rkt"
  (only-in "abstract-domain-ordering.rkt" renames? >=-extension)
  "abstract-knowledge.rkt"
  (only-in "abstract-multi-domain.rkt" abstract-atom?)
  "abstract-resolve.rkt"
  (only-in "concrete-domain.rkt" function?)
  (prefix-in ck: "concrete-knowledge.rkt")
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
    (prefix-in primes1: "analysis-trees/primes-one.rkt")
    (prefix-in primes1cp: "analysis-trees/primes-one-candidate-post.rkt")
    (prefix-in primes2: "analysis-trees/primes-two.rkt")
    (prefix-in primes2cand: "analysis-trees/primes-two-candidate.rkt")
    (prefix-in primes2cp: "analysis-trees/primes-two-candidate-post.rkt")
    (prefix-in primes3: "analysis-trees/primes-three.rkt")
    (prefix-in primes3cp: "analysis-trees/primes-three-candidate-post.rkt")
    (prefix-in primes4: "analysis-trees/primes-four.rkt")
    (prefix-in primes4cand: "analysis-trees/primes-four-candidate.rkt")
    (prefix-in primes4cp: "analysis-trees/primes-four-candidate-post.rkt")
    (prefix-in primes5: "analysis-trees/primes-five.rkt")
    (prefix-in wid: "analysis-trees/widening-tree.rkt")
    (prefix-in widcand: "analysis-trees/widening-tree-candidate.rkt")
    (prefix-in permsort: "analysis-trees/permsort-tree.rkt"))
  (check-equal? (candidate-and-predecessors primes0:val (list)) (cons primes0:val (list)))
  (check-equal?
   (candidate-and-predecessors primes2:val (list))
   (cons primes2cand:val
         (list (cons (interpret-abstract-conjunction "integers(γ2,α2),sift(α2,α1),len(α1,γ1)") 2)
               (cons (interpret-abstract-conjunction "primes(γ1,α1)") 1))))
  (check-equal?
   (candidate-and-predecessors primes4:val (list))
   (cons primes4cand:val
         (list (cons (interpret-abstract-conjunction "len([],γ1)") 4)
               (cons (interpret-abstract-conjunction "sift([],α1),len(α1,γ1)") 3)
               (cons (interpret-abstract-conjunction "integers(γ2,α2),sift(α2,α1),len(α1,γ1)") 2)
               (cons (interpret-abstract-conjunction "primes(γ1,α1)") 1))))
  (check-equal?
   (candidate-and-predecessors wid:val (list))
   (cons widcand:val
         (list (cons (interpret-abstract-conjunction "foo(γ1)") 2)
               (cons (interpret-abstract-conjunction "foo([])") 1))))
  (check-equal?
   (car (candidate-and-predecessors permsort:val (list)))
   #f))
(provide
 (proc-doc/names
  candidate-and-predecessors
  (-> node? list? (cons/c (or/c #f node?) (listof (cons/c (listof abstract-atom?) exact-positive-integer?))))
  (tree accumulator)
  @{Find the next candidate for unfolding and conjunctions which have already been dealt with.}))

; TODO reintroduce advance-analysis (see VC)

; TODO reintroduce code for tests advance in primes trees (see VC)

; TODO reintroduce useful candidate-and-predecessors tests (see VC)