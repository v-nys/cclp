#lang at-exp racket
(require
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

; TODO check if this needs to be updated
;(define (candidate-for-undo t)
;  (match t
;    [(node _ (list)) #f]
;    [(node l ch)
;     (if (andmap (compose empty? node-children) ch)
;         t
;         (candidate-for-undo (last (filter (compose not empty? node-children) ch))))]))
;(provide candidate-for-undo)

;(define (undo t)
;  (match t
;    [(node (tree-label con sel sub r i) ch)
;     (node (tree-label con (none) sub r #f) (list))]))
;
;(define (rewind t)
;  (let* ([candidate (candidate-for-undo t)]
;         [locally-rewound (if candidate (undo candidate) #f)])
;    (if candidate
;        (cons locally-rewound (replace-last-subtree t candidate locally-rewound))
;        #f)))
;(provide
; (proc-doc/names
;  rewind
;  (-> node? (or/c #f (cons/c node? node?)))
;  (t)
;  @{Undo the latest unfolding or generalization that occurred in @racket[t].
; The result is a @racket[pair] containing the node on which the operation has been applied
; and the top-level tree to which this node belongs, or @racket[#f].}))

; TODO check if this needs to be updated
;(define (resolvent->node res)
;  (node
;   (tree-label
;    (resolvent-conjunction res)
;    (none)
;    (resolvent-substitution res)
;    (resolvent-knowledge res)
;    #f ; resolvents have not yet been visited
;    (list)) ; first preprior is provided right before analysis
;   (list)))

(define (candidate-and-predecessors t acc)
  (match t
    [(node (tree-label '() _ _ _ _ _) '()) (cons (none) acc)]
    [(node 'fail '()) (cons (none) acc)]
    [(node (cycle _) '()) (cons (none) acc)]
    [(node (widening '() _ _ _ _) '()) (cons (none) acc)]
    [(node (tree-label c (none) s r #f pp) '())
     (cons (some (node (tree-label c (none) s r #f pp) '())) acc)]
    [(node (widening c (none) msg i pp) '())
     (cons (some (node (widening c (none) msg i pp) '())) acc)]
    ; children but no selection = widening or cycle
    [(node (tree-label c (none) s r i pp) (list single-child))
     (candidate-and-predecessors single-child (cons (cons c i) acc))]
    [(node (widening c (none) msg i pp) (list single-child))
     (candidate-and-predecessors single-child (cons (cons c i) acc))]
    ; next two cases are basically the same...
    [(node (tree-label c (some v) _ _ i pp) children)
     (foldl
      (λ (child acc2)
        (if (some? (car acc2))
            acc2
            (candidate-and-predecessors
             child
             (cdr acc2))))
      (cons (none) (cons (cons c i) acc))
      (node-children t))]
    [(node (widening c (some v) msg i pp) children)
     (foldl
      (λ (child acc2)
        (if (some? (car acc2))
            acc2
            (candidate-and-predecessors
             child
             (cdr acc2))))
      (cons (none) (cons (cons c i) acc))
      (node-children t))]))
(module+ test
  (require
    (prefix-in primes0: "analysis-trees/primes-zero.rkt")
    (prefix-in primes1: "analysis-trees/primes-one.rkt")
    (prefix-in primes1cp: "analysis-trees/primes-one-candidate-post.rkt")
    (prefix-in primes2: "analysis-trees/primes-two.rkt")
    (prefix-in primes2cp: "analysis-trees/primes-two-candidate-post.rkt")
    (prefix-in primes3: "analysis-trees/primes-three.rkt")
    (prefix-in primes3cp: "analysis-trees/primes-three-candidate-post.rkt")
    (prefix-in primes4: "analysis-trees/primes-four.rkt")
    (prefix-in primes4cp: "analysis-trees/primes-four-candidate-post.rkt")
    (prefix-in primes5: "analysis-trees/primes-five.rkt"))
  (check-equal? (candidate-and-predecessors primes0:val (list)) (cons primes0:val (list))))
(provide
 (proc-doc/names
  candidate-and-predecessors
  (-> node? list? (cons/c (or/c #f node?) (listof (cons/c (listof abstract-atom?) exact-positive-integer?))))
  (tree accumulator)
  @{Find the next candidate for unfolding and conjunctions which have already been dealt with.}))

(define (advance-analysis top clauses full-evaluations concrete-constants next-index prior) (error "not implemented yet"))
(provide
 (proc-doc/names
  advance-analysis
  (-> node? node? (listof ck:rule?) (listof full-evaluation?) (listof function?) exact-positive-integer? (listof (cons/c abstract-atom? exact-positive-integer?))
      (or/c 'no-candidate (cons/c 'underspecified-order node?) (cons/c node? node?)))
  (top candidate clauses full-evaluations concrete-constants next-index predecessors)
  @{Advances the analysis in @racket[top], when the next conjunction up for unfolding or full evaluation is @racket[candidate],
 the knowledge base consists of concrete clauses @racket[clauses] and full evaluation rules @racket[full-evaluations].
 Potential predecessors of the current candidate are supplied as @racket[predecessors].
 Returns a pair of values: the updated candidate and the updated top-level tree.}))

(module+ test
  ; TODO add test involving more general predecessor
  ; TODO add tests with cycles,...
  (define-syntax-rule
    (advance-primes-analysis top cand i predecessors)
    (advance-analysis top cand primes-clauses (map full-ai-rule->full-evaluation primes-full-evals) primes-consts i predecessors))
  (define-syntax-rule
    (test-advance top-pre cand-post top-post)
    (let* ([c-p (candidate-and-predecessors top-pre (list))]
           [cp-tp (advance-primes-analysis top-pre (some-v (car c-p)) (or (largest-node-index top-pre) 1) (cdr c-p))])
      (begin
        (check-equal? (car cp-tp) cand-post)
        (check-equal? (cdr cp-tp) top-post))))
  (test-advance primes0:val primes1:val primes1:val)
  (test-advance primes1:val primes1cp:val primes2:val)
  (test-advance primes2:val primes2cp:val primes3:val)
  (test-advance primes3:val primes3cp:val primes4:val)
  (test-advance primes4:val primes4cp:val primes5:val))

(module+ test 
  (test-case
   "candidate and predecessors for various scenarios"
   (let ([tree (node (tree-label (list (interpret-abstract-atom "foo(γ1)")) (none) '() #f #f (list)) '())])
     (check-equal? (candidate-and-predecessors tree '())
                   (cons (some tree) '())))
   (let* ([leaf1
           (node (tree-label (list (interpret-abstract-atom "bar(γ1)")) (none) '() #f #f (list)) '())]
          [leaf2
           (node (tree-label (list (interpret-abstract-atom "baz(α1)")) (none) '() #f #f (list)) '())]
          [tree
           (node
            (tree-label (list (interpret-abstract-atom "foo(γ1)")) (some 0) '() #f 1 (list))
            (list leaf1 leaf2))])
     (begin
       (check-equal?
        (node-label (some-v (car (candidate-and-predecessors tree '()))))
        (node-label leaf1))
       (check-equal?
        (cdr (candidate-and-predecessors tree '()))
        (list (cons (list (interpret-abstract-atom "foo(γ1)")) 1)))))
   (let* ([leaf1 (node 'fail '())]
          [middle
           (node
            (tree-label (list (interpret-abstract-atom "bar(γ1)")) (some 0) '() #f 2 (list))
            (list leaf1))]
          [leaf2
           (node (tree-label (list (interpret-abstract-atom "baz(α1)")) (none) '() #f #f (list)) '())]
          [tree
           (node
            (tree-label (list (interpret-abstract-atom "foo(γ1)")) (some 0) '() #f 1 (list))
            (list middle leaf2))])
     (begin
       (check-equal?
        (car (candidate-and-predecessors tree '()))
        (some leaf1))
       (check-equal?
        (cdr (candidate-and-predecessors tree '()))
        (list
         (cons (list (interpret-abstract-atom "bar(γ1)")) 2)
         (cons (list (interpret-abstract-atom "foo(γ1)")) 1)))))
   (let* ([bottom-left (node (cycle 1) '())]
          [above-bottom-left
           (node (tree-label (list (interpret-abstract-atom "a")) (none) (list) #f 3 (list))
                 (list bottom-left))]
          [left-of-root
           (node (tree-label (list (interpret-abstract-atom "b")) (some 0) (list) #f 2 (list))
                 (list above-bottom-left))]
          [bottom-right
           (node (tree-label (list (interpret-abstract-atom "c")) (none) (list) #f #f (list)) '())]
          [tree
           (node (tree-label (list (interpret-abstract-atom "a")) (some 0) (list) #f 1 (list))
                 (list left-of-root bottom-right))])
     (check-equal?
      (car (candidate-and-predecessors tree '()))
      (some bottom-right)))))
