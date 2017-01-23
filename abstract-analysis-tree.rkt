#lang at-exp racket
(require (only-in "abstract-multi-domain.rkt" abstract-atom?))
(require "abstract-knowledge.rkt")
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require "concrete-domain.rkt")
(require "abstract-substitution.rkt")
(require scribble/srcdoc)
(require racket/serialize)
(require (for-doc scribble/manual))
(require (only-in "abstract-domain-ordering.rkt" renames? >=-extension))
(require (only-in "execution.rkt" selected-index))
(require "abstract-resolve.rkt")
(require racket-tree-utils/src/tree)
(require "data-utils.rkt")
(require (only-in "similarity.rkt" s-similar?))
(require "abstract-analysis.rkt")
(require "preprior-graph.rkt")
(module+ test
  (require rackunit)
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
(provide largest-node-index)

(define (candidate-for-undo t)
  (match t
    [(node _ (list)) #f]
    [(node l ch)
     (if (andmap (compose empty? node-children) ch)
         t
         (candidate-for-undo (last (filter (compose not empty? node-children) ch))))]))
(provide candidate-for-undo)

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

(define (resolvent->node res)
  (node
   (tree-label
    (resolvent-conjunction res)
    (none)
    (resolvent-substitution res)
    (resolvent-knowledge res)
    #f ; resolvents have not yet been visited
    (list)) ; first preprior is provided right before analysis
   (list)))

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
(provide
 (proc-doc/names
  candidate-and-predecessors
  (-> node? list? (cons/c any/c (listof (cons/c (listof abstract-atom?) exact-positive-integer?))))
  (tree accumulator)
  @{Find the next candidate for unfolding and conjunctions which have already been dealt with.}))

(define (advance-analysis top candidate clauses full-evaluations concrete-constants next-index predecessors)
  (define candidate-label (node-label candidate))
  (define conjunction (label-conjunction candidate-label))
  (define more-general-predecessor (findf (λ (p-and-i) (>=-extension (car p-and-i) conjunction)) predecessors))

  ; replace a candidate by assigning an index, a selection, children and possibly a new preprior stack
  ;  (define (update-candidate idx sel ch)
  ;    (match candidate
  ;      [(node (tree-label c _ sub r _) _)
  ;       (node (tree-label c sel sub r idx) ch)]
  ;      [(node (widening c _ m _) _)
  ;       (node (widening c sel m idx) ch)]))

  ; TODO fix
  (define prior (mk-preprior-graph))
  (define (update-candidate idx sel ch) candidate)
  
  (if more-general-predecessor
      (let* ([cycle-node (node (cycle (cdr more-general-predecessor)) '())]
             [updated-candidate
              (update-candidate next-index (none) (list cycle-node))]
             [updated-top (replace-first-subtree top candidate updated-candidate)])
        (values updated-candidate updated-top))
      ; TODO: may not get a selection here...
      (let* ([selection (selected-index conjunction prior full-evaluations)]
             [similar-predecessor (foldl (λ (p acc) (if acc acc (if (s-similar? (cdr p) conjunction selection top) p acc))) #f predecessors)]
             [updated-candidate
              (if similar-predecessor
                  (let ([similarity-cycle-node (node (similarity-cycle (cdr similar-predecessor)) '())])
                    (update-candidate next-index (none) (list similarity-cycle-node)))
                  (let* ([resolvents (abstract-resolve conjunction selection clauses full-evaluations concrete-constants)]
                         ; TODO resolvents need stacks
                         ; before a child is unfolded, it gets the parent's topmost order
                         ; after completion of a child, the child's final order is pushed onto the parent's stack
                         [child-trees (map resolvent->node resolvents)])
                    (update-candidate next-index (some selection) child-trees)))]
             [updated-top (replace-first-subtree top candidate updated-candidate)])
        (values updated-candidate updated-top))))
(provide
 (proc-doc/names
  advance-analysis
  (-> node? node? (listof ck:rule?) (listof full-evaluation?) (listof function?) exact-positive-integer? list? (values node? node?))
  (top candidate clauses full-evaluations concrete-constants next-index more-general-predecessor)
  @{Advances the analysis in @racket[top], when the next conjunction up for unfolding or full evaluation is @racket[candidate],
 the knowledge base consists of concrete clauses @racket[clauses] and full evaluation rules @racket[full-evaluations].
 If a more general conjunction than that for @racket[candidate] exists earlier in the tree, it is supplied as @racket[more-general-predecessor].
 Otherwise, @racket[more-general-predecessor] should be @racket[#f].
 Returns two values: the updated candidate and the updated top-level tree.}))

(module+ test
  (check-equal?
   
   (advance-analysis top cand primes-clauses primes-evals primes-consts 1 (list (mk-preprior-graph)))
   (values)))

(module+ test 
  (test-case
   "candidate and predecessors for various scenarios"
   (let ([tree (node (tree-label (list (interpret-abstract-atom "foo(γ1)")) (none) '() #f #f) '())])
     (check-equal? (candidate-and-predecessors tree '())
                   (cons (some tree) '())))
   (let* ([leaf1
           (node (tree-label (list (interpret-abstract-atom "bar(γ1)")) (none) '() #f #f) '())]
          [leaf2
           (node (tree-label (list (interpret-abstract-atom "baz(α1)")) (none) '() #f #f) '())]
          [tree
           (node
            (tree-label (list (interpret-abstract-atom "foo(γ1)")) (some 0) '() #f 1)
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
            (tree-label (list (interpret-abstract-atom "bar(γ1)")) (some 0) '() #f 2)
            (list leaf1))]
          [leaf2
           (node (tree-label (list (interpret-abstract-atom "baz(α1)")) (none) '() #f #f) '())]
          [tree
           (node
            (tree-label (list (interpret-abstract-atom "foo(γ1)")) (some 0) '() #f 1)
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
           (node (tree-label (list (interpret-abstract-atom "a")) (none) (list) #f 3)
                 (list bottom-left))]
          [left-of-root
           (node (tree-label (list (interpret-abstract-atom "b")) (some 0) (list) #f 2)
                 (list above-bottom-left))]
          [bottom-right
           (node (tree-label (list (interpret-abstract-atom "c")) (none) (list) #f #f) '())]
          [tree
           (node (tree-label (list (interpret-abstract-atom "a")) (some 0) (list) #f 1)
                 (list left-of-root bottom-right))])
     (check-equal?
      (car (candidate-and-predecessors tree '()))
      (some bottom-right)))))
