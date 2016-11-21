#lang at-exp racket
(require (only-in "abstract-multi-domain.rkt" abstract-atom?))
(require "abstract-knowledge.rkt")
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require "concrete-domain.rkt")
(require (only-in parenlog model?))
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

(define (resolvent->node res)
  (node
   (tree-label
    (resolvent-conjunction res)
    (none)
    (resolvent-substitution res)
    (resolvent-knowledge res)
    #f) ; resolvents have not yet been visited
   (list)))

(define (candidate-and-predecessors t acc)
  (match t
    [(node (tree-label '() _ _ _ _) '()) (cons (none) acc)]
    [(node 'fail '()) (cons (none) acc)]
    [(node (cycle _) '()) (cons (none) acc)]
    [(node (similarity-cycle _) '()) (cons (none) acc)]
    [(node (widening '() _ _ _) '()) (cons (none) acc)]
    [(node (tree-label c (none) s r #f) '())
     (cons (some (node (tree-label c (none) s r #f) '())) acc)]
    [(node (widening c (none) msg i) '())
     (cons (some (node (widening c (none) msg i) '())) acc)]
    ; children but no selection = widening or cycle
    [(node (tree-label c (none) s r i) (list single-child))
     (candidate-and-predecessors single-child (cons (cons c i) acc))]
    [(node (widening c (none) msg i) (list single-child))
     (candidate-and-predecessors single-child (cons (cons c i) acc))]
    ; next two cases are basically the same...
    [(node (tree-label c (some v) _ _ i) children)
     (foldl
      (λ (child acc2)
        (if (some? (car acc2))
            acc2
            (candidate-and-predecessors
             child
             (cdr acc2))))
      (cons (none) (cons (cons c i) acc))
      (node-children t))]
    [(node (widening c (some v) msg i) children)
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

(define (advance-analysis top candidate clauses full-evaluations concrete-constants prior next-index predecessors)
  (define candidate-label (node-label candidate))
  (define conjunction (label-conjunction candidate-label))
  (define more-general-predecessor (findf (λ (p-and-i) (>=-extension (car p-and-i) conjunction)) predecessors))
  (define (update-candidate idx sel ch)
    (match candidate
      [(node (tree-label c _ sub r _) _) (node (tree-label c sel sub r idx) ch)]
      [(node (widening c _ m _) _) (node (widening c sel m idx) ch)]))
  (if more-general-predecessor
      (let* ([cycle-node (node (cycle (cdr more-general-predecessor)) '())]
             [updated-candidate
              (update-candidate next-index (none) (list cycle-node))]
             [updated-top (replace-first-subtree top candidate updated-candidate)])
        (values updated-candidate updated-top))
      (let* ([selection (selected-index conjunction prior full-evaluations)]
             [similar-predecessor (foldl (λ (p acc) (if acc acc (if (s-similar? (cdr p) conjunction selection top) p acc))) #f predecessors)]
             [updated-candidate
              (if similar-predecessor
                  (let ([similarity-cycle-node (node (similarity-cycle (cdr similar-predecessor)) '())])
                    (update-candidate next-index (none) (list similarity-cycle-node)))
                  (let* ([resolvents (abstract-resolve conjunction selection clauses full-evaluations concrete-constants)]
                         [child-trees (map resolvent->node resolvents)])
                    (update-candidate next-index (some selection) child-trees)))]
             [updated-top (replace-first-subtree top candidate updated-candidate)])
        (values updated-candidate updated-top))))
(provide
 (proc-doc/names
  advance-analysis
  (-> node? node? (listof ck:rule?) (listof full-evaluation?) (listof function?) model? exact-positive-integer? list? (values node? node?))
  (top candidate clauses full-evaluations concrete-constants prior next-index more-general-predecessor)
  @{Advances the analysis in @racket[top], when the next conjunction up for unfolding or full evaluation is @racket[candidate],
 the knowledge base consists of concrete clauses @racket[clauses] and full evaluation rules @racket[full-evaluations].
 If a more general conjunction than that for @racket[candidate] exists earlier in the tree, it is supplied as @racket[more-general-predecessor].
 Otherwise, @racket[more-general-predecessor] should be @racket[#f].
 Returns two values: the updated candidate and the update top-level tree.}))