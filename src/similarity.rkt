; MIT License
;
; Copyright (c) 2016 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#lang at-exp racket

(require (only-in racket-list-utils/utils findf-index))
(require racket-tree-utils/src/tree)
(require racket-list-utils/utils)

(struct identity-constraint (arg-number) #:transparent)
(provide (struct-out identity-constraint))

(require "abstract-multi-domain.rkt")
(require "abstraction-inspection-utils.rkt")
(require "abstract-analysis.rkt")
(require "generational-tree.rkt")
(require (only-in "abstract-domain-ordering.rkt" renames?))
(require (only-in "data-utils.rkt" some-v))

(require racket/logging)
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (sa1-renames-sa2? branch i1 i2)
  (let* ([label-i1 (findf (λ (n) (equal? i1 (label-index n))) branch)]
         [label-i2 (findf (λ (n) (equal? i2 (label-index n))) branch)])
    (renames?
     (list-ref (label-conjunction label-i1) (some-v (label-selection label-i1)))
     (list-ref (label-conjunction label-i2) (some-v (label-selection label-i2))))))

(define (shortest-branch-with-indices indices t)
  (match indices
    [(list) (list)]
    [(list-rest i1 is)
     (let* ([t-label (node-label t)]
            [t-index (cond [(tree-label? t-label) (tree-label-index t-label)]
                           [(widening? t-label) (widening-index t-label)]
                           [else #f])]
            [rest-indices (if (equal? i1 t-index) is indices)])
       (if (not t-index)
           #f ; children will not have indices either
           (let ([child-result
                  (foldl
                   (λ (el acc) (if acc acc (shortest-branch-with-indices rest-indices el)))
                   #f
                   (node-children t))])
             (if child-result (cons t-label child-result) #f))))]
    [_ #f]))
(provide
 (proc-doc/names
  shortest-branch-with-indices
  (-> (listof exact-nonnegative-integer?) node? (listof (or/c tree-label? widening?)))
  (indices tree)
  @{Find the shortest branch in @racket[tree]
 which has nodes with all indices in @racket[indices], in the correct order.}))

(define (dp-zero-subtree-depth-complement-at-level dp gen-tree lvl)
  (define gen-tree-lvl-subforest (horizontal-level gen-tree lvl #t))
  (define
    dp-zero-index
    (findf-index
     (λ (t) (equal? (node-label t) (atom-with-generation dp 0)))
     gen-tree-lvl-subforest))
  (if
   dp-zero-index
   (list
    (list-ref gen-tree-lvl-subforest dp-zero-index)
    lvl
    (append
     (take gen-tree-lvl-subforest dp-zero-index)
     (drop gen-tree-lvl-subforest (+ 1 dp-zero-index))))
   #f))
(provide
 (proc-doc/names
  dp-zero-subtree-depth-complement-at-level
  (-> abstract-atom? node? exact-nonnegative-integer? (or/c #f list?))
  (dp generational-tree lvl)
  @{Find a subtree of the generational tree @racket[generational-tree] with an exact occurrence
 of abstract atom @racket[dp] of generation 0 at its root at the level @racket[lvl].
 Additionally, track the level and find the complement at this level.
 This assumes there is only one exact occurrence of @racket[dp].}))

(define (find-dp-zero-subtree-depth-complement dp gen-tree)
  (define gt-depth (node-depth gen-tree))
  (define levels (range 0 (+ gt-depth 1)))
  (foldl
   (λ (l acc) (if acc acc (dp-zero-subtree-depth-complement-at-level dp gen-tree l)))
   #f
   levels))
(provide
 (proc-doc/names
  find-dp-zero-subtree-depth-complement
  (-> abstract-atom? node? (or/c #f list?))
  (dp generational-tree)
  @{Find a subtree of the generational tree @racket[generational-tree] with an exact occurrence
 of abstract atom @racket[dp] of generation 0 at its root, along with the depth of this root and
 with subtrees at the same level in @racket[generational-tree], i.e. the complement at that level.
 This assumes that there is only one exact occurrence of @racket[dp].
 In case there is no exact occurrence of @racket[dp], the result is @racket[#f].}))

(define (context-and-ends-match subset-s1-with-gen subset-s2-with-gen dp-complement depth ls1 ls2)
  (define big-l1 (apply max (map atom-with-generation-generation subset-s1-with-gen)))
  (define ls1-1-dp
    (filter (λ (a-g) (equal? (atom-with-generation-generation a-g) 1)) subset-s1-with-gen))
  (define ls1-L-dp
    (filter (λ (a-g) (equal? (atom-with-generation-generation a-g) big-l1)) subset-s1-with-gen))
  ; FIXME is the following use of atom-with-generation causing an error?
  (define full-complement-at-ls1
    (apply
     append
     (map
      (compose atom-with-generation-atom (λ (comp) (horizontal-level comp (- ls1 depth))))
      dp-complement)))
  (define big-l2 (apply max (map atom-with-generation-generation subset-s2-with-gen)))
  (define ls2-1-dp
    (filter (λ (a-g) (equal? (atom-with-generation-generation a-g) 1)) subset-s2-with-gen))
  (define ls2-L-dp
    (filter (λ (a-g) (equal? (atom-with-generation-generation a-g) big-l2)) subset-s2-with-gen))
  (define full-complement-at-ls2
    (apply
     append
     (map
      (compose atom-with-generation-atom (λ (comp) (horizontal-level comp (- ls2 depth))))
      dp-complement)))
  (renames?
   (append ls1-1-dp ls1-L-dp full-complement-at-ls1)
   (append ls1-1-dp ls1-L-dp full-complement-at-ls1)))

; subtrees: dp subtree with generation 0 at given depth
(define (checks-involving-generations ls1 ls2 gs1 gs2 subtree-depth-complement)
  ; note that complement means all trees at the same depth as that for dp!
  (match subtree-depth-complement
    [(list subtree depth complement)
     (begin
       (define subset-s1-with-gen (horizontal-level subtree (- ls1 depth)))
       (define subset-s2-with-gen (horizontal-level subtree (- ls2 depth)))
       (and
        (begin (log-debug "checking three-generation-correspondence") #t)
        (three-generation-correspondence gs1 gs2 subset-s1-with-gen subset-s2-with-gen)
        (begin (log-debug "checking whether context and ends match") #t)
        (context-and-ends-match subset-s1-with-gen subset-s2-with-gen complement depth ls1 ls2)
        (begin (log-debug "checking whether invertible function f applies (or is not needed)") #t)
        (invertible-function-f-applies gs1 gs2 ls1 ls2 (cons subtree depth))
        (begin (log-debug "checking whether invertible function g applies (or is not needed)") #t)
        (invertible-function-g-applies gs1 gs2 ls1 ls2 (cons subtree depth))))]))

(define (last-gen-renaming ls1 ls2 subtree depth)
  (define max-gen-1 (max-gen subtree (- ls1 depth)))
  (define max-gen-2 (max-gen subtree (- ls2 depth)))
  (renames?
   ((atoms-of-generation max-gen-1) (horizontal-level subtree (- ls1 depth)))
   ((atoms-of-generation max-gen-2) (horizontal-level subtree (- ls2 depth)))))

(define (invertible-function-f-applies ls1 ls2 gs1 gs2 subtree-and-depth)
  (or (< gs1 3)
      (let ([f-mapping (extract-f-mapping ls1 subtree-and-depth)])
        (applies-until-gs f-mapping ls1 ls2 gs1 gs2 subtree-and-depth))))
(provide invertible-function-f-applies)

(define (extract-f-mapping ls subtree-and-depth)
  (subsequent-gen-mapping ls subtree-and-depth 1))
(provide
 (proc-doc/names
  extract-f-mapping
  (-> exact-nonnegative-integer? (cons/c node? exact-nonnegative-integer?) list?)
  (ls subtree-and-depth)
  @{Extract a mapping describing the relation between subsequent conjunctions of different
 generations with respect to the genealogical tree, after the selected atom.
 The argument @racket[ls] represents the level of the top-level genealogical tree to which the
 selected atom belongs.
 The argument @racket[subtree-and-depth] is a pair consisting of a subtree with a target atom
 at its root, and the depth at which this subtree is found in the top-level genealogical tree.}))

(define (atoms-of-generation gen)
  (compose
   (curry map atom-with-generation-atom)
   (curry filter (compose (curry equal? gen) atom-with-generation-generation))))

(define (subsequent-gen-mapping ls subtree-and-depth i)
  (define subtree (car subtree-and-depth))
  (define horizontal-reading (horizontal-level subtree (- ls (cdr subtree-and-depth))))
  (define gen-i-plus-one ((atoms-of-generation (+ i 1)) horizontal-reading))
  (define gen-i ((atoms-of-generation i) horizontal-reading))
  (define gen-i-plus-one-args (apply append (map abstract-atom-args gen-i-plus-one)))
  (define gen-i-args (apply append (map abstract-atom-args gen-i)))
  (map (λ (x) (if (and (abstract-variable? x) (findf-index (λ (pa) (equal? x pa)) gen-i-plus-one-args)) (identity-constraint (+ 1 (findf-index (λ (pa) (equal? x pa)) gen-i-plus-one-args))) 'fresh)) gen-i-args))

(define (extract-g-mapping ls subtree-and-depth max-gen-1)
  (subsequent-gen-mapping ls subtree-and-depth (- max-gen-1 2)))
(provide
 (proc-doc/names
  extract-g-mapping
  (-> exact-nonnegative-integer? (cons/c node? exact-nonnegative-integer?) exact-nonnegative-integer? list?)
  (ls subtree-and-depth max-gen-1)
  @{Extract a mapping describing the relation between subsequent conjunctions of different
 generations with respect to the genealogical tree, before the selected atom.
 The mapping describes how each argument of a conjunction of a particular generation is linked
 to some argument of the next generation.
 The argument @racket[ls] represents the level of the top-level genealogical tree to which the
 selected atom belongs.
 The argument @racket[subtree-and-depth] is a pair consisting of a subtree with a target atom
 at its root, and the depth at which this subtree is found in the top-level genealogical tree.
 The argument @racket[max-gen-1] gives the maximum generation at level @racket[ls].}))

(define (invertible-function-g-applies gs1 gs2 ls1 ls2 subtree-and-depth)
  (define subtree (car subtree-and-depth))
  (define max-gen-1 (max-gen subtree (- ls1 (cdr subtree-and-depth))))
  (or (> gs1 (- max-gen-1 3))
      (let ([g-mapping (extract-g-mapping ls1 subtree max-gen-1)])
        (applies-beyond-gs g-mapping ls1 ls2 gs1 gs2 subtree-and-depth))))

(define (max-gen gen-tree level)
  (apply max (horizontal-level (node-map atom-with-generation-generation gen-tree) level)))

(define (applies-in-range mapping ls1 ls2 gs1 gs2 from-start subtree-and-depth)
  (define subtree (car subtree-and-depth))
  (define subtree-depth (cdr subtree-and-depth))
  (define h-level-1 (horizontal-level subtree (- ls1 subtree-depth)))
  (define h-level-2 (horizontal-level subtree (- ls2 subtree-depth)))
  (define atoms-gens-level-1
    (group-by atom-with-generation-generation h-level-1))
  (define atoms-gens-level-2
    (group-by atom-with-generation-generation h-level-2))
  (define max-gen-1 (max-gen subtree (- ls1 subtree-depth)))
  (define max-gen-2 (max-gen subtree (- ls2 subtree-depth)))
  (define relevant-groups-level-1
    (filter
     (λ (group)
       (and (>= (atom-with-generation-generation (car group)) (if from-start 1 (+ gs1 1)))
            (< (atom-with-generation-generation (car group)) (if from-start gs1 (- max-gen-1 1)))))
     atoms-gens-level-1))
  (define relevant-groups-level-2
    (filter
     (λ (group)
       (and (>= (atom-with-generation-generation (car group)) (if from-start 1 (+ gs2 1)))
            (< (atom-with-generation-generation (car group)) (if from-start gs2 (- max-gen-2 1)))))
     atoms-gens-level-2))
  (and (andmap
        (curry mapping-applies mapping)
        (map (curry map atom-with-generation-atom) (drop-gen-group relevant-groups-level-1 (- gs1 1)))
        (map (curry map atom-with-generation-atom) (drop-gen-group relevant-groups-level-1 1)))
       (andmap
        (curry mapping-applies mapping)
        (map (curry map atom-with-generation-atom) (drop-gen-group relevant-groups-level-2 (- gs2 1)))
        (map (curry map atom-with-generation-atom) (drop-gen-group relevant-groups-level-2 1)))))

(define (drop-gen-group lst gen)
  (foldr
   (λ (g acc) (if (equal? (atom-with-generation-generation (car g)) gen) acc (cons g acc)))
   '()
   lst))

(define (applies-until-gs f-mapping ls1 ls2 gs1 gs2 subtree-and-depth)
  (applies-in-range f-mapping ls1 ls2 gs1 gs2 #t subtree-and-depth))

(define (mapping-applies mapping gen-i gen-i-plus-one)
  (define gen-i-args (apply append (map abstract-atom-args gen-i)))
  (define gen-i-plus-one-args (apply append (map abstract-atom-args gen-i-plus-one)))
  (and (renames? gen-i gen-i-plus-one)
       (andmap (λ (constraint idx)
                 (cond [(equal? constraint 'fresh)
                        ((compose not contains-subterm?)
                         gen-i-plus-one
                         (list-ref gen-i-args (- idx 1)))]
                       [(identity-constraint? constraint)
                        (equal? (list-ref gen-i-plus-one-args (- (identity-constraint-arg-number constraint) 1))
                                (list-ref gen-i-args (- idx 1)))]))
               mapping
               (range 1 (+ (length mapping) 1)))))

(define (applies-beyond-gs g-mapping ls1 ls2 gs1 gs2 subtree-and-depth)
  (applies-in-range g-mapping ls1 ls2 gs1 gs2 #f subtree-and-depth))

(define (three-generation-correspondence gs1 gs2 subset-s1-with-gen subset-s2-with-gen)
  (define
    three-generation-conjunction-1
    (filter
     (λ (a-g) (<= (- gs1 1) (atom-with-generation-generation a-g) (+ gs1 1)))
     subset-s1-with-gen))
  (define
    three-generation-conjunction-2
    (filter
     (λ (a-g) (<= (- gs2 1) (atom-with-generation-generation a-g) (+ gs2 1)))
     subset-s2-with-gen))
  (and
   (renames?
    (map atom-with-generation-atom three-generation-conjunction-1)
    (map atom-with-generation-atom three-generation-conjunction-2))
   (equal?
    (map (compose (curry - gs1) atom-with-generation-generation) three-generation-conjunction-1)
    (map (compose (curry - gs2) atom-with-generation-generation) three-generation-conjunction-2))))
(provide
 (proc-doc/names
  three-generation-correspondence
  (-> exact-nonnegative-integer? exact-nonnegative-integer? (listof atom-with-generation?) (listof atom-with-generation?) boolean?)
  (gs1 gs2 level-1 level-2)
  @{Checks whether the selected generation @racket[gs1] at level @racket[level-1] (of a genealogical tree),
 along with the the preceding generation and the following generation consitutes a renamed instance of @racket[gs2]
 (and surrounding generations) at level @racket[level-2].}))

(define (s-similar? node-index-1 node-index-2 tree)
  (log-debug "checking for s-similarity")
  (define branch (shortest-branch-with-indices (list node-index-1 node-index-2) tree))
  (define skeleton (car (generational-tree-skeleton branch))) ; top-level is an atom anyway
  (define candidate-targets (candidate-target-atoms skeleton (- (length branch) 1)))
  (define all-generational-trees (generational-trees branch))
  (define ls1 (findf-index (λ (l) (equal? (label-index l) node-index-1)) branch))
  (define ls2 (findf-index (λ (l) (equal? (label-index l) node-index-2)) branch))
  (log-debug
   (format "selected atom 1 renames selected atom 2: ~a" (sa1-renames-sa2? branch node-index-1 node-index-2)))
  (and
   (sa1-renames-sa2? branch node-index-1 node-index-2)
   (ormap ; s-similarity for any candidate target atom is enough
    (λ (dp gt)
      (let* ([annotated-s1 (horizontal-level gt ls1)]
             [annotated-s2 (horizontal-level gt ls2)]
             [dp-zero-subtree-depth-complement
              (find-dp-zero-subtree-depth-complement dp gt)]
             [gs1
              (atom-with-generation-generation
               (list-ref annotated-s1 (some-v (label-selection (list-ref branch ls1)))))]
             [gs2
              (atom-with-generation-generation
               (list-ref annotated-s2 (some-v (label-selection (list-ref branch ls2)))))])
        (checks-involving-generations ls1 ls2 gs1 gs2 dp-zero-subtree-depth-complement)))
    candidate-targets
    all-generational-trees)))
(provide
 (proc-doc/names
  s-similar?
  (->
   exact-positive-integer?
   exact-positive-integer?
   node?
   boolean?)
  (node-index-1 node-index-2 analysis-tree)
  @{Determines whether the node with index @racket[node-index-2] is s-similar
 to the node with index @racket[node-index-1] in @racket[analysis-tree].
 The caller is responsible for checking that tree has a branch with the supplied indices.}))
