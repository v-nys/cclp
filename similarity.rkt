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
(require racket-tree-utils/src/tree (only-in racket-tree-utils/src/printer tree-display))
(require racket-list-utils/utils)

(struct identity-constraint (arg-number) #:transparent)
(provide (struct-out identity-constraint))

(require "abstract-multi-domain.rkt")
(require "abstraction-inspection-utils.rkt")
(require "abstract-analysis.rkt")
(require "execution.rkt")
(require "generational-tree.rkt")
(require (only-in "abstract-domain-ordering.rkt" renames?))
(require (only-in "data-utils.rkt" some-v))

(require racket/logging)
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (sa1-renames-sa2? branch i1 i2-or-conjunction i2-index)
  (let* ([label-i1 (findf (λ (n) (equal? i1 (label-index n))) branch)]
         [label-i2 (findf (λ (n) (equal? i2-or-conjunction (label-index n))) branch)])
    (renames?
     (list-ref (label-conjunction label-i1) (some-v (label-selection label-i1)))
     (if
      label-i2
      (list-ref (label-conjunction label-i2) i2-index)
      (list-ref i2-or-conjunction i2-index)))))

(define (shortest-branch-containing index1 index2-or-conjunction t)
  (define (reversed-shortest-branch-ending-in ending t acc)
    (match t
      [(node l ch)
       #:when (and (label-with-conjunction? l)
                   (or (equal? (label-conjunction l) ending) (equal? (label-index l) ending)))
       (cons l acc)]
      [(node l ch)
       #:when (label-with-conjunction? l)
       (foldl (λ (c acc2) (if acc2 acc2 (reversed-shortest-branch-ending-in ending c (cons l acc))))
              #f
              ch)]
      [_ #f]))
  (match t
    [(node l ch)
     #:when (and (label-with-conjunction? l) (equal? (label-index l) index1))
     (let ([tail (foldl (λ (c acc) (if acc acc (reversed-shortest-branch-ending-in index2-or-conjunction c '()))) #f ch)])
       (if tail (cons l (reverse tail)) #f))]
    [(node l ch)
     #:when (label-with-conjunction? l)
     (foldl
      (λ (c acc)
        (if acc acc (let ([rooted-at-child (shortest-branch-containing index1 index2-or-conjunction c)]) (if rooted-at-child (cons l rooted-at-child) #f))))
      #f
      ch)]
    [_ #f]))
(provide
 (proc-doc/names
  shortest-branch-containing
  (-> exact-nonnegative-integer? (or/c exact-nonnegative-integer? (listof abstract-atom?)) node? (or/c #f (listof (or/c tree-label? widening?))))
  (index1 index2-or-conjunction tree)
  @{Find the shortest branch in @racket[tree] which contains a node with index @racket[index1]
 and ends with a node with index or conjunction @racket[index2-or-conjunction].
 The branch alsways starts at the root of the tree.
 If there is no such branch, the result is @racket[#f].}))

(define (dp-zero-subtree-depth-complement-at-level dp gen-tree lvl)
  (define gen-tree-lvl-subforest (horizontal-level gen-tree lvl #t))
  (define
    dp-zero-index
    (findf-index
     (λ (t) (equal? (node-label t) (identified-atom-with-generation dp 0)))
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
  (-> identified-atom? node? exact-nonnegative-integer? (or/c #f list?))
  (dp generational-tree lvl)
  @{Find a subtree of the generational tree @racket[generational-tree] with
 the uniquely identified atom @racket[dp] of generation 0 at its root at the level @racket[lvl].
 Additionally, track the level and find the complement at this level.}))

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
  (-> identified-atom? node? (or/c #f list?))
  (dp generational-tree)
  @{Find a subtree of the generational tree @racket[generational-tree] with the uniquely identified
 atom @racket[dp] of generation 0 at its root, along with the depth of this root and
 with subtrees at the same level in @racket[generational-tree], i.e. the complement at that level.
 This assumes that there is only one exact occurrence of @racket[dp].
 In case there is no exact occurrence of @racket[dp], the result is @racket[#f].}))

(define (context-and-ends-match subset-s1-with-gen subset-s2-with-gen dp-complement depth ls1 ls2)
  (define big-l1 (apply max (map identified-atom-with-generation-generation subset-s1-with-gen)))
  (define ls1-1-dp
    (map identified-atom-with-generation-id-atom (filter (λ (a-g) (equal? (identified-atom-with-generation-generation a-g) 1)) subset-s1-with-gen)))
  (define ls1-L-dp
    (map identified-atom-with-generation-id-atom (filter (λ (a-g) (equal? (identified-atom-with-generation-generation a-g) big-l1)) subset-s1-with-gen)))
  (define full-complement-at-ls1
    (apply
     append
     (map
      (compose (curry map identified-atom-with-generation-id-atom) (λ (comp) (horizontal-level comp (- ls1 depth))))
      dp-complement)))
  (define big-l2 (apply max (map identified-atom-with-generation-generation subset-s2-with-gen)))
  (define ls2-1-dp
    (map identified-atom-with-generation-id-atom (filter (λ (a-g) (equal? (identified-atom-with-generation-generation a-g) 1)) subset-s2-with-gen)))
  (define ls2-L-dp
    (map identified-atom-with-generation-id-atom (filter (λ (a-g) (equal? (identified-atom-with-generation-generation a-g) big-l2)) subset-s2-with-gen)))
  (define full-complement-at-ls2
    (apply
     append
     (map
      (compose (curry map identified-atom-with-generation-id-atom) (λ (comp) (horizontal-level comp (- ls2 depth))))
      dp-complement)))
  (renames?
   (map identified-atom-atom (append ls1-1-dp ls1-L-dp full-complement-at-ls1))
   (map identified-atom-atom (append ls1-1-dp ls1-L-dp full-complement-at-ls1))))

(define (minimal-maximum-generation-check level1 level2)
  (let ([max-gen1 (apply max (map identified-atom-with-generation-generation level1))]
        [max-gen2 (apply max (map identified-atom-with-generation-generation level2))])
    (and (>= max-gen1 3)
         (> max-gen2 max-gen1))))

(define (checks-involving-generations ls1 ls2 gs1 gs2 subtree-depth-complement)
  ; note that complement means all trees at the same depth as that for dp!
  (match subtree-depth-complement
    [(list subtree depth complement)
     (begin
       (log-debug "ls1 is ~v" ls1)
       (log-debug "ls2 is ~v" ls2)
       (log-debug "depth is ~v" depth) ; this depth is that at which dp starts
       (define subset-s1-with-gen (horizontal-level subtree (- ls1 depth)))
       (define subset-s2-with-gen (horizontal-level subtree (- ls2 depth)))
       (and
        ;(minimal-maximum-generation-check subset-s1-with-gen subset-s2-with-gen)
        (three-generation-correspondence gs1 gs2 subset-s1-with-gen subset-s2-with-gen)
        (context-and-ends-match subset-s1-with-gen subset-s2-with-gen complement depth ls1 ls2)
        (invertible-function-f-applies gs1 gs2 ls1 ls2 (cons subtree depth))
        (invertible-function-g-applies gs1 gs2 ls1 ls2 (cons subtree depth))))]))

(define (invertible-function-f-applies gs1 gs2 ls1 ls2 subtree-and-depth)
  (or (< gs1 3)
      (let ([f-mapping (extract-f-mapping ls1 subtree-and-depth)])
        (applies-until-gs f-mapping ls1 ls2 gs1 gs2 subtree-and-depth))))
(provide invertible-function-f-applies)

(define (extract-f-mapping ls subtree-and-depth)
  (consecutive-gen-mapping ls subtree-and-depth 1))
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
   (curry map identified-atom-with-generation-id-atom)
   (curry filter (compose (curry equal? gen) identified-atom-with-generation-generation))))

(define (consecutive-gen-mapping ls subtree-and-depth i)
  (define subtree (car subtree-and-depth))
  (define horizontal-reading (horizontal-level subtree (- ls (cdr subtree-and-depth))))
  (define gen-i-plus-one (map identified-atom-atom ((atoms-of-generation (+ i 1)) horizontal-reading)))
  (define gen-i (map identified-atom-atom ((atoms-of-generation i) horizontal-reading)))
  (define gen-i-plus-one-args (apply append (map abstract-atom-args gen-i-plus-one)))
  (define gen-i-args (apply append (map abstract-atom-args gen-i)))
  (map (λ (x) (if (and (abstract-variable? x) (findf-index (λ (pa) (equal? x pa)) gen-i-plus-one-args)) (identity-constraint (+ 1 (findf-index (λ (pa) (equal? x pa)) gen-i-plus-one-args))) 'fresh)) gen-i-args))

(define (extract-g-mapping ls subtree-and-depth max-gen-1)
  (consecutive-gen-mapping ls subtree-and-depth (- max-gen-1 2)))
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
      (let ([g-mapping (extract-g-mapping ls1 subtree-and-depth max-gen-1)])
        (applies-beyond-gs g-mapping ls1 ls2 gs1 gs2 subtree-and-depth))))

(define (max-gen gen-tree level)
  (apply max (horizontal-level (node-map identified-atom-with-generation-generation gen-tree) level)))

(define (applies-in-range mapping ls1 ls2 gs1 gs2 from-start subtree-and-depth)
  (log-debug "checking whether mapping ~v applies (from start: ~v)" mapping from-start)
  (define subtree (car subtree-and-depth))
  (define subtree-depth (cdr subtree-and-depth))
  (define h-level-1 (horizontal-level subtree (- ls1 subtree-depth)))
  (define h-level-2 (horizontal-level subtree (- ls2 subtree-depth)))
  (define atoms-gens-level-1
    (group-by identified-atom-with-generation-generation h-level-1))
  (define atoms-gens-level-2
    (group-by identified-atom-with-generation-generation h-level-2))
  (define max-gen-1 (max-gen subtree (- ls1 subtree-depth)))
  (define max-gen-2 (max-gen subtree (- ls2 subtree-depth)))
  (define relevant-groups-level-1
    (filter
     (λ (group)
       (and (>= (identified-atom-with-generation-generation (car group)) (if from-start 1 (+ gs1 1)))
            (< (identified-atom-with-generation-generation (car group)) (if from-start gs1 (- max-gen-1 1)))))
     atoms-gens-level-1))
  (define relevant-groups-level-2
    (filter
     (λ (group)
       (and (>= (identified-atom-with-generation-generation (car group)) (if from-start 1 (+ gs2 1)))
            (< (identified-atom-with-generation-generation (car group)) (if from-start gs2 (- max-gen-2 1)))))
     atoms-gens-level-2))
  (and (andmap
        (curry mapping-applies mapping)
        (map (curry map (compose identified-atom-atom identified-atom-with-generation-id-atom)) (drop-gen-group relevant-groups-level-1 (if from-start (- gs1 1) (- max-gen-1 2))))
        (map (curry map (compose identified-atom-atom identified-atom-with-generation-id-atom)) (drop-gen-group relevant-groups-level-1 (if from-start 1 (+ gs1 1)))))
       (andmap
        (curry mapping-applies mapping)
        (map (curry map (compose identified-atom-atom identified-atom-with-generation-id-atom)) (drop-gen-group relevant-groups-level-2 (if from-start (- gs2 1) (- max-gen-2 2))))
        (map (curry map (compose identified-atom-atom identified-atom-with-generation-id-atom)) (drop-gen-group relevant-groups-level-2 (if from-start 1 (+ gs2 1)))))))

(define (drop-gen-group lst gen)
  (foldr
   (λ (g acc) (if (equal? (identified-atom-with-generation-generation (car g)) gen) acc (cons g acc)))
   '()
   lst))

(define (applies-until-gs f-mapping ls1 ls2 gs1 gs2 subtree-and-depth)
  (applies-in-range f-mapping ls1 ls2 gs1 gs2 #t subtree-and-depth))

(define (mapping-applies mapping gen-i gen-i-plus-one)
  (log-debug "checking mapping between ~v and ~v" gen-i gen-i-plus-one)
  (define gen-i-args (apply append (map abstract-atom-args gen-i)))
  (define gen-i-plus-one-args (apply append (map abstract-atom-args gen-i-plus-one)))
  ; may be able to strengthen the requirement to renaming for all but the first instance of the mapping
  ; (see primes, nodes 24 and 35, when plus is fully evaluated and the first filter is more instantiated than the rest)
  (and (equal? (map abstract-atom-symbol gen-i) (map abstract-atom-symbol gen-i-plus-one))
       (equal? (length gen-i-args) (length gen-i-plus-one-args))
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
     (λ (a-g) (<= (- gs1 1) (identified-atom-with-generation-generation a-g) (+ gs1 1)))
     subset-s1-with-gen))
  (define
    three-generation-conjunction-2
    (filter
     (λ (a-g) (<= (- gs2 1) (identified-atom-with-generation-generation a-g) (+ gs2 1)))
     subset-s2-with-gen))
  (and
   (renames?
    (map (compose identified-atom-atom identified-atom-with-generation-id-atom) three-generation-conjunction-1)
    (map (compose identified-atom-atom identified-atom-with-generation-id-atom) three-generation-conjunction-2))
   (equal?
    (map (compose (curry - gs1) identified-atom-with-generation-generation) three-generation-conjunction-1)
    (map (compose (curry - gs2) identified-atom-with-generation-generation) three-generation-conjunction-2))))
(provide
 (proc-doc/names
  three-generation-correspondence
  (-> exact-nonnegative-integer? exact-nonnegative-integer? (listof identified-atom-with-generation?) (listof identified-atom-with-generation?) boolean?)
  (gs1 gs2 level-1 level-2)
  @{Checks whether the selected generation @racket[gs1] at level @racket[level-1] (of a genealogical tree),
 along with the the preceding generation and the following generation consitutes a renamed instance of @racket[gs2]
 (and surrounding generations) at level @racket[level-2].}))

(define (s-similar? node-index-1 node-index-2-or-abstract-conjunction index-2-selection tree)
  (define branch (shortest-branch-containing node-index-1 node-index-2-or-abstract-conjunction tree))
  (define skeleton (if branch (car (generational-tree-skeleton branch)) #f))
  (define candidate-targets (if branch (candidate-target-identified-atoms skeleton (- (length branch) 1)) #f))
  (define all-generational-trees (if branch (generational-trees branch) #f))
  (define ls1 (if branch (findf-index (λ (l) (equal? (label-index l) node-index-1)) branch) #f))
  (define ls2
    (if branch
        (findf-index
         (λ (l) (or (equal? (label-index l) node-index-2-or-abstract-conjunction) (equal? (label-conjunction l) node-index-2-or-abstract-conjunction))) branch) #f))
  (if branch
      (and
       (sa1-renames-sa2? branch node-index-1 node-index-2-or-abstract-conjunction index-2-selection)
       (ormap ; s-similarity for any candidate target atom is enough
        (λ (dp gt)
          (let* ([annotated-s1 (horizontal-level gt ls1)]
                 [annotated-s2 (horizontal-level gt ls2)]
                 [dp-zero-subtree-depth-complement
                  (find-dp-zero-subtree-depth-complement dp gt)]
                 [gs1
                  (identified-atom-with-generation-generation
                   (list-ref annotated-s1 (some-v (label-selection (list-ref branch ls1)))))]
                 [gs2
                  (identified-atom-with-generation-generation
                   (list-ref annotated-s2 index-2-selection))])
            (if (<= ls1 (second dp-zero-subtree-depth-complement)) #f (checks-involving-generations ls1 ls2 gs1 gs2 dp-zero-subtree-depth-complement))))
        candidate-targets
        all-generational-trees)) #f))
(provide
 (proc-doc/names
  s-similar?
  (->
   exact-positive-integer?
   (or/c (listof abstract-atom?) exact-positive-integer?)
   exact-nonnegative-integer?
   node?
   boolean?)
  (node-index-1 node-index-2 index-2-selection analysis-tree)
  @{Determines whether the node with index @racket[node-index-2] is s-similar
 to the node with index @racket[node-index-1] in @racket[analysis-tree].
 The caller is responsible for checking that tree has a branch with the supplied indices and that
 @racket[index-2-selection] is the index of the selected atom in the second conjunction.}))

(module+ test
  (require rackunit "cclp-interpreter.rkt")

  (define (horizontal-level-bp . strings-ids-and-levels)
    (match strings-ids-and-levels
      [(list) (list)]
      [(list-rest h1 h2 h3 t)
       (cons
        (identified-atom-with-generation (identified-atom (interpret-abstract-atom h1) h2) h3)
        (apply horizontal-level-bp t))]))

  (test-case
   "finding level-0 instances of a target, and their complement, at a particular level"
   (check-equal?
    (dp-zero-subtree-depth-complement-at-level
     (identified-atom (abstract-atom 'dp '()) 2)
     (generational-tree-bp
      (a 1 0
         (dp 2 0
             (f 4 1)
             (g 5 1))
         (b 3 0
            (c 6 0)
            (dp 7 0
                (f 8 1)
                (g 9 1)))))
     1)
    (list
     (generational-tree-bp
      (dp 2 0
          (f 4 1)
          (g 5 1)))
     1
     (list
      (generational-tree-bp
       (b 3 0
          (c 6 0)
          (dp 7 0
              (f 8 1)
              (g 9 1))))))))

  (test-case
   "finding subtrees which begin with a particular atom, as well as their depth and complement"
   (check-equal?
    (find-dp-zero-subtree-depth-complement
     (identified-atom (abstract-atom 'dp '()) 2)
     (generational-tree-bp
      (a 1 0
         (dp 2 0
             (f 4 1)
             (g 5 1))
         (b 3 0
            (c 6 0)
            (dp 7 0
                (f 8 1)
                (g 9 1))))))
    (list
     (generational-tree-bp
      (dp 2 0
          (f 4 1)
          (g 5 1)))
     1
     (list
      (generational-tree-bp
       (b 3 0
          (c 6 0)
          (dp 7 0
              (f 8 1)
              (g 9 1)))))))
   (check-equal?
    (find-dp-zero-subtree-depth-complement
     (identified-atom (abstract-atom 'dp '()) 1)
     (generational-tree-bp
      (dp 1 0
          (a 2 1)
          (b 3 1))))
    (list
     (generational-tree-bp
      (dp 1 0
          (a 2 1)
          (b 3 1)))
     0
     (list)))
   (check-equal?
    (find-dp-zero-subtree-depth-complement
     (identified-atom (abstract-atom 'dp '()) 1)
     (generational-tree-bp
      (a 2 0)))
    #f)
   (check-equal?
    (find-dp-zero-subtree-depth-complement
     (identified-atom (abstract-atom 'dp '()) 1)
     (generational-tree-bp
      (a 2 0
         (b 3 0)
         (c 4 0))))
    #f))

  (let* ([bottom-left (node (cycle 1) '())]
         [bottom-right (node (tree-label (list) #f (list) #f #f) '())]
         [near-bottom-right-contents
          (widening (interpret-abstract-conjunction "b(α1)") #f "test" 3)]
         [near-bottom-right (node near-bottom-right-contents (list bottom-right))]
         [near-top-right-contents
          (tree-label (interpret-abstract-conjunction "b(γ1)") 0 (list) #f 2)]
         [near-top-right (node near-top-right-contents (list near-bottom-right))]
         [top-contents
          (widening (interpret-abstract-conjunction "a(α1)") #f "test" 1)]
         [top (node top-contents (list bottom-left near-top-right))])
    (check-equal?
     (shortest-branch-containing 1 3 top)
     (list top-contents near-top-right-contents near-bottom-right-contents)))

  (test-case
   "extracting the invertible function f from two related conjunctions"
   (check-equal?
    (extract-f-mapping
     3
     (cons
      (generational-tree-bp
       (collect
        ((g 1) (a 1))
        1
        0
        (collect
         ((g 2) (a 2))
         2
         1
         (collect
          ((g 4) (a 4))
          5
          2
          (collect
           ((g 6) (a 6))
           10
           3)
          (collect
           ((g 7) (a 7))
           11
           3)
          (append
           ((a 6) (a 7) (a 4))
           12
           3))
         (collect
          ((g 5) (a 5))
          6
          2
          (collect
           ((g 5) (a 5))
           13
           2))
         (append
          ((a 4) (a 5) (a 2))
          7
          2
          (append
           ((a 4) (a 5) (a 2))
           14
           2)))
        (collect
         ((g 3) (a 3))
         3
         1
         (collect
          ((g 3) (a 3))
          8
          1
          (collect
           ((g 3) (a 3))
           15
           1)))
        (append
         ((a 2) (a 3) (a 1))
         4
         1
         (append
          ((a 2) (a 3) (a 1))
          9
          1
          (append
           ((a 2) (a 3) (a 1))
           16
           1)))))
      0))
    (list 'fresh 'fresh (identity-constraint 5) 'fresh 'fresh)))

  (test-case
   "checking whether f expresses exactly the desired mapping, across all related conjunctions"
   (check-equal?
    (invertible-function-f-applies
     3
     4
     3
     4
     (cons
      (generational-tree-bp
       (collect
        ((g 1) (a 1))
        1
        0
        (collect
         ((g 2) (a 2))
         2
         1
         (collect
          ((g 3) (a 3))
          5
          2
          (collect
           ((g 4) (a 4))
           10
           3
           (collect ((g 5) (a 5)) 17 4)
           (collect ((g 6) (a 6)) 18 4)
           (append ((a 5) (a 6) (a 4)) 19 4))
          (collect
           ((g 7) (a 7))
           11
           3
           (collect ((g 7) (a 7)) 20 3))
          (append
           ((a 4) (a 7) (a 3))
           12
           3
           (append ((a 4) (a 7) (a 3)) 21 3)))
         (collect
          ((g 8) (a 8))
          6
          2
          (collect
           ((g 8) (a 8))
           13
           2
           (collect ((g 8) (a 8)) 22 2)))
         (append
          ((a 3) (a 8) (a 2))
          7
          2
          (append
           ((a 3) (a 8) (a 2))
           14
           2
           (append ((a 3) (a 8) (a 2)) 23 2))))
        (collect
         ((g 9) (a 9))
         3
         1
         (collect
          ((g 9) (a 9))
          8
          1
          (collect
           ((g 9) (a 9))
           15
           1
           (collect ((g 9) (a 9)) 24 1))))
        (append
         ((a 2) (a 9) (a 1))
         4
         1
         (append
          ((a 2) (a 9) (a 1))
          9
          1
          (append
           ((a 2) (a 9) (a 1))
           16
           1
           (append ((a 2) (a 9) (a 1)) 25 1))))))
      0))
    #t)
 
   (check-equal?
    (invertible-function-f-applies
     3
     4
     3
     4
     (cons
      (generational-tree-bp
       (collect
        ((g 1) (a 1))
        1
        0
        (collect
         ((g 2) (a 2))
         2
         1
         (collect
          ((g 3) (a 3))
          5
          2
          (collect
           ((g 4) (a 4))
           10
           3
           (collect ((g 5) (a 5)) 17 4)
           (collect ((g 6) (a 6)) 18 4)
           (append ((a 5) (a 6) (a 4)) 19 4))
          (collect
           ((g 7) (a 7))
           11
           3
           (collect ((g 7) (a 7)) 20 3))
          (append
           ((a 4) (a 7) (a 3))
           12
           3
           (append ((a 4) (a 7) (a 3)) 21 3)))
         (collect
          ((g 8) (a 8))
          6
          2
          (collect
           ((g 8) (a 8))
           13
           2
           (collect ((g 8) (a 8)) 22 2)))
         (append
          ; not properly aliased with gen 3
          ((a 1000) (a 8) (a 2))
          7
          2
          (append
           ((a 3) (a 8) (a 2))
           14
           2
           (append ((a 3) (a 8) (a 2)) 23 2))))
        (collect
         ((g 9) (a 9))
         3
         1
         (collect
          ((g 9) (a 9))
          8
          1
          (collect
           ((g 9) (a 9))
           15
           1
           (collect ((g 9) (a 9)) 24 1))))
        (append
         ((a 2) (a 9) (a 1))
         4
         1
         (append
          ((a 2) (a 9) (a 1))
          9
          1
          (append
           ((a 2) (a 9) (a 1))
           16
           1
           (append ((a 2) (a 9) (a 1)) 25 1))))))
      0))
    #t))

  (test-case
   "checking for correspondence between generations around and including selected one"
   (check-equal?
    (three-generation-correspondence
     2
     3
     (horizontal-level-bp
      "integers(γ1,α1)" 1 0
      "filter(γ2,α1,α2)" 2 1
      "filter(γ3,[γ4|α2],α3)" 3 2
      "filter(γ5,α3,α4)" 4 3
      "sift(α4,α5)" 5 3
      "length(α5,γ6)" 6 0)
     (horizontal-level-bp
      "integers(γ1,α1)" 7 0
      "filter(γ2,α1,α2)" 8 1
      "filter(γ3,α2,α3)" 9 2
      "filter(γ4,[γ5|α3],α4)" 10 3
      "filter(γ6,α4,α5)" 11 4
      "sift(α5,α6)" 12 4
      "length(α6,γ7)" 13 0))
    #t)
   (check-equal?
    (three-generation-correspondence
     1
     1
     (horizontal-level-bp
      "integers(γ1,α1)" 1 0
      "filter(γ2,[γ3|α1],α2)" 2 1
      "filter(γ4,α2,α3)" 3 2
      "filter(γ5,α3,α4)" 4 3
      "sift(α4,α5)" 5 3
      "length(α5,γ6)" 6 0)
     (horizontal-level-bp
      "integers(γ1,α1)" 7 0
      "filter(γ2,[γ3|α1],α2)" 8 1
      "filter(γ4,α2,α3)" 9 2
      "filter(γ5,α3,α4)" 10 3
      "filter(γ6,α4,α5)" 11 4
      "sift(α5,α6)" 12 4
      "length(α6,γ7)" 13 0))
    #t)
   (check-equal?
    (three-generation-correspondence
     1
     1
     (horizontal-level-bp
      "integers(γ1,α1)" 1 0
      "filter(γ2,[γ3|α1],α2)" 2 1
      "filter(γ4,α2,α3)" 3 2
      "filter(γ5,α3,α4)" 4 3
      "sift(α4,α5)" 5 3
      "length(α5,γ6)" 6 0)
     (horizontal-level-bp
      "integers(γ1,α1)" 7 0
      "filter(γ2,[γ3|α1],α2)" 8 1
      "filter(γ4,α2,α3)" 9 2
      "filter(γ5,α3,α4)" 10 3
      "filter(γ6,α4,α5)" 11 4
      "sift(α5,α6)" 12 4
      "length(γ6,γ7)" 13 0)) ; difference in preceding generation
    #f)
   (check-equal?
    (three-generation-correspondence
     1
     1
     (horizontal-level-bp
      "integers(γ1,α1)" 1 0
      "filter(γ2,[γ3|α1],α2)" 2 1
      "filter(γ4,α2,α3)" 3 2
      "filter(γ5,α3,α4)" 4 3
      "sift(α4,α5)" 5 3
      "length(α5,γ6)" 6 0)
     (horizontal-level-bp
      "integers(γ1,α1)" 7 0
      "filter(γ2,[γ3|α1],α2)" 8 1
      "filter(γ4,α2,γ3)" 9 2 ; difference in subsequent generation
      "filter(γ5,α3,α4)" 10 3
      "filter(γ6,α4,α5)" 11 4
      "sift(α5,α6)" 12 4
      "length(α6,γ7)" 13 0))
    #f)))