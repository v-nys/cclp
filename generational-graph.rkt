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
(require
  scribble/srcdoc
  graph
  racket-list-utils/utils
  racket/logging
  racket-tree-utils/src/tree
  "abstract-analysis.rkt"
  "abstract-domain-ordering.rkt"
  "abstract-knowledge.rkt"
  "abstract-multi-domain.rkt"
  "abstract-substitution.rkt"
  "abstraction-inspection-utils.rkt"
  (prefix-in ck: "concrete-knowledge.rkt")
  "data-utils.rkt"
  (only-in "control-flow.rkt" aif it)
  "gen-graph-structs.rkt")
(require (for-doc scribble/manual))

(define (graph-map proc g)
  (define g* (graph-copy g))
  (for ([v (get-vertices g)]) (rename-vertex! g* v (proc v)))
  g*)
(provide graph-map)

;; computes how many conjuncts will be introduced when knowledge (clause or full evaluation) is applied
(define (knowledge-output-length knowledge conjunct)
  (match knowledge
    [(ck:rule h b _) (length b)]
    [(full-evaluation i o _) 0]
    ['one (length (multi-conjunction conjunct))]
    ['many (add1 (length (multi-conjunction conjunct)))]))

;; checks whether a value 'idx' is inside the half-open interval 'range'
(define (contains range idx)
  (and (>= idx (index-range-start range))
       (< idx (index-range-end-before range))))

;; computes the skeleton for a generational graph, i.e. the data structure without any generations
;; uid-acc is the unique identifier used for an abstract conjunct in the generational graph
;; graph is the (mutable) generational graph
;; edges is a list of edges from conjuncts at the previous level to a range of indices at the current level
(define (generational-graph-skeleton branch [uid-acc 1] [graph (unweighted-graph/directed (list))] [edges (list)])
  (match branch
    [(list label) ; same for tree label and generalization
     (foldl
      (match-lambda**
       [(conjunct (cons uid idx))
        ;; avoid folding if parent was a generalization, otherwise we'll get an infinite cycle
        (let ([foldable? (andmap (match-lambda [(cons (gen-node (? multi?) _ _ #t _) _) #f] [_ #t]) edges)])
          (add-vertex! graph (gen-node conjunct uid #f #f foldable?)) ; end of branch never has selection
          (for ([edge edges])
            (when (contains (cdr edge) idx)
              (add-directed-edge! graph (car edge) (gen-node conjunct uid #f #f foldable?))))
          (cons (add1 uid) (add1 idx)))])
      (cons uid-acc 0)
      (label-conjunction label))
     graph]
    ; if there are unfoldings and there is a selection
    [(list-rest
      (or (tree-label tl-con1 (some selected1) _ _ _ _)
          (generalization tl-con1 (some selected1) _ _ _ _))
      (tree-label _ _ _ tl-rule2 _ _)
      l-rest)
     (match-let
         ;; all these identified values correspond to parameters of the function
         ([(list next-uid _ _ add-edges)
           (foldl
            (match-lambda**
             [(conjunct (list uid idx range-start introduced-edges))
              (let ([foldable? (andmap (match-lambda [(cons (gen-node (? multi?) _ _ #t _) _) #f] [_ #t]) edges)])
                (add-vertex! graph (gen-node conjunct uid #f (equal? idx selected1) foldable?))
                (for ([edge edges])
                  (when (contains (cdr edge) idx)
                    (add-directed-edge! graph (car edge) (gen-node conjunct uid #f (equal? idx selected1) foldable?))))
                (let ([vertex-edges
                       (cons ;; pair of the current conjunct (as a node) and the range of "spawned" conjuncts
                        (gen-node conjunct uid #f (equal? idx selected1) foldable?)
                        (if (not (equal? idx selected1))
                            (index-range range-start (add1 range-start)) ; unselected conjuncts refer to the conjunct "below" hem
                            (index-range range-start (+ range-start (knowledge-output-length tl-rule2 conjunct)))))]
                      [new-range-start (if (not (equal? idx selected1)) (add1 range-start) (+ range-start (knowledge-output-length tl-rule2 conjunct)))])
                  (list (add1 uid) (add1 idx) new-range-start (cons vertex-edges introduced-edges))))])
            (list uid-acc 0 0 (list))
            tl-con1)])
       (generational-graph-skeleton (cdr branch) next-uid graph add-edges))]
    ; if there are unfoldings but no selection
    [(list-rest
      ;; generalization followed by generalization is possible on paper, but implementation never does this
      (tree-label tl-con1 (none) _ _ _ _)
      (generalization _ _ _ _ abstracted-ranges _) ; TODO may be able to utilize last field, which was added later
      l-rest)
     (log-debug "first element on branch: ~a" (first branch))
     (log-debug "second element on branch: ~a" (second branch))
     (match-let
         ([(list next-uid _ _ add-edges)
           (foldl
            (match-lambda**
             [(conjunct (list uid idx range-start introduced-edges))
              (let ([foldable? (andmap (match-lambda [(cons (gen-node (? multi?) _ _ #t _) _) #f] [_ #t]) edges)])
                (add-vertex! graph (gen-node conjunct uid #f #f foldable?))
                (for ([edge edges])
                  (when (contains (cdr edge) idx)
                    (add-directed-edge! graph (car edge) (gen-node conjunct uid #f #f foldable?))))
                (let ([vertex-edges
                       (cons
                        (gen-node conjunct uid #f #f foldable?)
                        (index-range range-start (add1 range-start)))] ; each conjunct has exactly one outgoing edge!
                      [new-range-start
                       (if (ormap (λ (r) (contains (struct-copy index-range r [end-before (sub1 (index-range-end-before r))]) idx)) abstracted-ranges)
                           range-start ; next conjunct will also be abstracted
                           (add1 range-start))])
                  (list (add1 uid) (add1 idx) new-range-start (cons vertex-edges introduced-edges))))])
            (list uid-acc 0 0 (list))
            tl-con1)])
       (generational-graph-skeleton (cdr branch) next-uid graph add-edges))]))
(module+ test
  (require
    rackunit
    (prefix-in sl-branch-tree: "analysis-trees/sameleaves-branch.rkt")
    (prefix-in sl-branch-skeleton: "analysis-trees/sameleaves-no-multi-branch-gen-tree-skeleton.rkt"))
  (define sl-branch (active-branch sl-branch-tree:val))
  (define sl-graph-skeleton sl-branch-skeleton:val)
  (check-equal? (generational-graph-skeleton sl-branch) sl-graph-skeleton)
  (require (prefix-in o-primes-branch-tree: "analysis-trees/optimus-primes-branch.rkt")
           (prefix-in o-primes-branch-skeleton: "analysis-trees/optimus-primes-branch-gen-graph-skeleton.rkt"))
  (define o-primes-branch (active-branch o-primes-branch-tree:val))
  (define o-primes-graph-skeleton o-primes-branch-skeleton:val)
  (check-equal? (generational-graph-skeleton o-primes-branch) o-primes-graph-skeleton))
(provide generational-graph-skeleton) ; just for test

(define (active-branch t)
  (match t
    [(node (tree-label (list) _ _ _ _ _) '()) #f]
    [(node 'fail '()) #f]
    [(node (cycle _) '()) #f]
    [(node (tree-label c (none) s r #f ie) '())
     (list (tree-label c (none) s r #f ie))]
    [(node (generalization c (none) #f ie rngs bb) '())
     (list (generalization c (none) #f ie rngs bb))]
    [(node (tree-label c sel s r i ie) ch)
     (aif (foldl (λ (c acc) (if acc acc (active-branch c))) #f ch)
          (cons (tree-label c sel s r i ie) it)
          #f)]
    [(node (generalization c sel i ie rngs bb) ch)
     (aif (foldl (λ (c acc) (if acc acc (active-branch c))) #f ch)
          (cons (generalization c sel i ie rngs bb) it)
          #f)]))
(provide
 (proc-doc/names
  active-branch
  (-> node? (or/c #f (listof (or/c tree-label? generalization?))))
  (tree)
  @{Returns a @racket[list] of all the labels on the active branch in @racket[tree].
 The active branch is the first branch, considered from left to right, with a leaf node eligible for further unfolding.
 If there is no such leaf node in the tree, the result is @racket[#f].}))

;; checks if a1 renames a2 *and* whether any shared abstract variables occur in corresponding positions
;; assumes this is only used with abstract-atoms, not with multi abstractions
(define (renames-with-corresponding-args? a1 a2)
  (define (as at) (list->set (map a (assemble-var-indices a? at))))
  (define (gs at) (list->set (map g (assemble-var-indices g? at))))
  (define shared
    (set-union (set-intersect (as a1) (as a2)) (set-intersect (gs a1) (gs a2))))
  (define subst (map (λ (v) (abstract-equality v (abstract-function (gensym "dummy") (list)))) (set->list shared)))
  (renames? (apply-substitution subst a1) (apply-substitution subst a2)))
(module+ test
  (require "cclp-interpreter.rkt") ; note: interpreter will be deprecated at some point
  (check-true
   (renames-with-corresponding-args?
    (interpret-abstract-atom "atom(γ1,γ2,α3,α4)")
    (interpret-abstract-atom "atom(γ1,γ2,α3,α4)")))
  (check-true
   (renames-with-corresponding-args?
    (interpret-abstract-atom "atom(γ1,γ2,α3,α4)")
    (interpret-abstract-atom "atom(γ1,γ3,α3,α5)")))
  (check-false
   (renames-with-corresponding-args?
    (interpret-abstract-atom "atom(γ1,γ2,α3,α4)")
    (interpret-abstract-atom "atom(γ2,γ1,α3,α5)")))
  (check-true
   (renames-with-corresponding-args?
    (interpret-abstract-atom "atom(γ1,γ2,α3,α4)")
    (interpret-abstract-atom "atom(γ5,γ6,α7,α8)"))))

;; checks whether any descendant of root in graph is a renaming of root and whether shared vars are in the same position
;; target is assumed to be either equal to root, or descendant of root which renames it with corresponding args
(define (descendant-renames-with-corresponding-args? graph root [target root])
  (define tc (transitive-closure graph))
  (hash-set! tc (list root root) #f)
  (define reached (map second (filter (λ (p) (and (hash-ref tc p) (equal? (first p) target))) (hash-keys tc))))
  (define just-atoms (map gen-node-conjunct reached))
  (define root-atom (gen-node-conjunct root))
  (ormap (λ (a) (and (abstract-atom? root-atom) (abstract-atom? a) (renames-with-corresponding-args? root-atom a))) just-atoms))
(module+ test
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 #f #t #t)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 3 #f #f #t)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'eq (list (a 1) (a 2))) 4 #f #f #t)))
  (check-false (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'sameleaves (list (g 1) (g 2))) 1 #f #t #t))))

;; Finds potential target atoms for recursion analysis.
; REFACTOR: live-depth is probably redundant, as is root, because the skeleton is a DAG
(define (candidate-targets skeleton [root #f] [live-depth #f]) ; "OPTIONAL" ARGS ARE IGNORED! remove in calls.
  (define dists (floyd-warshall skeleton))
  (define (max-dist el acc)
    (define dist (hash-ref dists el))
    (if (and (not (equal? dist +inf.0)) (or (not acc) (> dist (hash-ref dists acc)))) el acc))
  (define max-key (foldl max-dist #f (hash-keys dists)))
  (define root (car max-key))
  (define live-depth (hash-ref dists max-key))
  (define verts (get-vertices skeleton))
  (define (ancestor? v1 v2) (< 0 (hash-ref dists `(,v1 ,v2)) +inf.0))
  (define (has-live-descendant? v1)
    (define v1-depth (hash-ref dists `(,root ,v1)))
    (ormap (λ (v2) (= (+ v1-depth (hash-ref dists `(,v1 ,v2))) live-depth)) verts))
  (define (right-genealogy? v1)
    (let ([direct-live-lines 0]
          [eq-descendant? #f])
      (for ([v2 verts] #:break (and (= direct-live-lines 2) eq-descendant?))
        (let ([dist (hash-ref dists `(,v1 ,v2))])
          (when (and (= dist 1) (has-live-descendant? v2))
            (set! direct-live-lines (add1 direct-live-lines)))
          (when (and (not eq-descendant?)
                     (< 0 dist +inf.0)
                     (abstract-atom? (gen-node-conjunct v2))
                     (renames-with-corresponding-args?
                      (gen-node-conjunct v1)
                      (gen-node-conjunct v2)))
            (set! eq-descendant? #t))))
      (and (= direct-live-lines 2) eq-descendant?)))
  (define (accumulate-vert v acc)
    (if (and (abstract-atom? (gen-node-conjunct v)) (right-genealogy? v)) (cons v acc) acc))
  (define indirect-candidates (foldl accumulate-vert (list) verts))
  (define (ancestor-in? c cs)
    (ormap (λ (e) (ancestor? e c)) cs))
  (filter (λ (ic) (not (ancestor-in? ic indirect-candidates))) indirect-candidates))

(module+ test
  (define sl-skeleton-root
    (gen-node (abstract-atom 'sameleaves (list (g 1) (g 2))) 1 #f #t #t))
  (check-equal?
   (candidate-targets
    sl-graph-skeleton
    sl-skeleton-root
    3)
   (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 #f #t #t)))
  (define o-primes-skeleton-root (gen-node (abstract-atom 'oprimes (list (g 1) (a 1))) 1 #f #t #t))
  (define o-primes-candidate-targets
    (list
     (gen-node (abstract-atom 'siftA (list (abstract-function 'cons (list (g 2) (a 4))) (a 3))) 7 #f #t #t)
     (gen-node (abstract-atom 'siftB (list (abstract-function 'cons (list (g 2) (a 6))) (a 1))) 13 #f #t #t)))
  (check-equal?
   (candidate-targets
    o-primes-graph-skeleton
    o-primes-skeleton-root
    5)
   o-primes-candidate-targets))
(provide candidate-targets) ; just for test

;; tests whether 'root' in RDAG 'graph' has at least two children and whether both children have descendants at depth 'live-depth', when 'root' is at 'curr-depth'
;; note spelling: live lines, not life lines
;; that is, these lines are not "extinct"
(define (multiple-direct-live-lines? graph root live-depth curr-depth)
  (let ([children-reaching-live-depth
         (filter
          (λ (c) (can-reach-depth? c graph live-depth (+ curr-depth 1)))
          (get-neighbors graph root))])
    (>= (length children-reaching-live-depth) 2)))

(define (can-reach-depth? vertex graph target-depth curr-depth)
  (cond [(>= curr-depth target-depth) #t]
        [(null? (get-neighbors graph vertex)) #f]
        [else
         (ormap
          (λ (c) can-reach-depth? c graph target-depth (+ curr-depth) 1)
          (get-neighbors graph vertex))]))

(define (generic-minmax ltgt elem . elems)
  (foldl (λ (el acc) (if (ltgt acc el) acc el)) elem elems))
(module+ test
  (check-equal? (generic-minmax < 4 9 2 7 6) 2)
  (check-equal? (generic-minmax > 4 9 2 7 6) 9))

;; used to organize descendants of a relevant target atom by generation
;; the cases below should cover all scenarios, if transitivity is applied
;; e.g. start from generation with number
;; then, there is a multi which starts with a number and ends with a symbolic "L"
;; then, there could be another conjunction or atom with a symbolic sum
;; then, another multi whose lowest generation shares a symbol with that symbolic sums
;; we cannot compare the symbolic "L's" directly, but we can apply transitivity
(define (gen-number< g1 g2)
  (match* (g1 g2)
    [((? number?) (? number?)) (< g1 g2)]
    [((? number?) (or (? symbol?) (? symsum?))) #t]
    [((or (? symbol?) (? symsum?)) (? number?)) #f]
    [((? symbol?) (? symbol?)) #:when (equal? g1 g2) #f]
    [((? symbol?) (symsum sym num)) #:when (equal? g1 sym) (>= num 1)]
    [((symsum sym num) (? symbol?)) #:when (equal? sym g2) (< num 0)]
    [((symsum sym1 num1) (symsum sym2 num2)) #:when (equal? sym1 sym2) (< num1 num2)]
    [(_ _) (error (format "unexpected comparison of generations" g1 g2))]))
(module+ test
  (check-true (gen-number< 2 3))
  (check-false (gen-number< 3 2))
  (check-true (gen-number< 1000 'l))
  (check-false (gen-number< 'l 1000))
  (check-true (gen-number< (symsum 'l 0) (symsum 'l 10)))
  (check-false (gen-number< (symsum 'l 10) (symsum 'l 0))))
(provide
 (proc-doc/names
  gen-number<
  (-> (or/c number? symbol? symsum?) (or/c number? symbol? symsum?) boolean?)
  (g1 g2)
  @{Checks whether @racket[g1] is a strictly lower generation number than @racket[g2].
 If the two generations are not directly comparable, an error is raised.}))

;; check whether we can quantify the generational gap between two generations
;; if so, return the size of the gap
(define (gen-gap gen1 gen2)
  (and
   (gen-origin gen1)
   (gen-origin gen2)
   (equal? (gen-origin gen1) (gen-origin gen2))
   (let ([gen1n (gen-number gen1)]
         [gen2n (gen-number gen2)])
     (cond
       [(equal? gen1 gen2) 0]
       [(and (number? gen1n) (number? gen2n))
        (- gen1n gen2n)]
       [(and (symbol? gen1n) (symsum? gen2n) (equal? gen1n (symsum-sym gen2n)))
        (symsum-num gen2n)]
       [(and (symbol? gen2n) (symsum? gen1n) (equal? gen2n (symsum-sym gen1n)))
        (symsum-num gen1n)]
       [(and (symsum? gen1n) (symsum? gen2n) (equal? (symsum-sym gen1n) (symsum-sym gen2n)))
        (- (symsum-num gen1n) (symsum-num gen2n))]
       [else #f]))))
(module+ test
  (check-false (gen-gap (gen 'l1 1) (gen 0 #f)))
  (check-false (gen-gap (gen 'l1 1) (gen 'l2 1)))
  (check-false (gen-gap (gen '3 1) (gen 2 2)))
  (check-equal? (gen-gap (gen (symsum 'l1 3) 1) (gen 'l1 1)) 3)
  (check-equal? (gen-gap (gen (symsum 'l1 -2) 1) (gen 'l1 1)) -2))

;; minimum generation in the range of a conjunct
(define (local-min c)
  (match c
    [(gen-node _ _ (gen num _) _ _) num]
    [(gen-node _ _ (gen-range fst lst _ #t) _ _) fst]
    [(gen-node _ _ (gen-range fst lst _ #f) _ _) lst]
    [else (error (format "conjunct unaccounted for: ~a" c))]))
(module+ test
  (check-equal?
   (local-min (gen-node (multi (list) #f (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 'l1 1 1 #f) #t #t))
   1)
  (check-equal?
   (local-min (gen-node (multi (list) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l1 1 #t) #f #t))
   1))

(define (local-max c)
  (match c
    [(gen-node _ _ (gen num _) _ _) num]
    [(gen-node _ _ (gen-range fst lst _ #t) _ _) lst]
    [(gen-node _ _ (gen-range fst lst _ #f) _ _) fst]))
(module+ test
  (check-equal?
   (local-max (gen-node (multi (list) #f (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 'l1 1 1 #f) #f #t))
   'l1)
  (check-equal?
   (local-max (gen-node (multi (list) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l1 1 #t) #f #t))
   'l1))

(define (gen-range-first/gen rng)
  (gen (gen-range-first rng) (gen-range-origin rng)))
(provide gen-range-first/gen)

(define (gen-range-last/gen rng)
  (gen (gen-range-last rng) (gen-range-origin rng)))
(provide gen-range-last/gen)

(define (subsequent-gens? g1 g2)
  (match* (g1 g2)
    [((? gen?) (? gen?))
     (or (equal? (gen-increment g1) g2)
         (equal? (gen-decrement g1) g2))]
    [((? gen?) (? gen-range?))
     (if (gen-range-ascending? g2)
         (equal? (gen-increment g1) (gen-range-first/gen g2))
         (equal? (gen-decrement g1) (gen-range-first/gen g2)))]
    [((? gen-range?) (? gen?))
     (if (gen-range-ascending? g1)
         (equal? (gen-increment (gen-range-last/gen g1)) g2)
         (equal? (gen-decrement (gen-range-last/gen g1)) g2))]
    [((gen-range f1 l1 o asc?) (gen-range f2 l2 o asc?))
     (if asc?
         (equal? (gen-increment (gen-range-last/gen g1)) (gen-range-first/gen g2))
         (equal? (gen-decrement (gen-range-last/gen g1)) (gen-range-first/gen g2)))]
    [(_ _) #f]))
(provide subsequent-gens?)

;; add an offset to a (potentially symbolic) generation number
(define (gen-add g o)
  (cond [(= o 0) g]
        [(> o 0) (gen-add (gen-add1 g) (sub1 o))]
        [(< o 0) (gen-add (gen-sub1 g) (add1 o))]))

(define (gen-increment g)
  (match g
    [(gen num o) (gen (gen-add1 num) o)]))
(provide gen-increment)

(define (gen-decrement g)
  (match g
    [(gen num o) (gen (gen-sub1 num) o)]))
(provide gen-decrement)

(define (gen-add1 gen-num)
  (match gen-num
    [(? exact-integer?) (add1 gen-num)]
    [(? symbol?) (symsum gen-num 1)]
    [(symsum sym -1) sym]
    [(symsum sym num) (symsum sym (add1 num))]))
(provide
 (proc-doc/names
  gen-add1
  (-> (or/c exact-integer? symbol? symsum?) (or/c exact-integer? symbol? symsum?))
  (gen-num)
  @{Computes the generation number after @racket[gen-num].}))

(define (gen-sub1 gen-num)
  (match gen-num
    [(? exact-integer?) (sub1 gen-num)]
    [(? symbol?) (symsum gen-num -1)]
    [(symsum sym 1) sym]
    [(symsum sym num) (symsum sym (sub1 num))]))
(provide
 (proc-doc/names
  gen-sub1
  (-> (or/c exact-integer? symbol? symsum?) (or/c exact-integer? symbol? symsum?))
  (gen-num)
  @{Computes the generation number before @racket[gen-num].}))

(define (annotate-unfolding! id-conjunct ann-parent relevant-targets graph live-depth curr-depth)
  (match-define (gen-node parent-conjunct parent-id parent-gen parent-unfolded? _) ann-parent)
  (cond
    [(member ann-parent relevant-targets)
     (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range (gen 1 parent-id)]))]
    [(and
      (abstract-atom? parent-conjunct)
      (let ([tg (findf (λ (rel-tg) (and (equal? (gen-origin parent-gen) (gen-node-id rel-tg)) (renames-with-corresponding-args? parent-conjunct (gen-node-conjunct rel-tg)))) relevant-targets)])
        (and tg (descendant-renames-with-corresponding-args? graph tg ann-parent)))
      (multiple-direct-live-lines? graph ann-parent live-depth curr-depth))
     (if parent-unfolded?
         (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range (gen-increment parent-gen)]))
         (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range parent-gen])))]
    [(or (abstract-atom? parent-conjunct) (not parent-unfolded?))
     (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range parent-gen]))]
    ;; the remaining cases, by elimination, are multi cases
    [(and (abstract-atom? (gen-node-conjunct id-conjunct)))
     (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range (gen (gen-range-first parent-gen) (gen-range-origin parent-gen))]))]
    [(and (multi? (gen-node-conjunct id-conjunct)) (multi-ascending? parent-conjunct))
     (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range (struct-copy gen-range parent-gen [first (gen-add1 (gen-range-first parent-gen))])]))]
    [else
     (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range (struct-copy gen-range parent-gen [first (gen-sub1 (gen-range-first parent-gen))])]))]))
; TODO this needs separate tests!

(define (apply-multi-mapping! spc parent multi-mapping graph)
  ;; compute the generation of a child of parent-gen when substitutee-gen is replaced with substituter-gen on the same level of the RDAG
  ;; e.g. if gen 3 becomes symbol l1, gen 4 becomes symbolic sum l1+1
  (define (translate-gen parent-gen substitutee-gen substituter-gen)
    (define gap (gen-gap parent-gen substitutee-gen))
    (if gap (struct-copy gen parent-gen [number (gen-add substituter-gen gap)]) #f))
  (define parent-gens (gen-node-range parent))
  (define (apply-single! substitutee-gen substituter-gen)
    (cond
      [(gen? parent-gens)
       (let ([translation  (translate-gen parent-gens substitutee-gen substituter-gen)])
         (when translation (rename-vertex! graph spc (struct-copy gen-node spc [range translation]))))]
      [(gen-range? parent-gens)
       (let* ([first* (gen (gen-range-first parent-gens) (gen-range-origin parent-gens))]
              [last* (gen (gen-range-last parent-gens) (gen-range-origin parent-gens))]
              [translation1 (translate-gen first* substitutee-gen substituter-gen)]
              [translation2 (translate-gen last* substitutee-gen substituter-gen)])
         (when (and translation1 (not (equal? translation1 last*)))
           (rename-vertex! graph spc (struct-copy gen-node spc [range (struct-copy gen-range parent-gens [first (gen-number translation1)])])))
         (when (and translation2 (not (equal? translation2 first*)))
           (rename-vertex! graph spc (struct-copy gen-node spc [range (struct-copy gen-range parent-gens [last (gen-number translation2)])])))
         (when (and translation1 (equal? translation1 last*))
           (rename-vertex! graph spc (struct-copy gen-node spc [range translation1])))
         (when (and translation2 (equal? translation2 first*))
           (rename-vertex! graph spc (struct-copy gen-node spc [range translation2]))))]))
  (for ([key (hash-keys multi-mapping)])
    (apply-single! key (hash-ref multi-mapping key)))
  ;; if the graph still contains original spc at this point, no mapping was needed
  (when (has-vertex? graph spc)
    (rename-vertex! graph spc (struct-copy gen-node spc [range parent-gens]))))

;; given an annotated multi, compute which generation (in the range) disappears when the multi is unfolded in case:one
;; the result is a pair from the disappearing generation to thereplacing generation number
(define (disappearing-pair m-node)
  (match m-node
    [(gen-node c _ (gen-range f l id #t) _ _)
     (let ([offset (if (symbol? l) 0 (symsum-num l))])
       (cons (gen (gen-add l (- offset)) id) (gen-add f (- offset))))]
    [(gen-node c _ (gen-range f l id #f) _ _)
     (let ([offset (if (symbol? f) 0 (symsum-num f))])
       (cons (gen (gen-add f (- offset)) id) (gen-add l (- offset))))]))

;; annotates a level of the RDAG, other than the root level
;; TODO: parent-level-number is completely redundant? it is just the current level - 1...
(define (annotate-level! graph annotated-root postfix-box relevant-targets live-depth parent-level-number level-number)
  (define parent-level (rdag-level graph annotated-root parent-level-number))
  (define level (rdag-level graph annotated-root level-number))
  (match-define-values
   (new-multis single-parent-conjuncts)
   (partition (λ (conjunct) (> (length (get-neighbors (transpose graph) conjunct)) 1)) level))
  (define ex-multis
    (filter
     (lambda (c) (and (multi? (gen-node-conjunct c)) (not (ormap (compose1 multi? gen-node-conjunct) (get-neighbors graph c)))))
     parent-level))
  (define i-multi-mapping (foldl (curry annotate-new-multi! graph postfix-box) #hash() new-multis))
  (define o-multi-mapping (foldl (λ (e acc) (let ([pair (disappearing-pair e)]) (hash-set acc (car pair) (cdr pair)))) #hash() ex-multis))
  (for ([spc single-parent-conjuncts])
    (let ([parent (first (get-neighbors (transpose graph) spc))])
      (cond [(and (null? ex-multis) (null? new-multis))
             (annotate-unfolding! spc parent relevant-targets graph live-depth parent-level-number)]
            [(not (null? new-multis)) (apply-multi-mapping! spc parent i-multi-mapping graph)]
            [(not (null? ex-multis)) (apply-multi-mapping! spc parent o-multi-mapping graph)]))))
(module+ test
  (require (prefix-in almost-annotated: "analysis-trees/sameleaves-multi-branch-gen-tree-almost-annotated.rkt"))
  (define almost-annotated (graph-copy almost-annotated:val))
  (define sl-annotated-root (struct-copy gen-node sl-skeleton-root [range (gen 0 #f)]))
  (require
    (prefix-in sl-multi-graph-skeleton: "analysis-trees/sameleaves-multi-branch-gen-tree-skeleton.rkt")
    (prefix-in sl-multi-graph-annotated: "analysis-trees/sameleaves-multi-branch-gen-tree.rkt"))
  (define sl-multi-graph-annotated (graph-copy sl-multi-graph-skeleton:val))
  (rename-vertex! sl-multi-graph-annotated sl-skeleton-root sl-annotated-root)
  (annotate-level! sl-multi-graph-annotated sl-annotated-root (box 1) (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t #t)) 6 1 2)
  (check-equal?
   (rdag-level sl-multi-graph-annotated sl-annotated-root 2)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 2))
  (annotate-level! sl-multi-graph-annotated sl-annotated-root (box 1) (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t #t)) 6 2 3)
  (check-equal?
   (rdag-level sl-multi-graph-annotated sl-annotated-root 3)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 3))
  (annotate-level! sl-multi-graph-annotated sl-annotated-root (box 1) (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t #t)) 6 3 4)
  (check-equal?
   (rdag-level sl-multi-graph-annotated sl-annotated-root 4)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 4))
  (annotate-level! almost-annotated sl-annotated-root (box 1) (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t #t)) 6 5 6)
  (check-equal?
   (rdag-level almost-annotated sl-annotated-root 6)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 6)))

(define (annotate-new-multi! graph postfix-box new-multi mapping)
  ;; parents must be ordered to check whether generations are ascending
  (define parents (sort (get-neighbors (transpose graph) new-multi) < #:key gen-node-id))
  (define bare-multi (gen-node-conjunct new-multi))
  (define parent-minimum (apply (curry generic-minmax gen-number<) (map local-min parents)))
  (define parent-maximum (apply (curry generic-minmax (compose not gen-number<)) (map local-max parents)))
  (define parent-origin (let ([parent-gen (gen-node-range (first parents))]) (if (gen? parent-gen) (gen-origin parent-gen) (gen-range-origin parent-gen)))) ; first, but could pick any one
  (define symbolic-l (string->symbol (format "l~a" (unbox postfix-box))))
  (define multi-parent (findf (λ (p) (multi? (gen-node-conjunct p))) parents))
  (define ascending?
    (if multi-parent
        ((compose1 gen-range-ascending? gen-node-range) multi-parent)
        (gen-number< (gen-number (gen-node-range (first parents))) (gen-number (gen-node-range (last parents))))))
  (define range
    (if ascending?
        (gen-range parent-minimum (if (not multi-parent) symbolic-l parent-maximum) parent-origin ascending?)
        (gen-range (if (not multi-parent) symbolic-l parent-maximum) parent-minimum parent-origin ascending?)))
  (define updated-multi (struct-copy gen-node new-multi [range range]))
  (rename-vertex! graph new-multi updated-multi)
  (if (not multi-parent) (begin (set-box! postfix-box (add1 (unbox postfix-box))) (hash-set mapping (gen parent-maximum parent-origin) symbolic-l)) mapping))
(module+ test
  (set! almost-annotated (graph-copy almost-annotated:val))
  (require (prefix-in almost-annotated-m: "analysis-trees/sameleaves-multi-branch-gen-tree-almost-annotated-with-multi.rkt"))
  (define almost-annotated-with-multi (graph-copy almost-annotated-m:val))
  (define _ (annotate-new-multi! almost-annotated (box 1) (list-ref (sort (rdag-level almost-annotated sl-annotated-root 6) < #:key gen-node-id) 3) (make-immutable-hash)))
  (for ([lv (range 1 6)])
    (check-equal?
     (rdag-level almost-annotated sl-annotated-root lv)
     (rdag-level almost-annotated-with-multi sl-annotated-root lv)))
  (check-equal?
   (rdag-level almost-annotated sl-annotated-root 6)
   (rdag-level almost-annotated-with-multi sl-annotated-root 6))
  (check-equal? almost-annotated almost-annotated-with-multi))

;; note: this takes a skeleton as an input, but it modifies it so that it becomes a full generational graph
(define (annotate-general! skeleton root relevant-targets rdag-depth)
  (define postfix-box (box 1))
  (define annotated-root (struct-copy gen-node root [range (gen 0 #f)]))
  (rename-vertex! skeleton root annotated-root)
  (for ([parent-level (range 1 rdag-depth)]
        [current-level (range 2 (add1 rdag-depth))])
    (annotate-level! skeleton annotated-root postfix-box relevant-targets rdag-depth parent-level current-level)))
(module+ test
  (require
    (prefix-in sl-graph-annotated: "analysis-trees/sameleaves-no-multi-branch-gen-tree.rkt"))
  (define sl-graph-annotated (graph-copy sl-graph-skeleton))
  (annotate-general! sl-graph-annotated sl-skeleton-root (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t #t)) (length sl-branch))
  (check-equal?
   sl-graph-annotated
   sl-graph-annotated:val)
  (require
    (prefix-in sl-multi-branch-tree: "analysis-trees/sameleaves-multi-branch.rkt"))
  (define sl-multi-branch (active-branch sl-multi-branch-tree:val))
  (set! sl-multi-graph-annotated (graph-copy sl-multi-graph-skeleton:val))
  (annotate-general! sl-multi-graph-annotated sl-skeleton-root (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t #t)) (length sl-multi-branch))
  (check-equal?
   sl-multi-graph-annotated
   sl-multi-graph-annotated:val)
  (require
    (prefix-in o-primes-graph-annotated: "analysis-trees/optimus-primes-branch-gen-graph.rkt"))
  (define o-primes-graph-annotated (graph-copy o-primes-graph-skeleton))
  (annotate-general!
   o-primes-graph-annotated
   o-primes-skeleton-root
   o-primes-candidate-targets
   (length o-primes-branch))
  (define o-primes-annotated-root (struct-copy gen-node o-primes-skeleton-root [range (gen 0 #f)]))
  (for ([lv (range 1 22)])
    (check-equal?
     (sort (rdag-level o-primes-graph-annotated o-primes-annotated-root lv) < #:key gen-node-id)
     (sort (rdag-level o-primes-graph-annotated:val o-primes-annotated-root lv)  < #:key gen-node-id)))
  (check-equal?
   o-primes-graph-annotated
   o-primes-graph-annotated:val)
  (require (prefix-in fake-primes-skeleton: "analysis-trees/fake-primes-gen-graph-skeleton.rkt"))
  (require (prefix-in fake-primes-annotated: "analysis-trees/fake-primes-gen-graph.rkt"))
  (define fake-primes-annotated (graph-copy fake-primes-skeleton:val))
  (define fake-primes-root (gen-node (abstract-atom 'fakeprimes (list (g 3) (a 2))) 1 #f #t))
  (define fake-primes-annotated-root (gen-node (abstract-atom 'fakeprimes (list (g 3) (a 2))) 1 (gen 0 #f) #t))
  (annotate-general! fake-primes-annotated fake-primes-root (list (gen-node (abstract-atom 'sift (list (abstract-function 'cons (list (g 2) (a 1))) (a 2))) 3 (gen 0 #f) #t)) 19)
  (check-equal?
   fake-primes-annotated
   fake-primes-annotated:val)
  (require (prefix-in sameleaves-extended-skeleton: "analysis-trees/sameleaves-multi-branch-gen-tree-extended-1-skeleton.rkt"))
  (require (prefix-in sameleaves-extended: "analysis-trees/sameleaves-multi-branch-gen-tree-extended-1.rkt"))
  (define sameleaves-extended-annotated (graph-copy sameleaves-extended-skeleton:val))
  (annotate-general! sameleaves-extended-annotated sl-skeleton-root (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) 16))
(provide annotate-general!) ; just for test

;; extract a level from a rooted DAG
;; the lowest level that can be extracted is 1 (the root)
;; no guarantees about how the result is sorted, but it does not contain duplicates
(define (rdag-level rdag root level)
  (define (rdag-level-aux rdag root level depth-acc)
    (if (eqv? depth-acc level)
        (list root)
        (apply append (map (λ (s) (rdag-level-aux rdag s level (add1 depth-acc))) (get-neighbors rdag root)))))
  (remove-duplicates (rdag-level-aux rdag root level 1)))
(module+ test
  (check-equal?
   (rdag-level sl-graph-skeleton sl-skeleton-root 1)
   (list sl-skeleton-root))
  (check-equal?
   (sort (rdag-level sl-graph-skeleton sl-skeleton-root 2) < #:key gen-node-id)
   (list
    (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 #f #t)
    (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 3 #f #f)
    (gen-node (abstract-atom 'eq (list (a 1) (a 2))) 4 #f #f))))
(provide rdag-level) ; for testing

;; replace sequences with the same origin with multi abstractions at a given level of the generational tree
;(define (gen-tree-level->generalized-conjunction lvl)
;  (map (compose1 identified-atom-atom identified-atom-with-generation-id-atom) lvl)) ; TODO
(module+ test
  ;  (define sl-graph-annotated (graph-copy sl-graph-skeleton))
  ;  (annotate-general! sl-graph-annotated sl-root (list (identified-atom (abstract-atom 'collect (list (g 1) (a 1))) 2)) (length sl-branch))
  ;  (check-equal?
  ;   (gen-tree-level->generalized-conjunction (rdag-level sl-graph-annotated (identified-atom-with-generation sl-root (generation 0 #f)) 5))
  ;   (list)) ; TODO: introduce a multi abstraction
  ;  (check-equal?
  ;   (gen-tree-level->generalized-conjunction
  ;    (list
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'integers (list (g 1) (a 1))) 3) (generation 0 #f))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'filterA (list (g 2) (a 1) (a 2))) 4) (generation 1 1))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'filterA (list (g 3) (a 2) (a 3))) 5) (generation 2 1))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'filterA (list (g 4) (a 3) (a 4))) 6) (generation 3 1))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'siftA (list (a 4) (a 5))) 7) (generation 3 1))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'filterB (list (g 5) (a 5) (a 6))) 8) (generation 1 2))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'filterB (list (g 6) (a 6) (a 7))) 9) (generation 2 2))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'filterB (list (g 7) (a 7) (a 8))) 10) (generation 3 2))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'siftB (list (a 8) (a 9))) 11) (generation 3 2))
  ;     (identified-atom-with-generation (identified-atom (abstract-atom 'length (list (a 9) (a 10))) 12) (generation 0 #f))))
  ;   (list))
  ) ; TODO: introduce two multi abstractions