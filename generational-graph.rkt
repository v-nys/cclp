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

;; computes how many conjuncts will be introduced when knowledge (clause or full evaluation) is applied
(define (knowledge-output-length knowledge)
  (match knowledge
    [(ck:rule h b) (length b)]
    [(full-evaluation i o) 0]))

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
     (begin
       (foldl
        (match-lambda**
         [(conjunct (cons uid idx))
          (begin
            (add-vertex! graph (identified-abstract-conjunct conjunct uid))
            (for ([edge edges])
              (when (contains (cdr edge) idx)
                (add-directed-edge! graph (car edge) (identified-abstract-conjunct conjunct uid))))
            (cons (add1 uid) (add1 idx)))])
        (cons uid-acc 0)
        (label-conjunction label))
       graph)]
    [(list-rest
      (or (tree-label tl-con1 (some selected1) _ _ _ _)
          (generalization tl-con1 (some selected1) _ _ _))
      (tree-label _ _ _ tl-rule2 _ _)
      l-rest)
     (let* ([first-unselected (take tl-con1 selected1)]
            [selected-atom (list-ref tl-con1 selected1)]
            [last-unselected (drop tl-con1 (+ 1 selected1))])
       (match-let
           ;; all these identified values correspond to parameters of the function
           ([(list next-uid _ _ add-edges)
             (foldl
              (match-lambda**
               [(conjunct (list uid idx range-start introduced-edges))
                (begin
                  (add-vertex! graph (identified-abstract-conjunct conjunct uid))
                  (for ([edge edges])
                    (when (contains (cdr edge) idx)
                      (add-directed-edge! graph (car edge) (identified-abstract-conjunct conjunct uid))))
                  (let ([vertex-edges
                         (cons ; pair of the current conjunct and the range of "spawned" conjuncts
                          (identified-abstract-conjunct conjunct uid)
                          (if (not (equal? idx selected1))
                              (index-range range-start (add1 range-start)) ; unselected conjuncts refer to the conjunct "below" hem
                              (index-range range-start (+ range-start (knowledge-output-length tl-rule2)))))]
                        [new-range-start (if (not (equal? idx selected1)) (add1 range-start) (+ range-start (knowledge-output-length tl-rule2)))])
                    (list (add1 uid) (add1 idx) new-range-start (cons vertex-edges introduced-edges))))])
              (list uid-acc 0 0 (list))
              tl-con1)])
         (generational-graph-skeleton (cdr branch) next-uid graph add-edges)))]
    [(list-rest
      (or (tree-label tl-con1 (none) _ _ _ _)
          (generalization tl-con1 (none) _ _ _))
      (generalization _ _ _ _ abstracted-ranges)
      l-rest)
     (match-let
         ([(list next-uid _ _ add-edges)
           (foldl
            (match-lambda**
             [(conjunct (list uid idx range-start introduced-edges))
              (begin
                (add-vertex! graph (identified-abstract-conjunct conjunct uid))
                (for ([edge edges])
                  (when (contains (cdr edge) idx)
                    (add-directed-edge! graph (car edge) (identified-abstract-conjunct conjunct uid))))
                (let ([vertex-edges
                       (cons
                        (identified-abstract-conjunct conjunct uid)
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
  (define sl-branch (active-branch-info sl-branch-tree:val))
  (define sl-graph-skeleton sl-branch-skeleton:val)
  (check-equal? (generational-graph-skeleton sl-branch) sl-graph-skeleton)
  (require (prefix-in o-primes-branch-tree: "analysis-trees/optimus-primes-branch.rkt")
           (prefix-in o-primes-branch-skeleton: "analysis-trees/optimus-primes-branch-gen-graph-skeleton.rkt"))
  (define o-primes-branch (active-branch-info o-primes-branch-tree:val))
  (define o-primes-graph-skeleton o-primes-branch-skeleton:val)
  (check-equal? (generational-graph-skeleton o-primes-branch) o-primes-graph-skeleton))

(define (active-branch-info t)
  (match t
    [(node (tree-label (list) _ _ _ _ _) '()) #f]
    [(node 'fail '()) #f]
    [(node (cycle _) '()) #f]
    [(node (tree-label c (none) s r #f ie) '())
     (list (tree-label c (none) s r #f ie))]
    [(node (generalization c (none) #f ie rngs) '())
     (list (generalization c (none) #f ie rngs))]
    [(node (tree-label c sel s r i ie) ch)
     (aif (foldl (λ (c acc) (if acc acc (active-branch-info c))) #f ch)
          (cons (tree-label c sel s r i ie) it)
          #f)]
    [(node (generalization c sel i ie rngs) ch)
     (aif (foldl (λ (c acc) (if acc acc (active-branch-info c))) #f ch)
          (cons (generalization c sel i ie rngs) it)
          #f)]))
(provide
 (proc-doc/names
  active-branch-info
  (-> node? (or/c #f (listof (or/c tree-label? generalization?))))
  (tree)
  @{Returns the information required to synthesize a clause corresponding to the active branch in @racket[tree].
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
(define (descendant-renames-with-corresponding-args? graph root)
  (define tc (transitive-closure graph))
  (hash-set! tc (list root root) #f)
  (define reached (map second (filter (λ (p) (and (hash-ref tc p) (equal? (first p) root))) (hash-keys tc))))
  (define just-atoms (map identified-abstract-conjunct-conjunct reached))
  (define root-atom (identified-abstract-conjunct-conjunct root))
  (ormap (λ (a) (and (abstract-atom? root-atom) (abstract-atom? a) (renames-with-corresponding-args? root-atom a))) just-atoms))
(module+ test
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (identified-abstract-conjunct (abstract-atom 'collect (list (g 1) (a 1))) 2)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (identified-abstract-conjunct (abstract-atom 'collect (list (g 2) (a 2))) 3)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (identified-abstract-conjunct (abstract-atom 'eq (list (a 1) (a 2))) 4)))
  (check-false (descendant-renames-with-corresponding-args? sl-graph-skeleton (identified-abstract-conjunct (abstract-atom 'sameleaves (list (g 1) (g 2))) 1))))

;; finds potential target atoms for recursion analysis.
;; root is the root of the rooted DAG (i.e. the skeleton)
;; live-depth indicates depth from which an atom may survive indefinitely
(define (candidate-target-identified-atoms skeleton root live-depth)
  (define (candidate-target-identified-atoms-aux skeleton root live-depth [depth-acc 0])
    (if (>= depth-acc live-depth)
        (list)
        (let ([candidates-among-descendants
               (foldl
                (λ (c candidate-acc)
                  (append candidate-acc (candidate-target-identified-atoms-aux skeleton c live-depth (+ depth-acc 1))))
                (list)
                (reached-neighbors skeleton root))])
          (if (and (multiple-direct-live-lines? skeleton root live-depth depth-acc)
                   (descendant-renames-with-corresponding-args? skeleton root))
              (list root)
              candidates-among-descendants))))
  (remove-duplicates (candidate-target-identified-atoms-aux skeleton root live-depth 0)))
(module+ test
  (define sl-skeleton-root
    (identified-abstract-conjunct (abstract-atom 'sameleaves (list (g 1) (g 2))) 1))
  (check-equal?
   (candidate-target-identified-atoms
    sl-graph-skeleton
    sl-skeleton-root
    3)
   (list (identified-abstract-conjunct (abstract-atom 'collect (list (g 1) (a 1))) 2)))
  (define o-primes-skeleton-root (identified-abstract-conjunct (abstract-atom 'oprimes (list (g 1) (a 1))) 1))
  (define o-primes-candidate-targets
    (list
     (identified-abstract-conjunct (abstract-atom 'siftA (list (abstract-function 'cons (list (g 2) (a 4))) (a 3))) 7)
     (identified-abstract-conjunct (abstract-atom 'siftB (list (abstract-function 'cons (list (g 2) (a 6))) (a 1))) 13)))
  (check-equal?
   (candidate-target-identified-atoms
    o-primes-graph-skeleton
    o-primes-skeleton-root
    5)
   o-primes-candidate-targets))

;; tests whether 'root' in RDAG 'graph' has at least two children and whether both children have descendants at depth 'live-depth', when 'root' is at 'curr-depth'
;; note spelling: live lines, not life lines
;; that is, these lines are not "extinct"
(define (multiple-direct-live-lines? graph root live-depth curr-depth)
  (let ([children-reaching-live-depth
         (filter
          (λ (c) (can-reach-depth? c graph live-depth (+ curr-depth 1)))
          (reached-neighbors graph root))])
    (>= (length children-reaching-live-depth) 2)))

(define (reached-neighbors g v)
  (filter (λ (n) (has-edge? g v n)) (get-neighbors g v)))

(define (can-reach-depth? vertex graph target-depth curr-depth)
  (cond [(>= curr-depth target-depth) #t]
        [(null? (reached-neighbors graph vertex)) #f]
        [else
         (ormap
          (λ (c) can-reach-depth? c graph target-depth (+ curr-depth) 1)
          (reached-neighbors graph vertex))]))

;; TODO: use new approach which will also work for multi
(define (annotate-general! skeleton root relevant-targets rdag-depth) skeleton)
(module+ test
  (require
    (prefix-in sl-graph-annotated: "analysis-trees/sameleaves-no-multi-branch-gen-tree.rkt"))
  (define sl-graph-annotated (graph-copy sl-graph-skeleton))
  (annotate-general! sl-graph-annotated sl-skeleton-root (list (identified-atom (abstract-atom 'collect (list (g 1) (a 1))) 2)) (length sl-branch))
  (check-equal?
   sl-graph-annotated
   sl-graph-annotated:val)
  (require
    (prefix-in sl-multi-branch-tree: "analysis-trees/sameleaves-multi-branch.rkt"))
  (require
    (prefix-in sl-multi-graph-skeleton: "analysis-trees/sameleaves-multi-branch-gen-tree-skeleton.rkt")
    (prefix-in sl-multi-graph-annotated: "analysis-trees/sameleaves-multi-branch-gen-tree.rkt"))
  (define sl-multi-branch (active-branch-info sl-multi-branch-tree:val))
  (define sl-multi-graph-annotated (graph-copy sl-multi-graph-skeleton:val))
  (annotate-general! sl-multi-graph-annotated sl-skeleton-root (list (identified-atom (abstract-atom 'collect (list (g 1) (a 1))) 2)) (length sl-multi-branch))
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
  (check-equal?
   o-primes-graph-annotated
   o-primes-graph-annotated:val))

;; extract a level from a rooted DAG
;; the lowest level that can be extracted is 1 (the root)
;; no guarantees about how the result is sorted, but it does not contain duplicates
(define (rdag-level rdag root level)
  (define (rdag-level-aux rdag root level depth-acc)
    (if (eqv? depth-acc level)
        (list root)
        (apply append (map (λ (s) (rdag-level-aux rdag s level (add1 depth-acc))) (reached-neighbors rdag root)))))
  (remove-duplicates (rdag-level-aux rdag root level 1)))
(module+ test
  (check-equal?
   (rdag-level sl-graph-skeleton sl-skeleton-root 1)
   (list sl-skeleton-root))
  (check-equal?
   (sort (rdag-level sl-graph-skeleton sl-skeleton-root 2) < #:key identified-abstract-conjunct-id-number)
   (list
    (identified-abstract-conjunct (abstract-atom 'collect (list (g 1) (a 1))) 2)
    (identified-abstract-conjunct (abstract-atom 'collect (list (g 2) (a 2))) 3)
    (identified-abstract-conjunct (abstract-atom 'eq (list (a 1) (a 2))) 4))))

;; replace sequences with the same origin with multi abstractions at a given level of the generational tree
(define (gen-tree-level->generalized-conjunction lvl)
  (map (compose1 identified-atom-atom identified-atom-with-generation-id-atom) lvl)) ; TODO
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

(define (generalize t) (cons t #f))
(module+ test
  ;  (require (prefix-in primes5: "analysis-trees/primes-five.rkt"))
  ;  (require (prefix-in generalizedsl-branch-tree: "analysis-trees/generalized-sameleaves-branch.rkt"))
  ;  (check-equal? (generalize primes5:val) (cons primes5:val #f))
  ;  (check-equal? (generalize sl-branch-tree:val) (cons generalizedsl-branch-tree:val #t))
  )
(provide
 (proc-doc/names
  generalize
  (-> node? (cons/c node? boolean?))
  (top)
  @{Attempts to generalize the candidate node in @racket[t].
 If generalization is successful, the first element of the returned pair is @racket[t], extended with a @racket[node?] whose label is a @racket[generalization?] and the second element is @racket[#t].
 Otherwise, the first element is @racket[t] and the second element is @racket[#f].}))