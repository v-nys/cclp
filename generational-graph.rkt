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
  racket/struct
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
  (prefix-in slbranch: "analysis-trees/sameleaves-branch.rkt")
  (prefix-in ck: "concrete-knowledge.rkt")
  "data-utils.rkt"
  (only-in "control-flow.rkt" aif it))

(require (for-doc scribble/manual))


(struct generation (number origin)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'generation)
      (λ (obj) (list (generation-number obj)
                     (generation-origin obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur)
     (and (equal?-recur (generation-number g1)
                        (generation-number g2))
          (equal?-recur (generation-origin g1)
                        (generation-origin g2))))
   (define (hash-proc my-gen hash-recur)
     (+ (hash-recur (generation-number my-gen))
        (hash-recur (generation-origin my-gen))))
   (define (hash2-proc my-gen hash-recur)
     (+ (hash-recur (generation-number my-gen))
        (hash-recur (generation-origin my-gen))))])
(provide
 (struct*-doc
  generation
  ([number (or/c exact-positive-integer? (cons/c symbol? exact-integer?))]
   [origin (or/c #f exact-positive-integer?)])
  @{Used to track the recursion depth of an atom with respect to a uniquely identified target atom.}))

(struct identified-atom-with-generation (id-atom generation)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'identified-atom-with-generation)
      (λ (obj) (list (identified-atom-with-generation-id-atom obj)
                     (identified-atom-with-generation-generation obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc a1 a2 equal?-recur)
     (and (equal?-recur (identified-atom-with-generation-id-atom a1)
                        (identified-atom-with-generation-id-atom a2))
          (equal?-recur (identified-atom-with-generation-generation a1)
                        (identified-atom-with-generation-generation a2))))
   (define (hash-proc my-awg hash-recur)
     (+ (hash-recur (identified-atom-with-generation-id-atom my-awg))
        (hash-recur (identified-atom-with-generation-generation my-awg))))
   (define (hash2-proc my-awg hash2-recur)
     (+ (hash2-recur (identified-atom-with-generation-id-atom my-awg))
        (hash2-recur (identified-atom-with-generation-generation my-awg))))])
(provide
 (struct*-doc
  identified-atom-with-generation
  ([id-atom identified-atom?]
   [generation generation?])
  @{The label of an atom vertex in a generational graph.
     The field @racket[atom] is a single identified atom which,
     juxtaposed with other @racket[identified-atom]s at the same depth,
     forms an abstract conjunction encountered during analysis.
     The field @racket[generation] indicates how many recursive unfoldings of the target atom took place before @racket[atom] was introduced.}))

(define (knowledge-output-length knowledge)
  (match knowledge
    [(ck:rule h b) (length b)]
    [(full-evaluation i o) 0]))
(provide
 (proc-doc/names
  knowledge-output-length
  (-> (or/c ck:rule? full-evaluation?) exact-nonnegative-integer?)
  (knowledge)
  @{Computes how many conjuncts will be introduced when @racket[knowledge] is applied.}))

(struct identified-atom (atom uid)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'identified-atom)
      (λ (obj) (list (identified-atom-atom obj)
                     (identified-atom-uid obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc a1 a2 equal?-recur)
     (and (equal?-recur (identified-atom-atom a1)
                        (identified-atom-atom a2))
          (equal?-recur (identified-atom-uid a1)
                        (identified-atom-uid a2))))
   (define (hash-proc my-ida hash-recur)
     (+ (hash-recur (identified-atom-atom my-ida))
        (hash-recur (identified-atom-uid my-ida))))
   (define (hash2-proc my-ida hash2-recur)
     (+ (hash2-recur (identified-atom-atom my-ida))
        (hash2-recur (identified-atom-uid my-ida))))])
(provide
 (struct*-doc
  identified-atom
  ([atom abstract-atom?]
   [uid exact-nonnegative-integer?])
  @{A uniquely identifiable instance of an abstract atom in a generational graph (or skeleton).
     The field @racket[uid] is unique to each atom in a generational graph.}))

(struct index-range (start end-before))
(define (contains range idx)
  (and (>= idx (index-range-start range))
       (< idx (index-range-end-before range))))

;; associates a unique ID with a conjunct
(define (identify conjunct idx)
  (match conjunct
    ; can do multi when I have an identified multi... (or just identified conjunct)
    [(abstract-atom s a) (identified-atom (abstract-atom s a) idx)]))

;; computes the skeleton for a generational graph, i.e. the data structure without any generations
(define (generational-graph-skeleton branch [uid-acc 1] [graph (unweighted-graph/directed (list))] [edges (list)])
  (match branch
    [(list label)
     (begin
       (foldl
        (match-lambda**
         [(conjunct (cons uid idx))
          (begin
            (add-vertex! graph (identify conjunct uid))
            (for ([edge edges])
              (when (contains (cdr edge) idx)
                (add-directed-edge! graph (car edge) (identify conjunct uid))))
            (cons (add1 uid) (add1 idx)))])
        (cons uid-acc 0)
        (label-conjunction label))
       graph)]
    ; case: tree-label followed by tree-label (which is always the case when there is a selection)
    ; TODO: combinations of tree-label, cycle, generalization, widening, and case split
    ; at the very least, tree label followed by generalization will be needed for demo
    [(list-rest (tree-label tl-con (some selected) tl-subst tl-rule tl-idx tl-edges) l-rest)
     (let* ([first-unselected (take tl-con selected)]
            [selected-atom (list-ref tl-con selected)]
            [last-unselected (drop tl-con (+ 1 selected))]
            [next-conjunction (label-conjunction (first l-rest))])
       (begin
         (match-let
             ([(list next-uid next-idx next-range-start add-edges)
               (foldl
                (match-lambda**
                 [(conjunct (list uid idx range-start introduced-edges))
                  (begin
                    (add-vertex! graph (identify conjunct uid))
                    (for ([edge edges])
                      (when (contains (cdr edge) idx)
                        (add-directed-edge! graph (car edge) (identify conjunct uid))))
                    (let ([vertex-edges (cons (identify conjunct uid) (if (not (equal? idx selected)) (index-range range-start (add1 range-start)) (index-range range-start (+ range-start (knowledge-output-length (tree-label-rule (car l-rest)))))))]
                          [new-range-start (if (not (equal? idx selected)) (add1 range-start) (+ range-start (knowledge-output-length (tree-label-rule (car l-rest)))))])
                      (list (add1 uid) (add1 idx) new-range-start (cons vertex-edges introduced-edges))))])
                (list uid-acc 0 0 (list))
                tl-con)])
           (generational-graph-skeleton l-rest next-uid graph add-edges))))]))

(module+ test
  (define branch (active-branch-info slbranch:val))
  (define sl-graph (generational-graph-skeleton branch)))
(define (active-branch-info t)
  (match t
    [(node (tree-label (list) _ _ _ _ _) '()) #f]
    [(node 'fail '()) #f]
    [(node (cycle _) '()) #f]
    [(node (tree-label c (none) s r #f ie) '())
     (list (tree-label c (none) s r #f ie))]
    [(node (tree-label c sel s r i ie) ch)
     (aif (foldl (λ (c acc) (if acc acc (active-branch-info c))) #f ch)
          (cons (tree-label c sel s r i ie) it)
          #f)]))
(provide
 (proc-doc/names
  active-branch-info
  (-> node? (or/c #f (listof tree-label?)))
  (tree)
  @{Returns the information required to synthesize a clause corresponding to the active branch in @racket[tree].
 The active branch is the first branch, considered from left to right, with a leaf node eligible for further unfolding.
 If there is no such leaf node in the tree, the result is @racket[#f].}))

;; checks if a1 renames a2 *and* whether any shared abstract variables occur in corresponding positions
(define (renames-with-corresponding-args? a1 a2)
  (define (as at) (list->set (map a (assemble-var-indices a? at))))
  (define (gs at) (list->set (map g (assemble-var-indices g? at))))
  (define shared
    (set-union (set-intersect (as a1) (as a2)) (set-intersect (gs a1) (gs a2))))
  (define subst (map (λ (v) (abstract-equality v (abstract-function (gensym "dummy") (list)))) (set->list shared)))
  (renames? (apply-substitution subst a1) (apply-substitution subst a2)))
(module+ test
  (require rackunit)
  (require "cclp-interpreter.rkt")
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
  (define just-atoms (map identified-atom-atom reached))
  (define root-atom (identified-atom-atom root))
  (ormap (λ (a) (renames-with-corresponding-args? root-atom a)) just-atoms))
(module+ test
  (check-true (descendant-renames-with-corresponding-args? sl-graph (identified-atom (abstract-atom 'collect (list (g 1) (a 1))) 2)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph (identified-atom (abstract-atom 'collect (list (g 2) (a 2))) 3)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph (identified-atom (abstract-atom 'eq (list (a 1) (a 2))) 4)))
  (check-false (descendant-renames-with-corresponding-args? sl-graph (identified-atom (abstract-atom 'sameleaves (list (g 1) (g 2))) 1))))

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
  (check-equal?
   (candidate-target-identified-atoms
    sl-graph
    (identified-atom (abstract-atom 'sameleaves (list (g 1) (g 2))) 1)
    3)
   (list (identified-atom (abstract-atom 'collect (list (g 1) (a 1))) 2))))
(provide
 (proc-doc/names
  candidate-target-identified-atoms
  (-> unweighted-graph? identified-atom? exact-positive-integer? (listof identified-atom?))
  (skeleton root live-depth)
  @{Finds potential target atoms for recursion analysis.
 The parameter @racket[skeleton] is a non-annotated recursion analysis,
 @racket[root] is the root of the rooted DAG,
 @racket[live-depth] indicates depth from which an atom may survive indefinitely.}))

(define (multiple-direct-live-lines? graph root live-depth curr-depth)
  (let ([children-reaching-live-depth
         (filter
          (λ (c) (can-reach-depth? c graph live-depth (+ curr-depth 1)))
          (reached-neighbors graph root))])
    (>= (length children-reaching-live-depth) 2)))
(provide
 (proc-doc/names
  multiple-direct-live-lines?
  (-> unweighted-graph? identified-atom? exact-nonnegative-integer? exact-nonnegative-integer? boolean?)
  (graph root live-depth current-depth)
  @{Tests whether @racket[root] in @racket[graph] has at least two children and whether both children have descendants at depth @racket[live-depth], when @racket[node] is at @racket[current-depth].}))

(define (reached-neighbors g v)
  (filter (λ (n) (has-edge? g v n)) (get-neighbors g v)))

(define (can-reach-depth? vertex graph target-depth curr-depth)
  (cond [(>= curr-depth target-depth) #t]
        [(null? (reached-neighbors graph vertex)) #f]
        [else
         (ormap
          (λ (c) can-reach-depth? c graph target-depth (+ curr-depth) 1)
          (reached-neighbors graph vertex))]))

(define (annotate-general! skeleton root relevant-targets rdag-depth)
  ;; annotate when the relevant target atom for a subgraph is known
  (define (annotate-specific-aux! skeleton aux-root relevant-target-atom rdag-depth depth-acc)
    (match-define (generation root-gen-number root-origin)
      (identified-atom-with-generation-generation aux-root))
    (define next-layer-gen-number
      (if (and
           (renames-with-corresponding-args?
            relevant-target-atom
            (identified-atom-atom (identified-atom-with-generation-id-atom aux-root)))
           (multiple-direct-live-lines? skeleton aux-root rdag-depth depth-acc))
          (add1 root-gen-number)
          root-gen-number))
    (define next-gen (generation next-layer-gen-number root-origin))
    (for ([c (reached-neighbors skeleton aux-root)])
      (let ([annotated-c (identified-atom-with-generation c next-gen)])
        (rename-vertex! skeleton c annotated-c)
        (annotate-specific-aux! skeleton annotated-c relevant-target-atom rdag-depth (add1 depth-acc)))))
  ;; annotate when the relevant target atom for a subgraph is not yet known
  (define (annotate-general-aux! skeleton aux-root relevant-targets rdag-depth depth-acc)
    (if (member (identified-atom-with-generation-id-atom aux-root) relevant-targets)
        (for ([c (reached-neighbors skeleton aux-root)])
          (let* ([root-id-atom (identified-atom-with-generation-id-atom aux-root)]
                 [origin (index-of relevant-targets root-id-atom)]
                 [annotated-c (identified-atom-with-generation c (generation 1 origin))])
            (rename-vertex! skeleton c annotated-c)
            (annotate-specific-aux! skeleton annotated-c (identified-atom-atom root-id-atom) rdag-depth (add1 depth-acc))))
        (for ([c (reached-neighbors skeleton aux-root)])
          (let ([annotated-c (identified-atom-with-generation c (generation 0 #f))])
            (rename-vertex! skeleton c annotated-c)
            (annotate-general-aux! skeleton annotated-c relevant-targets rdag-depth (add1 depth-acc))))))
  ;; start by giving top level generation 0
  (define annotated-root (identified-atom-with-generation root (generation 0 #f)))
  (rename-vertex! skeleton root annotated-root)
  (annotate-general-aux! skeleton annotated-root relevant-targets rdag-depth 1))

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
  (define sl-root (identified-atom (abstract-atom 'sameleaves (list (g 1) (g 2))) 1))
  (check-equal?
   (rdag-level sl-graph sl-root 1)
   (list sl-root))
  (check-equal?
   (sort (rdag-level sl-graph sl-root 2) < #:key identified-atom-uid)
   (list
    (identified-atom (abstract-atom 'collect (list (g 1) (a 1))) 2)
    (identified-atom (abstract-atom 'collect (list (g 2) (a 2))) 3)
    (identified-atom (abstract-atom 'eq (list (a 1) (a 2))) 4))))

;; replace sequences with the same origin with multi abstractions at a given level of the generational tree
(define (gen-tree-level->generalized-conjunction lvl)
  (map (compose1 identified-atom-atom identified-atom-with-generation-id-atom) lvl))
(module+ test
  (annotate-general! sl-graph sl-root (list (identified-atom (abstract-atom 'collect (list (g 1) (a 1))) 2)) (length branch))
  (check-equal?
   (gen-tree-level->generalized-conjunction (rdag-level sl-graph (identified-atom-with-generation sl-root (generation 0 #f)) 5))
   (list)) ; TODO: introduce a multi abstraction
  (check-equal?
   (gen-tree-level->generalized-conjunction
    (list
     (identified-atom-with-generation (identified-atom (abstract-atom 'integers (list (g 1) (a 1))) 3) (generation 0 #f))
     (identified-atom-with-generation (identified-atom (abstract-atom 'filterA (list (g 2) (a 1) (a 2))) 4) (generation 1 1))
     (identified-atom-with-generation (identified-atom (abstract-atom 'filterA (list (g 3) (a 2) (a 3))) 5) (generation 2 1))
     (identified-atom-with-generation (identified-atom (abstract-atom 'filterA (list (g 4) (a 3) (a 4))) 6) (generation 3 1))
     (identified-atom-with-generation (identified-atom (abstract-atom 'siftA (list (a 4) (a 5))) 7) (generation 3 1))
     (identified-atom-with-generation (identified-atom (abstract-atom 'filterB (list (g 5) (a 5) (a 6))) 8) (generation 1 2))
     (identified-atom-with-generation (identified-atom (abstract-atom 'filterB (list (g 6) (a 6) (a 7))) 9) (generation 2 2))
     (identified-atom-with-generation (identified-atom (abstract-atom 'filterB (list (g 7) (a 7) (a 8))) 10) (generation 3 2))
     (identified-atom-with-generation (identified-atom (abstract-atom 'siftB (list (a 8) (a 9))) 11) (generation 3 2))
     (identified-atom-with-generation (identified-atom (abstract-atom 'length (list (a 9) (a 10))) 12) (generation 0 #f))))
   (list))) ; TODO: introduce two multi abstractions

(define (generalize t) (cons t #f))
(module+ test
  (require (prefix-in primes5: "analysis-trees/primes-five.rkt"))
  (require (prefix-in generalizedslbranch: "analysis-trees/generalized-sameleaves-branch.rkt"))
  (check-equal? (generalize primes5:val) (cons primes5:val #f))
  (check-equal? (generalize slbranch:val) (cons generalizedslbranch:val #t)))
(provide
 (proc-doc/names
  generalize
  (-> node? (cons/c node? boolean?))
  (top)
  @{Attempts to generalize the candidate node in @racket[t].
 If generalization is successful, the first element of the returned pair is @racket[t], extended with a @racket[node?] whose label is a @racket[generalization?] and the second element is @racket[#t].
 Otherwise, the first element is @racket[t] and the second element is @racket[#f].}))