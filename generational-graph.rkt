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
            (add-vertex! graph (gen-node conjunct uid #f #f)) ; end of branch never has selection
            (for ([edge edges])
              (when (contains (cdr edge) idx)
                (add-directed-edge! graph (car edge) (gen-node conjunct uid #f #f))))
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
                  (add-vertex! graph (gen-node conjunct uid #f (equal? idx selected1)))
                  (for ([edge edges])
                    (when (contains (cdr edge) idx)
                      (add-directed-edge! graph (car edge) (gen-node conjunct uid #f (equal? idx selected1)))))
                  (let ([vertex-edges
                         (cons ; pair of the current conjunct and the range of "spawned" conjuncts
                          (gen-node conjunct uid #f (equal? idx selected1))
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
                (add-vertex! graph (gen-node conjunct uid #f #f))
                (for ([edge edges])
                  (when (contains (cdr edge) idx)
                    (add-directed-edge! graph (car edge) (gen-node conjunct uid #f #f))))
                (let ([vertex-edges
                       (cons
                        (gen-node conjunct uid #f #f)
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

(define (active-branch t)
  (match t
    [(node (tree-label (list) _ _ _ _ _) '()) #f]
    [(node 'fail '()) #f]
    [(node (cycle _) '()) #f]
    [(node (tree-label c (none) s r #f ie) '())
     (list (tree-label c (none) s r #f ie))]
    [(node (generalization c (none) #f ie rngs) '())
     (list (generalization c (none) #f ie rngs))]
    [(node (tree-label c sel s r i ie) ch)
     (aif (foldl (λ (c acc) (if acc acc (active-branch c))) #f ch)
          (cons (tree-label c sel s r i ie) it)
          #f)]
    [(node (generalization c sel i ie rngs) ch)
     (aif (foldl (λ (c acc) (if acc acc (active-branch c))) #f ch)
          (cons (generalization c sel i ie rngs) it)
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
(define (descendant-renames-with-corresponding-args? graph root)
  (define tc (transitive-closure graph))
  (hash-set! tc (list root root) #f)
  (define reached (map second (filter (λ (p) (and (hash-ref tc p) (equal? (first p) root))) (hash-keys tc))))
  (define just-atoms (map gen-node-conjunct reached))
  (define root-atom (gen-node-conjunct root))
  (ormap (λ (a) (and (abstract-atom? root-atom) (abstract-atom? a) (renames-with-corresponding-args? root-atom a))) just-atoms))
(module+ test
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 #f #t)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 3 #f #f)))
  (check-true (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'eq (list (a 1) (a 2))) 4 #f #f)))
  (check-false (descendant-renames-with-corresponding-args? sl-graph-skeleton (gen-node (abstract-atom 'sameleaves (list (g 1) (g 2))) 1 #f #t))))

;; finds potential target atoms for recursion analysis.
;; root is the root of the rooted DAG (i.e. the skeleton)
;; live-depth indicates depth from which an atom may survive indefinitely
(define (candidate-targets skeleton root live-depth)
  (define (candidate-targets-aux skeleton root live-depth [depth-acc 0])
    (if (>= depth-acc live-depth)
        (list)
        (let ([candidates-among-descendants
               (foldl
                (λ (c candidate-acc)
                  (append candidate-acc (candidate-targets-aux skeleton c live-depth (+ depth-acc 1))))
                (list)
                (get-neighbors skeleton root))])
          (if (and (multiple-direct-live-lines? skeleton root live-depth depth-acc)
                   (descendant-renames-with-corresponding-args? skeleton root))
              (list root)
              candidates-among-descendants))))
  (remove-duplicates (candidate-targets-aux skeleton root live-depth 0)))
(module+ test
  (define sl-skeleton-root
    (gen-node (abstract-atom 'sameleaves (list (g 1) (g 2))) 1 #f #t))
  (check-equal?
   (candidate-targets
    sl-graph-skeleton
    sl-skeleton-root
    3)
   (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 #f #t)))
  (define o-primes-skeleton-root (gen-node (abstract-atom 'oprimes (list (g 1) (a 1))) 1 #f #t))
  (define o-primes-candidate-targets
    (list
     (gen-node (abstract-atom 'siftA (list (abstract-function 'cons (list (g 2) (a 4))) (a 3))) 7 #f #t)
     (gen-node (abstract-atom 'siftB (list (abstract-function 'cons (list (g 2) (a 6))) (a 1))) 13 #f #t)))
  (check-equal?
   (candidate-targets
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
    [(_ _) (error "unexpected comparison of generations")]))
(module+ test
  (check-true (gen-number< 2 3))
  (check-false (gen-number< 3 2))
  (check-true (gen-number< 1000 'l))
  (check-false (gen-number< 'l 1000))
  (check-true (gen-number< (symsum 'l 0) (symsum 'l 10)))
  (check-false (gen-number< (symsum 'l 10) (symsum 'l 0))))

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
  (check-equal? (gen-gap (gen (symsum 'l1 3) 1) (gen 'l1 1)) 3))

;; minimum generation in the range of a conjunct
(define (local-min c)
  (match c
    [(gen-node _ _ (gen num _) _) num]
    [(gen-node _ _ (gen-range fst lst _ #t) _) fst]
    [(gen-node _ _ (gen-range fst lst _ #f) _) lst]))
(module+ test
  (check-equal?
   (local-min (gen-node (multi (list) #f (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 'l1 1 1 #f) #f))
   1)
  (check-equal?
   (local-min (gen-node (multi (list) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l1 1 #t) #f))
   1))

(define (local-max c)
  (match c
    [(gen-node _ _ (gen num _) _) num]
    [(gen-node _ _ (gen-range fst lst _ #t) _) lst]
    [(gen-node _ _ (gen-range fst lst _ #f) _) fst]))
(module+ test
  (check-equal?
   (local-max (gen-node (multi (list) #f (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 'l1 1 1 #f) #f))
   'l1)
  (check-equal?
   (local-max (gen-node (multi (list) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l1 1 #t) #f))
   'l1))

(define (increment-rel-tg-unfolding! id-conjunct ann-parent relevant-targets graph)
  (match-define (gen-node parent-conjunct parent-id parent-gen parent-unfolded?) ann-parent)
  (cond
    [(member ann-parent relevant-targets)
     (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range (gen 1 parent-id)]))]
    [(and
      (abstract-atom? parent-conjunct)
      (findf (λ (rel-tg) (and (equal? (gen-origin parent-gen) (gen-node-id rel-tg)) (renames-with-corresponding-args? parent-conjunct (gen-node-conjunct rel-tg)))) relevant-targets))
     (if parent-unfolded? ; if parent renames a relevant target atom, it cannot be a multi!
         (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range (gen (add1 (gen-number parent-gen)) (gen-origin parent-gen))]))
         (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range parent-gen])))]
    [else
     (rename-vertex! graph id-conjunct (struct-copy gen-node id-conjunct [range parent-gen]))]))
; TODO this needs separate tests!

(define (apply-multi-mapping! spc parent multi-mapping graph)
  (define (translate-gen parent-gen original-gen symbolic-gen)
    (define gap (gen-gap parent-gen original-gen))
    (cond
      [(and gap (equal? gap 0)) (struct-copy gen parent-gen [number symbolic-gen])]
      [(and gap (> gap 0)) (struct-copy gen parent-gen [number (symsum symbolic-gen gap)])]
      [else #f]))
  (define parent-gens (gen-node-range parent))
  (define (apply-single! original-gen symbolic-gen)
    (cond
      [(gen? parent-gens)
       (let ([translation  (translate-gen parent-gens original-gen symbolic-gen)])
         (when translation (rename-vertex! graph spc (struct-copy gen-node spc [range translation]))))]
      [(gen-range? parent-gens)
       (let ([translation1 (translate-gen (gen (gen-range-first parent-gens) (gen-range-origin parent-gens)) original-gen symbolic-gen)]
             [translation2 (translate-gen (gen (gen-range-last parent-gens) (gen-range-origin parent-gens)) original-gen symbolic-gen)])
         ;; there can be at most one translation
         (when translation1 (rename-vertex! graph spc (struct-copy gen-node spc [range (struct-copy gen-range parent-gens [first (gen-number translation1)])])))
         (when translation2 (rename-vertex! graph spc (struct-copy gen-node spc [range (struct-copy gen-range parent-gens [last (gen-number translation2)])]))))]))
  (for ([key (hash-keys multi-mapping)])
    (apply-single! key (hash-ref multi-mapping key)))
  ;; if the graph still has spc at this point, no mapping took place
  (when (has-vertex? graph spc)
    (rename-vertex! graph spc (struct-copy gen-node spc [range parent-gens]))))
(module+ test)

;; annotates a level of the RDAG, other than the root level
;; TODO: parent-level-number is completely redundant? it is just the current level - 1...
(define (annotate-level! graph annotated-root l-postfix relevant-targets parent-level-number level-number)
  (define parent-level (rdag-level graph annotated-root parent-level-number))
  (define level (rdag-level graph annotated-root level-number))
  (match-define-values
   (new-multis single-parent-conjuncts)
   (partition (λ (conjunct) (> (length (get-neighbors (transpose graph) conjunct)) 1)) level))
  (define multi-mapping (foldl (curry annotate-new-multi! graph l-postfix) (make-immutable-hash) new-multis))
  (for ([spc single-parent-conjuncts])
    (let ([parent (first (get-neighbors (transpose graph) spc))])
      (if (null? new-multis)
          (increment-rel-tg-unfolding! spc parent relevant-targets graph)
          (apply-multi-mapping! spc parent multi-mapping graph)))))
(module+ test
  (require (prefix-in almost-annotated: "analysis-trees/sameleaves-multi-branch-gen-tree-almost-annotated.rkt"))
  (define almost-annotated (graph-copy almost-annotated:val))
  (define sl-annotated-root (struct-copy gen-node sl-skeleton-root [range (gen 0 #f)]))
  (require
    (prefix-in sl-multi-graph-skeleton: "analysis-trees/sameleaves-multi-branch-gen-tree-skeleton.rkt")
    (prefix-in sl-multi-graph-annotated: "analysis-trees/sameleaves-multi-branch-gen-tree.rkt"))
  (define sl-multi-graph-annotated (graph-copy sl-multi-graph-skeleton:val))
  (rename-vertex! sl-multi-graph-annotated sl-skeleton-root sl-annotated-root)
  (annotate-level! sl-multi-graph-annotated sl-annotated-root 1 (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) 1 2)
  (check-equal?
   (rdag-level sl-multi-graph-annotated sl-annotated-root 2)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 2))
  (annotate-level! sl-multi-graph-annotated sl-annotated-root 1 (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) 2 3)
  (check-equal?
   (rdag-level sl-multi-graph-annotated sl-annotated-root 3)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 3))
  (annotate-level! sl-multi-graph-annotated sl-annotated-root 1 (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) 3 4)
  (check-equal?
   (rdag-level sl-multi-graph-annotated sl-annotated-root 4)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 4))
  (annotate-level! almost-annotated sl-annotated-root 1 (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) 5 6)
  (check-equal?
   (rdag-level almost-annotated sl-annotated-root 6)
   (rdag-level sl-multi-graph-annotated:val sl-annotated-root 6)))

(define (annotate-new-multi! graph l-postfix new-multi mapping)
  (define parents (get-neighbors (transpose graph) new-multi))
  (define bare-multi (gen-node-conjunct new-multi))
  (define parent-minimum (apply (curry generic-minmax gen-number<) (map local-min parents)))
  (define parent-maximum (apply (curry generic-minmax (compose not gen-number<)) (map local-max parents)))
  (define parent-origin (let ([parent-gen (gen-node-range (first parents))]) (if (gen? parent-gen) (gen-origin parent-gen) (gen-range-origin parent-gen)))) ; first, but could pick any one
  (define symbolic-maximum (string->symbol (format "l~a" l-postfix)))
  (define multi-parent (findf (λ (p) (multi? (gen-node-conjunct p))) parents))
  (define ascending?
    (if multi-parent
        ((compose1 gen-range-ascending? gen-node-range) multi-parent)
        (< (gen-number (gen-node-range (first parents))) (gen-number (gen-node-range (last parents))))))
  (define range
    (if ascending?
        (gen-range parent-minimum symbolic-maximum parent-origin ascending?)
        (gen-range symbolic-maximum parent-minimum parent-origin ascending?)))
  (set! l-postfix (add1 l-postfix))
  (define updated-multi (struct-copy gen-node new-multi [range range]))
  (rename-vertex! graph new-multi updated-multi)
  (hash-set mapping (gen parent-maximum parent-origin) symbolic-maximum))
; TODO add tests for o-primes
(module+ test
  (set! almost-annotated (graph-copy almost-annotated:val))
  (require (prefix-in almost-annotated-m: "analysis-trees/sameleaves-multi-branch-gen-tree-almost-annotated-with-multi.rkt"))
  (define almost-annotated-with-multi (graph-copy almost-annotated-m:val))
  (annotate-new-multi! almost-annotated 1 (list-ref (sort (rdag-level almost-annotated sl-annotated-root 6) < #:key gen-node-id) 3) (make-immutable-hash))
  (for ([lv (range 1 6)])
    (check-equal?
     (rdag-level almost-annotated sl-annotated-root lv)
     (rdag-level almost-annotated-with-multi sl-annotated-root lv)))
  (check-equal?
   (rdag-level almost-annotated sl-annotated-root 6)
   (rdag-level almost-annotated-with-multi sl-annotated-root 6))
  (check-equal? almost-annotated almost-annotated-with-multi))

;; note: this takes a skeleton as an input, but it modifies it so that it becomes a full generational graph
; TODO: add some really tough cases
(define (annotate-general! skeleton root relevant-targets rdag-depth)
  (define l-postfix 1)
  (define annotated-root (struct-copy gen-node root [range (gen 0 #f)]))
  (rename-vertex! skeleton root annotated-root)
  (map
   (curry annotate-level! skeleton annotated-root l-postfix relevant-targets)
   (range 1 rdag-depth)
   (range 2 (add1 rdag-depth))))
(module+ test
  (require
    (prefix-in sl-graph-annotated: "analysis-trees/sameleaves-no-multi-branch-gen-tree.rkt"))
  (define sl-graph-annotated (graph-copy sl-graph-skeleton))
  (annotate-general! sl-graph-annotated sl-skeleton-root (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) (length sl-branch))
  (check-equal?
   sl-graph-annotated
   sl-graph-annotated:val)
  (require
    (prefix-in sl-multi-branch-tree: "analysis-trees/sameleaves-multi-branch.rkt"))
  (define sl-multi-branch (active-branch sl-multi-branch-tree:val))
  (set! sl-multi-graph-annotated (graph-copy sl-multi-graph-skeleton:val))
  (annotate-general! sl-multi-graph-annotated sl-skeleton-root (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) (length sl-multi-branch))
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
   o-primes-graph-annotated:val)
  (require (prefix-in fake-primes-skeleton: "analysis-trees/fake-primes-gen-graph-skeleton.rkt"))
  (require (prefix-in fake-primes-annotated: "analysis-trees/fake-primes-gen-graph.rkt"))
  (define fake-primes-annotated (graph-copy fake-primes-skeleton:val))
  (define fake-primes-root (gen-node (abstract-atom 'fakeprimes (list (g 3) (a 2))) 1 #f #t))
  (annotate-general! fake-primes-annotated fake-primes-root (abstract-atom 'sift (list (abstract-function 'cons (list (g 2) (a 1))) (a 2))) 19)
  (check-equal?
   fake-primes-annotated
   fake-primes-annotated:val)
  (require (prefix-in sameleaves-extended-skeleton: "analysis-trees/sameleaves-multi-branch-gen-tree-extended-1-skeleton.rkt"))
  (require (prefix-in sameleaves-extended: "analysis-trees/sameleaves-multi-branch-gen-tree-extended-1.rkt"))
  (define sameleaves-extended-annotated (graph-copy sameleaves-extended-skeleton:val))
  (annotate-general! sameleaves-extended-annotated sl-skeleton-root (list (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 0 #f) #t)) 16))

;; extract a level from a rooted DAG
;; the lowest level that can be extracted is 1 (the root)
;; no guarantees about how the result is sorted, but it does not contain duplicates
(define (rdag-level rdag root level)
  (define (rdag-level-aux rdag root level depth-acc)
    (if (eqv? depth-acc level)
        (list root)
        (apply append (map (λ (s) (rdag-level-aux rdag s level (add1 depth-acc))) (get-neighbors rdag root)))))
  (remove-duplicates (rdag-level-aux rdag root level 1)))
;(module+ test
;  (check-equal?
;   (rdag-level sl-graph-skeleton sl-skeleton-root 1)
;   (list sl-skeleton-root))
;  (check-equal?
;   (sort (rdag-level sl-graph-skeleton sl-skeleton-root 2) < #:key identified-abstract-conjunct-id-number)
;   (list
;    (identified-abstract-conjunct (abstract-atom 'collect (list (g 1) (a 1))) 2)
;    (identified-abstract-conjunct (abstract-atom 'collect (list (g 2) (a 2))) 3)
;    (identified-abstract-conjunct (abstract-atom 'eq (list (a 1) (a 2))) 4))))

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