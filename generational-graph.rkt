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
                     (generation-origin obj)))))])
(provide
 (struct*-doc
  generation
  ([number (or/c exact-positive-integer? (cons/c symbol? exact-integer?))]
   [origin exact-positive-integer?])
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
  (define graph (generational-graph-skeleton branch))
  (display (graphviz graph)))

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

;(define (annotate-generational-tree tree target-identified-atom generation-acc live-depth depth-acc)
;  (match tree
;    [(node id-atom-label (list))
;     (node (identified-atom-with-generation id-atom-label generation-acc) (list))]
;    [(node id-atom-label (list single-elem))
;     (node (identified-atom-with-generation id-atom-label generation-acc)
;           (list
;            (annotate-generational-tree single-elem target-identified-atom generation-acc live-depth (add1 depth-acc))))]
;    [(node atom-label (list-rest h t))
;     #:when (and (or (equal? (identified-atom-uid target-identified-atom) (identified-atom-uid atom-label))
;                     (and (renames?
;                           (identified-atom-atom target-identified-atom)
;                           (identified-atom-atom atom-label))
;                          (> generation-acc 0)))
;                 (multiple-direct-live-lines? (node atom-label (cons h t)) live-depth depth-acc))
;     (node (identified-atom-with-generation atom-label generation-acc)
;           (map
;            (λ (subtree)
;              (annotate-generational-tree subtree target-identified-atom (add1 generation-acc) live-depth (add1 depth-acc)))
;            (cons h t)))]
;    [(node atom-label (list-rest h t))
;     (node (identified-atom-with-generation atom-label generation-acc)
;           (map
;            (λ (subtree)
;              (annotate-generational-tree subtree target-identified-atom generation-acc live-depth (add1 depth-acc)))
;            (cons h t)))]))
;(provide
; (proc-doc/names
;  annotate-generational-tree
;  (-> node? identified-atom? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? node?)
;  (raw-tree target-atom gen-acc live-depth depth-acc)
;  @{Assign generation numbers to each atom in an unnumbered generational tree @racket[raw-tree].
; The generation is incremented on unfolding a renaming of @racket[target-atom], but the first increment only occurs for the exact instance of the target atom.
; The generation is tracked using @racket[gen-acc].
; If an atom is still present at depth @racket[live-depth], the analysis does not provide proof that it will be dealt with in the future.
; The value @racket[depth-acc] is used to track the depth at which atoms occur.
; }))
; TODO should use default arguments 0 for gen-acc and depth-acc

; TODO test candidate-target-atoms
;
(define (descendant-renames? graph root)
  (define tc (transitive-closure graph))
  (hash-set! tc (list root root) #f)
  (define reached (filter (λ (p) (and (hash-ref tc p) (equal? (first p) root))) (hash-keys tc)))
  (define just-atoms (map identified-atom-atom reached))
  (define root-atom (identified-atom-atom root))
  (ormap (λ (a) (renames? root-atom a)) just-atoms))

(define (candidate-target-identified-atoms skeleton root live-depth)
  (define (candidate-target-identified-atoms-aux skeleton root live-depth [depth-acc 0])
    (if (>= depth-acc live-depth)
        (list)
        (let ([candidates-among-descendants
               (foldl
                (λ (c candidate-acc)
                  (append candidate-acc (candidate-target-identified-atoms-aux skeleton c live-depth (+ depth-acc 1))))
                (if (and (multiple-direct-live-lines? skeleton root live-depth depth-acc)
                         (descendant-renames? skeleton root))
                    (list (node-label skeleton))
                    (list))
                (reached-neighbors skeleton root))])
          (if (and (multiple-direct-live-lines? skeleton root live-depth depth-acc)
                   (descendant-renames? skeleton root))
              (cons root candidates-among-descendants)
              candidates-among-descendants))))
  (remove-duplicates (candidate-target-identified-atoms-aux skeleton root live-depth 0)))
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
        [(null? (reached-neighbors vertex graph)) #f]
        [else
         (ormap
          (λ (c) can-reach-depth? c graph target-depth (+ curr-depth) 1)
          (reached-neighbors vertex graph))]))