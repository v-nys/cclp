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
(require racket-tree-utils/src/tree)
(require "abstract-knowledge.rkt")
(require "abstract-multi-domain.rkt")
(require "data-utils.rkt")
(require "abstract-domain-ordering.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/manual))
(require "abstract-analysis.rkt")
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require racket-list-utils/utils)

(define (write-atom-with-generation obj port mode)
  (if (boolean? mode)
      (fprintf
       port
       "#(struct:identified-atom-with-generation ~s ~s)"
       (identified-atom-with-generation-id-atom obj)
       (identified-atom-with-generation-generation obj))
      (begin (fprintf port "~v" (identified-atom-with-generation-id-atom obj))
             (fprintf port ";")
             (fprintf port "~v" (identified-atom-with-generation-generation obj)))))
(struct identified-atom-with-generation (id-atom generation)
  #:methods
  gen:custom-write [(define write-proc write-atom-with-generation)]
  #:methods
  gen:equal+hash
  [(define (equal-proc a1 a2 equal?-recur)
     (and (equal?-recur (identified-atom-with-generation-id-atom a1) (identified-atom-with-generation-id-atom a2))
          (equal?-recur (identified-atom-with-generation-generation a1) (identified-atom-with-generation-generation a2))))
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
   [generation exact-nonnegative-integer?])
  @{The label of a node in a generational tree.
     The field @racket[atom] is a single identified atom which, juxtaposed with other @racket[identified-atom]s at the same depth, forms an abstract conjunction encountered during analysis.
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

(struct identified-atom (atom uid) #:transparent)
(provide
 (struct*-doc
  identified-atom
  ([atom abstract-atom?]
   [uid exact-nonnegative-integer?])
  @{A uniquely identifiable instance of an abstract atom in a generational tree (or skeleton).
     The field @racket[uid] is unique to each atom in a generational tree.}))

; FIXME ID's will not be unique?
(define (generational-tree-skeleton branch [uid-acc 1])
  (match branch
    [(list label)
     (car
      (map-accumulatel
       (λ (atom acc) (cons (node (identified-atom atom acc) '()) (add1 acc)))
       uid-acc
       (tree-label-conjunction label)))]
    [(list-rest (tree-label tl-con (some selected) tl-subst tl-r tl-i) tl-rest)
     (let* ([first-unselected (take tl-con selected)]
            [selected-atom (list-ref tl-con selected)]
            [last-unselected (drop tl-con (+ 1 selected))]
            [next-layer (generational-tree-skeleton tl-rest (+ uid-acc (length tl-con)))]
            [first-successors (take next-layer selected)]
            [selected-successors
             (take (drop next-layer selected) (knowledge-output-length (tree-label-rule (car tl-rest))))]
            [last-successors (drop next-layer (+ selected (knowledge-output-length (tree-label-rule (car tl-rest)))))])
       (append (car
                (foldr
                 (λ (pre post acc)
                   (cons
                    (cons (node (identified-atom pre (cdr acc)) (list post)) (car acc))
                    (sub1 (cdr acc))))
                 (cons '() (- (+ uid-acc selected) 1))
                 first-unselected
                 first-successors))
               (list (node (identified-atom selected-atom (+ uid-acc selected)) selected-successors))
               (car
                (foldr
                 (λ (pre post acc)
                   (cons
                    (cons (node (identified-atom pre (cdr acc)) (list post)) (car acc))
                    (sub1 (cdr acc))))
                 (cons '() (+ uid-acc selected (length last-unselected)))
                 last-unselected
                 last-successors))))]
    [_ (error "unforeseen pattern for generational-tree-skeleton")]))
(provide
 (proc-doc/names
  generational-tree-skeleton
  (-> (listof tree-label?) (listof node?)) ; more specifically: nodes of identified-atoms
  (branch)
  @{Computes the lineage of each atom on a branch, without numbering the generations of atoms.
 The result is a @racket[list], as each atom of the starting conjunction has its own lineage and lines are never joined --- they can only split.}))

(define (annotate-generational-trees skeleton-tree live-depth)
  (let ([candidates (candidate-target-identified-atoms skeleton-tree live-depth)])
    (map (λ (c) (annotate-generational-tree skeleton-tree c 0 live-depth 0)) candidates)))

(define (active-branch-info t)
  (match t
    [(node (tree-label (list) _ _ _ _) '()) #f]
    [(node 'fail '()) #f]
    [(node (cycle _) '()) #f]
    [(node (similarity-cycle _) '()) #f]
    [(node (tree-label c (none) s r #f) '())
     (list (tree-label c (none) s r #f))]
    [(node (tree-label c sel s r i) ch)
     (let ([first-child-branch (foldl (λ (c acc) (if acc acc (active-branch-info c))) #f ch)])
       (if first-child-branch
           (cons (tree-label c sel s r i) first-child-branch)
           #f))]))

(provide
 (proc-doc/names
  active-branch-info
  (-> node? (or/c #f (listof tree-label?)))
  (tree)
  @{Returns the information required to synthesize a clause corresponding to the active branch in @racket[tree].
 The active branch is the first branch, considered from left to right, with a leaf node eligible for further unfolding.
 If there is no such leaf node in the tree, the result is @racket[#f].}))

(define (annotate-generational-tree tree target-identified-atom generation-acc live-depth depth-acc)
  (match tree
    [(node id-atom-label (list))
     (node (identified-atom-with-generation id-atom-label generation-acc) (list))]
    [(node id-atom-label (list single-elem))
     (node (identified-atom-with-generation id-atom-label generation-acc)
           (list
            (annotate-generational-tree single-elem target-identified-atom generation-acc live-depth (add1 depth-acc))))]
    [(node atom-label (list-rest h t))
     #:when (and (or (equal? (identified-atom-uid target-identified-atom) (identified-atom-uid atom-label))
                     (and (renames?
                           (identified-atom-atom target-identified-atom)
                           (identified-atom-atom atom-label))
                          (> generation-acc 0)))
                 (multiple-direct-live-lines? (node atom-label (cons h t)) live-depth depth-acc))
     (node (identified-atom-with-generation atom-label generation-acc)
           (map
            (λ (subtree)
              (annotate-generational-tree subtree target-identified-atom (add1 generation-acc) live-depth (add1 depth-acc)))
            (cons h t)))]
    [(node atom-label (list-rest h t))
     (node (identified-atom-with-generation atom-label generation-acc)
           (map
            (λ (subtree)
              (annotate-generational-tree subtree target-identified-atom generation-acc live-depth (add1 depth-acc)))
            (cons h t)))]))
(provide
 (proc-doc/names
  annotate-generational-tree
  (-> node? identified-atom? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? node?)
  (raw-tree target-atom gen-acc live-depth depth-acc)
  @{Assign generation numbers to each atom in an unnumbered generational tree @racket[raw-tree].
 The generation is incremented on unfolding a renaming of @racket[target-atom], but the first increment only occurs for the exact instance of the target atom.
 The generation is tracked using @racket[gen-acc].
 If an atom is still present at depth @racket[live-depth], the analysis does not provide proof that it will be dealt with in the future.
 The value @racket[depth-acc] is used to track the depth at which atoms occur.
 }))
; TODO should use default arguments 0 for gen-acc and depth-acc

; TODO test candidate-target-atoms

(define (descendant-renames? n atom)
  (or (ormap (λ (c) (renames? atom (node-label c))) (node-children n))
      (ormap (λ (c) (descendant-renames? c atom)) (node-children n))))
(provide
 (proc-doc/names
  descendant-renames?
  (-> node? abstract-atom? boolean?)
  (node abstract-atom)
  @{Checks whether any descendant of @racket[node] (not node @racket[node] itself) has a label which renames @racket[abstract-atom].}))

(define (candidate-target-identified-atoms skeleton live-depth)
  (define (candidate-target-identified-atoms-aux skeleton live-depth [depth-acc 0])
    (if (>= depth-acc live-depth)
        (list)
        (let ([candidates-among-descendants
               (foldl
                (λ (c candidate-acc)
                  (append candidate-acc (candidate-target-identified-atoms-aux c live-depth (+ depth-acc 1))))
                (if (and (multiple-direct-live-lines? skeleton live-depth depth-acc)
                         (descendant-renames? (node-map identified-atom-atom skeleton) (identified-atom-atom (node-label skeleton))))
                    (list (node-label skeleton))
                    (list))
                (node-children skeleton))])
          (if (and (multiple-direct-live-lines? skeleton live-depth depth-acc)
                   (descendant-renames? (node-map identified-atom-atom skeleton) (identified-atom-atom (node-label skeleton))))
              (cons (node-label skeleton) candidates-among-descendants)
              candidates-among-descendants))))
  (remove-duplicates (candidate-target-identified-atoms-aux skeleton live-depth 0)))
(provide
 (proc-doc/names
  candidate-target-identified-atoms
  (-> node? exact-positive-integer? (listof identified-atom?))
  (skeleton live-depth)
  @{Finds potential target atoms for recursion analysis.
 The parameter @racket[skeleton] is a non-annotated recursion analysis,
 @racket[live-depth] indicates depth from which an atom may survive indefinitely.}))

; TODO test
(define (multiple-direct-live-lines? my-node live-depth curr-depth)
  (let ([children-reaching-live-depth
         (filter
          (λ (c) (can-reach-depth? c live-depth (+ curr-depth 1)))
          (node-children my-node))])
    (>= (length children-reaching-live-depth) 2)))
(provide
 (proc-doc/names
  multiple-direct-live-lines?
  (-> node? exact-nonnegative-integer? exact-nonnegative-integer? boolean?)
  (node live-depth current-depth)
  @{Tests whether @racket[node] has at least two children and whether both children have descendants at depth @racket[live-depth], when @racket[node] is at @racket[current-depth].}))

(define (can-reach-depth? my-node target-depth curr-depth)
  (cond [(>= curr-depth target-depth) #t]
        [(null? (node-children my-node)) #f]
        [else
         (ormap
          (λ (c) can-reach-depth? c target-depth (+ curr-depth) 1)
          (node-children my-node))]))

(define (generational-trees branch)
  (define skeleton (generational-tree-skeleton branch))
  (annotate-generational-trees
   (car skeleton) ; the assumption here is that the branch starts with a single atom
   (- (length branch) 1)))
; can refine this further:
; first resolution-info should have an atomic query
; not having a selection-and-clause and having a successor list element would also be a violation
; +vice versa
(provide
 (proc-doc/names
  generational-trees
  (-> (non-empty-listof tree-label?) (listof node?))
  (branch)
  @{Computes all potentially interesting generational trees for @racket[branch].
 All generational trees for a branch have the same skeleton, but potentially interesting ones are those for which a target atom can be found which is recursively evaluated.}))

(define-syntax (generational-atom-arg-bp stx)
  (syntax-case stx (a g)
    [(_ (a IDX))
     #'(a IDX)]
    [(_ (g IDX))
     #'(g IDX)]
    [(_ SYM)
     #'(abstract-function 'SYM '())]
    [(_ (SYM ARG ...))
     #'(abstract-function 'SYM (list (generational-atom-arg-bp ARG) ...))]))
(define-syntax (skeleton-bp stx)
  (syntax-case stx ()
    [(_ (SYM (ARG ...) ID TREE ...))
     #'(node
        (identified-atom (abstract-atom 'SYM (list (generational-atom-arg-bp ARG) ...)) ID)
        (list (skeleton-bp TREE) ...))]))
(define-syntax (no-id-skeleton-bp stx)
  (syntax-case stx ()
    [(_ (SYM (ARG ...) TREE ...))
     #'(node
        (abstract-atom 'SYM (list (generational-atom-arg-bp ARG) ...))
        (list (no-id-skeleton-bp TREE) ...))]))
(define-syntax (generational-tree-bp stx)
  (syntax-case stx ()
    [(_ (SYM (ARG ...) ID GEN TREE ...))
     #'(node
        (identified-atom-with-generation (identified-atom (abstract-atom 'SYM (list (generational-atom-arg-bp ARG) ...)) ID) GEN)
        (list (generational-tree-bp TREE) ...))]
    [(_ (SYM ID GEN TREE ...))
     #'(node
        (identified-atom-with-generation (identified-atom (abstract-atom 'SYM '()) ID) GEN)
        (list (generational-tree-bp TREE) ...))]))
(provide generational-tree-bp)

(module+ test
  (require rackunit)
  (require "cclp-interpreter.rkt")

  (test-case
   "Extracting the active branch from a tree."
   (let* ([completed-leaf (node (tree-label '() (none) '() (interpret-concrete-rule "foo") #f) '())]
          [parent
           (node
            (tree-label (interpret-abstract-conjunction "foo") (some 0) '() #f 1)
            (list completed-leaf completed-leaf))])
     (check-equal? (active-branch-info parent) #f "all branches have been completed"))
   (let* ([completed-leaf (node (tree-label '() (none) '() (interpret-concrete-rule "bar") #f) '())]
          [left-child
           (node
            (tree-label
             (interpret-abstract-conjunction "bar") (some 0) '() (interpret-concrete-rule "foo :- bar") 2)
            (list completed-leaf))]
          [active-leaf
           (node
            (tree-label (interpret-abstract-conjunction "baz") (none) '() (interpret-concrete-rule "quux :- baz") #f)
            '())]
          [right-child
           (node
            (tree-label
             (interpret-abstract-conjunction "quux") (some 0) '() (interpret-concrete-rule "foo :- quux") 3)
            (list active-leaf))]
          [root
           (node
            (tree-label
             (interpret-abstract-conjunction "foo") (some 0) '() #f 1)
            (list left-child right-child))]
          [info1
           (tree-label (interpret-abstract-conjunction "foo") (some 0) (list) #f 1)]
          [info2
           (tree-label (interpret-abstract-conjunction "quux") (some 0) (list) (interpret-concrete-rule "foo :- quux") 3)]
          [info3
           (tree-label (interpret-abstract-conjunction "baz") (none) (list) (interpret-concrete-rule "quux :- baz") #f)])
     (check-equal? (active-branch-info root) (list info1 info2 info3))))

  (test-case
   "the skeleton is computed correctly based on a branch of several nodes"
   (define-values (atom-a atom-b atom-c atom-d atom-e atom-f)
     (values (interpret-abstract-atom "a")
             (interpret-abstract-atom "b")
             (interpret-abstract-atom "c")
             (interpret-abstract-atom "d")
             (interpret-abstract-atom "e")
             (interpret-abstract-atom "f")))
   (define-values (a-clause b-clause c-clause)
     (values (interpret-concrete-rule "a :- b, c")
             (interpret-concrete-rule "b :- d, e")
             (interpret-concrete-rule "c :- f")))
   (let* ([level-3-analysis
           (node
            (tree-label (interpret-abstract-conjunction "d,e,f") (none) (list) c-clause #f) '())]
          [level-2-analysis
           (node
            (tree-label (interpret-abstract-conjunction "d,e,c") (some 2) (list) b-clause 3)
            (list level-3-analysis))]
          [level-1-analysis
           (node
            (tree-label (interpret-abstract-conjunction "b,c") (some 0) (list) a-clause 2)
            (list level-2-analysis))]
          [level-0-analysis
           (node
            (tree-label (interpret-abstract-conjunction "a") (some 0) (list) #f 1)
            (list level-1-analysis))]
          [branch (active-branch-info level-0-analysis)]
          [level-2-skeleton-1 (node (identified-atom atom-d 4) (list (node (identified-atom atom-d 7) '())))]
          [level-2-skeleton-2 (node (identified-atom atom-e 5) (list (node (identified-atom atom-e 8) '())))]
          [level-2-skeleton-3 (node (identified-atom atom-c 6) (list (node (identified-atom atom-f 9) '())))]
          [level-1-skeleton-1 (node (identified-atom atom-b 2) (list level-2-skeleton-1 level-2-skeleton-2))]
          [level-1-skeleton-2 (node (identified-atom atom-c 3) (list level-2-skeleton-3))]
          [level-0-skeleton (node (identified-atom atom-a 1) (list level-1-skeleton-1 level-1-skeleton-2))]
          [outcome (generational-tree-skeleton branch)])
     (check-equal? outcome (list level-0-skeleton))))

  (let* ([level-3
          (node
           (tree-label
            (interpret-abstract-conjunction "b,b,a") (none) (list) (interpret-concrete-rule "a :- b,a") #f)
           '())]
         [level-2
          (node
           (tree-label
            (interpret-abstract-conjunction "b,a") (some 1) (list) (interpret-concrete-rule "a :- b,a") 2)
           (list level-3))]
         [level-1
          (node
           (tree-label (interpret-abstract-conjunction "a") (some 0) (list) #f 1)
           (list level-2))])
    (test-case
     "generational tree for analysis tree a - b,a - b,b,a"
     (check-equal?
      (generational-tree-skeleton (active-branch-info level-1))
      (list
       (skeleton-bp
        (a () 1
           (b () 2
              (b () 4))
           (a () 3
              (b () 5)
              (a () 6)))))
      "check if the skeleton is correct")
     (let ([gen-trees (generational-trees (active-branch-info level-1))])
       (check-equal?
        gen-trees
        (list
         (generational-tree-bp
          (a 1 0
             (b 2 1
                (b 4 1))
             (a 3 1
                (b 5 2)
                (a 6 2))))
         (generational-tree-bp
          (a 1 0
             (b 2 0
                (b 4 0))
             (a 3 0
                (b 5 1)
                (a 6 1)))))
        "check if analysis without a target atom is correct"))))

  (check-equal?
   (descendant-renames?
    (no-id-skeleton-bp
     (a ()
        (b ())
        (a ())))
    (abstract-atom 'a '()))
   #t)

  (check-equal?
   (descendant-renames?
    (no-id-skeleton-bp
     (b ()
        (b ())
        (a ())))
    (abstract-atom 'a '()))
   #t)

  (check-equal?
   (descendant-renames?
    (no-id-skeleton-bp
     (b ()
        (a ())
        (a ())))
    (abstract-atom 'b '()))
   #f)

  (check-equal?
   (descendant-renames?
    (no-id-skeleton-bp
     (a ()
        (b ())
        (c ()
           (a ()))))
    (abstract-atom 'a '()))
   #t)

  (test-case
   "annotating skeletons"
   (check-equal?
    (annotate-generational-tree
     (skeleton-bp
      (collect ((g 1) (a 1)) 1
               (collect ((g 2) (a 2)) 2
                        (collect ((g 4) (a 4)) 5
                                 (collect ((g 6) (a 6)) 10)
                                 (collect ((g 7) (a 7)) 11)
                                 (append ((a 6) (a 7) (a 4)) 12))
                        (collect ((g 5) (a 5)) 6
                                 (collect ((g 5) (a 5)) 13))
                        (append ((a 4) (a 5) (a 2)) 7
                                (append ((a 4) (a 5) (a 2)) 14)))
               (collect ((g 3) (a 3)) 3
                        (collect ((g 3) (a 3)) 8
                                 (collect ((g 3) (a 3)) 15)))
               (append ((a 2) (a 3) (a 1)) 4
                       (append ((a 2) (a 3) (a 1)) 9
                               (append ((a 2) (a 3) (a 1)) 16)))))
     (identified-atom (abstract-atom 'collect (list (g 2) (a 2))) 2)
     0
     3
     0)
    (generational-tree-bp
     (collect ((g 1) (a 1))
              1
              0
              (collect ((g 2) (a 2))
                       2
                       0
                       (collect ((g 4) (a 4))
                                5
                                1
                                (collect ((g 6) (a 6)) 10 2)
                                (collect ((g 7) (a 7)) 11 2)
                                (append ((a 6) (a 7) (a 4)) 12 2))
                       (collect ((g 5) (a 5))
                                6
                                1
                                (collect ((g 5) (a 5)) 13 1))
                       (append ((a 4) (a 5) (a 2))
                               7
                               1
                               (append ((a 4) (a 5) (a 2)) 14 1)))
              (collect ((g 3) (a 3))
                       3
                       0
                       (collect ((g 3) (a 3))
                                8
                                0
                                (collect ((g 3) (a 3)) 15 0)))
              (append ((a 2) (a 3) (a 1))
                      4
                      0
                      (append ((a 2) (a 3) (a 1))
                              9
                              0
                              (append ((a 2) (a 3) (a 1)) 16 0)))))
    "generational increase begins with first literal occurrence of dp")))