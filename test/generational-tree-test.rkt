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

#lang racket
(require rackunit)
(require "../src/abstract-renaming.rkt")
(require "../src/domain-switching.rkt")
(require (prefix-in cbp: "concrete-domain-boilerplate.rkt"))
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require (prefix-in ak: "../src/abstract-knowledge.rkt"))
(require "../src/generational-tree.rkt")
(require racket-tree-utils/src/tree)
(require racket-tree-utils/src/printer)
(require "../src/data-utils.rkt")
(require "printed-test-results.rkt")
(require "../src/abstract-knowledge.rkt")
(require (prefix-in ad: "../src/abstract-multi-domain.rkt"))
(require "../src/abstract-analysis.rkt")
(require "../src/cclp-interpreter.rkt")
(require (only-in "../src/interaction.rkt" print-atom-with-generation-node))

(define-syntax (skeleton-bp stx)
  (syntax-case stx ()
    [(_ (SYM (ARG ...) TREE ...))
     #'(node
        (ad:abstract-atom 'SYM (list (generational-atom-arg-bp ARG) ...))
        (list (skeleton-bp TREE) ...))]))

(define-syntax (generational-tree-bp stx)
  (syntax-case stx ()
    [(_ (SYM (ARG ...) GEN TREE ...))
     #'(node
        (atom-with-generation (ad:abstract-atom 'SYM (list (generational-atom-arg-bp ARG) ...)) GEN)
        (list (generational-tree-bp TREE) ...))]
    [(_ (SYM GEN TREE ...))
     #'(node
        (atom-with-generation (ad:abstract-atom 'SYM '()) GEN)
        (list (generational-tree-bp TREE) ...))]))
(provide generational-tree-bp)

(define-syntax (generational-atom-arg-bp stx)
  (syntax-case stx (a g)
    [(_ (a IDX))
     #'(ad:a IDX)]
    [(_ (g IDX))
     #'(ad:g IDX)]
    [(_ SYM)
     #'(ad:abstract-function 'SYM '())]
    [(_ (SYM ARG ...))
     #'(ad:abstract-function 'SYM (list (generational-atom-arg-bp ARG) ...))]))

(test-case
 "Extracting the active branch from a tree."
 (let* ([completed-leaf (node (tree-label '() (none) '() (cbp:parse-rule "foo") #f) '())]
        [parent
         (node
          (tree-label (interpret-abstract-conjunction "foo") (some 0) '() #f 1)
          (list completed-leaf completed-leaf))])
   (check-equal? (active-branch-info parent) #f "all branches have been completed"))
 (let* ([completed-leaf (node (tree-label '() (none) '() (cbp:parse-rule "bar") #f) '())]
        [left-child
         (node
          (tree-label
           (interpret-abstract-conjunction "bar") (some 0) '() (cbp:parse-rule "foo :- bar") 2)
          (list completed-leaf))]
        [active-leaf
         (node
          (tree-label (interpret-abstract-conjunction "baz") (none) '() (cbp:parse-rule "quux :- baz") #f)
          '())]
        [right-child
         (node
          (tree-label
           (interpret-abstract-conjunction "quux") (some 0) '() (cbp:parse-rule "foo :- quux") 3)
          (list active-leaf))]
        [root
         (node
          (tree-label
           (interpret-abstract-conjunction "foo") (some 0) '() #f 1)
          (list left-child right-child))]
        [info1
         (tree-label (interpret-abstract-conjunction "foo") (some 0) (list) #f 1)]
        [info2
         (tree-label (interpret-abstract-conjunction "quux") (some 0) (list) (cbp:parse-rule "foo :- quux") 3)]
        [info3
         (tree-label (interpret-abstract-conjunction "baz") (none) (list) (cbp:parse-rule "quux :- baz") #f)])
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
   (values (cbp:parse-rule "a :- b, c")
           (cbp:parse-rule "b :- d, e")
           (cbp:parse-rule "c :- f")))
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
        [level-2-skeleton-1 (node atom-d (list (node atom-d '())))]
        [level-2-skeleton-2 (node atom-e (list (node atom-e '())))]
        [level-2-skeleton-3 (node atom-c (list (node atom-f '())))]
        [level-1-skeleton-1 (node atom-b (list level-2-skeleton-1 level-2-skeleton-2))]
        [level-1-skeleton-2 (node atom-c (list level-2-skeleton-3))]
        [level-0-skeleton (node atom-a (list level-1-skeleton-1 level-1-skeleton-2))]
        [outcome (generational-tree-skeleton branch)])
   (check-equal? outcome (list level-0-skeleton))))

(let* ([level-3
        (node
         (tree-label
          (interpret-abstract-conjunction "b,b,a") (none) (list) (cbp:parse-rule "a :- b,a") #f)
         '())]
       [level-2
        (node
         (tree-label
          (interpret-abstract-conjunction "b,a") (some 1) (list) (cbp:parse-rule "a :- b,a") 2)
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
      (a ()
         (b ()
            (b ()))
         (a ()
            (b ())
            (a ())))))
    "check if the skeleton is correct")
   (check-equal?
    (generational-trees (active-branch-info level-1))
    ; a occurs three times in analysis, but four times in the skeleton
    (build-list
     4
     (λ (_)
       (generational-tree-bp
        (a 0
           (b 1
              (b 1))
           (a 1
              (b 2)
              (a 2))))))
    "check if analysis without a target atom is correct")))

(check-equal?
 (descendant-renames?
  (skeleton-bp
   (a ()
      (b ())
      (a ())))
  (ad:abstract-atom 'a '()))
 #t)

(check-equal?
 (descendant-renames?
  (skeleton-bp
   (b ()
      (b ())
      (a ())))
  (ad:abstract-atom 'a '()))
 #t)

(check-equal?
 (descendant-renames?
  (skeleton-bp
   (b ()
      (a ())
      (a ())))
  (ad:abstract-atom 'b '()))
 #f)

(check-equal?
 (descendant-renames?
  (skeleton-bp
   (a ()
      (b ())
      (c ()
         (a ()))))
  (ad:abstract-atom 'a '()))
 #t)

(test-case
 "annotating skeletons"
 (check-equal?
  (annotate-generational-tree
   (skeleton-bp
    (collect ((g 1) (a 1))
             (collect ((g 2) (a 2))
                      (collect ((g 4) (a 4))
                               (collect ((g 6) (a 6)))
                               (collect ((g 7) (a 7)))
                               (append ((a 6) (a 7) (a 4))))
                      (collect ((g 5) (a 5))
                               (collect ((g 5) (a 5))))
                      (append ((a 4) (a 5) (a 2))
                              (append ((a 4) (a 5) (a 2)))))
             (collect ((g 3) (a 3))
                      (collect ((g 3) (a 3))
                               (collect ((g 3) (a 3)))))
             (append ((a 2) (a 3) (a 1))
                     (append ((a 2) (a 3) (a 1))
                             (append ((a 2) (a 3) (a 1)))))))
   (ad:abstract-atom 'collect (list (ad:g 2) (ad:a 2)))
   0
   3
   0)
  (generational-tree-bp
   (collect ((g 1) (a 1))
            0
            (collect ((g 2) (a 2))
                     0
                     (collect ((g 4) (a 4))
                              1
                              (collect ((g 6) (a 6)) 2)
                              (collect ((g 7) (a 7)) 2)
                              (append ((a 6) (a 7) (a 4)) 2))
                     (collect ((g 5) (a 5))
                              1
                              (collect ((g 5) (a 5)) 1))
                     (append ((a 4) (a 5) (a 2))
                             1
                             (append ((a 4) (a 5) (a 2)) 1)))
            (collect ((g 3) (a 3))
                     0
                     (collect ((g 3) (a 3))
                              0
                              (collect ((g 3) (a 3)) 0)))
            (append ((a 2) (a 3) (a 1))
                    0
                    (append ((a 2) (a 3) (a 1))
                            0
                            (append ((a 2) (a 3) (a 1)) 0)))))
  "generational increase begins with first *literal* occurrence of dp"))