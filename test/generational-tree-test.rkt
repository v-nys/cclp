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
(require "../src/abstract-multi-domain.rkt")
(require "../src/abstract-analysis.rkt")

(define (node-display tree out)
  (print (node-label tree) out))

(test-case
 "Extracting the active branch from a tree."
 (let* ([completed-leaf (node (tree-label '() (none) '() (cbp:parse-rule "foo") #f) '())]
        [parent
         (node
          (tree-label (abp:parse-abstract-conjunction "foo") (some 0) '() #f 1)
          (list completed-leaf completed-leaf))])
   (check-equal? (active-branch-info parent) #f "all branches have been completed"))
 (let* ([completed-leaf (node (tree-label '() (none) '() (cbp:parse-rule "bar") #f) '())]
        [left-child
         (node
          (tree-label
           (abp:parse-abstract-conjunction "bar") (some 0) '() (cbp:parse-rule "foo :- bar") 2)
          (list completed-leaf))]
        [active-leaf
         (node
          (tree-label (abp:parse-abstract-conjunction "baz") (none) '() (cbp:parse-rule "quux :- baz") #f)
          '())]
        [right-child
         (node
          (tree-label
           (abp:parse-abstract-conjunction "quux") (some 0) '() (cbp:parse-rule "foo :- quux") 3)
          (list active-leaf))]
        [root
         (node
          (tree-label
           (abp:parse-abstract-conjunction "foo") (some 0) '() #f 1)
          (list left-child right-child))]
        [info1
         (tree-label (abp:parse-abstract-conjunction "foo") (some 0) (list) #f 1)]
        [info2
         (tree-label (abp:parse-abstract-conjunction "quux") (some 0) (list) (cbp:parse-rule "foo :- quux") 3)]
        [info3
         (tree-label (abp:parse-abstract-conjunction "baz") (none) (list) (cbp:parse-rule "quux :- baz") #f)])
   (check-equal? (active-branch-info root) (list info1 info2 info3))))

(test-case
 "the skeleton is computed correctly based on a branch of several nodes"
 (define-values (atom-a atom-b atom-c atom-d atom-e atom-f)
   (values (abp:parse-abstract-atom "a")
           (abp:parse-abstract-atom "b")
           (abp:parse-abstract-atom "c")
           (abp:parse-abstract-atom "d")
           (abp:parse-abstract-atom "e")
           (abp:parse-abstract-atom "f")))
 (define-values (a-clause b-clause c-clause)
   (values (cbp:parse-rule "a :- b, c")
           (cbp:parse-rule "b :- d, e")
           (cbp:parse-rule "c :- f")))
 (let* ([level-3-analysis
         (node
          (tree-label (abp:parse-abstract-conjunction "d,e,f") (none) (list) c-clause #f) '())]
        [level-2-analysis
         (node
          (tree-label (abp:parse-abstract-conjunction "d,e,c") (some 2) (list) b-clause 3)
          (list level-3-analysis))]
        [level-1-analysis
         (node
          (tree-label (abp:parse-abstract-conjunction "b,c") (some 0) (list) a-clause 2)
          (list level-2-analysis))]
        [level-0-analysis
         (node
          (tree-label (abp:parse-abstract-conjunction "a") (some 0) (list) #f 1)
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
          (abp:parse-abstract-conjunction "b,b,a") (none) (list) (cbp:parse-rule "a :- b,a") #f)
         '())]
       [level-2
        (node
         (tree-label
          (abp:parse-abstract-conjunction "b,a") (some 1) (list) (cbp:parse-rule "a :- b,a") 2)
         (list level-3))]
       [level-1
        (node
         (tree-label (abp:parse-abstract-conjunction "a") (some 0) (list) #f 1)
         (list level-2))]
       [skeleton-lv-3-1 (node (abstract-atom 'b '()) '())]
       [skeleton-lv-3-2 (node (abstract-atom 'a '()) '())]
       [skeleton-lv-2-1 (node (abstract-atom 'b '()) (list skeleton-lv-3-1))]
       [skeleton-lv-2-2 (node (abstract-atom 'a '()) (list skeleton-lv-3-1 skeleton-lv-3-2))]
       [skeleton-lv-1 (node (abstract-atom 'a '()) (list skeleton-lv-2-1 skeleton-lv-2-2))])
  (test-case
   "generational tree for analysis tree a - b,a - b,b,a"
   (check-equal?
    (generational-tree-skeleton (active-branch-info level-1))
    (list skeleton-lv-1)
    "check if the skeleton is correct")
   (let* ([annotated-lv-3-1 (node (atom-with-generation (abstract-atom 'b '()) 1) '())]
          [annotated-lv-3-2 (node (atom-with-generation (abstract-atom 'b '()) 2) '())]
          [annotated-lv-3-3 (node (atom-with-generation (abstract-atom 'a '()) 2) '())]
          [annotated-lv-2-1 (node (atom-with-generation (abstract-atom 'b '()) 1) (list annotated-lv-3-1))]
          [annotated-lv-2-2 (node (atom-with-generation (abstract-atom 'a '()) 1) (list annotated-lv-3-2 annotated-lv-3-3))]
          [annotated-lv-1 (node (atom-with-generation (abstract-atom 'a '()) 0) (list annotated-lv-2-1 annotated-lv-2-2))])
     (begin
       (check-equal?
        (annotate-generational-tree skeleton-lv-1 (abp:parse-abstract-atom "a") 0 2 0)
        annotated-lv-1
        "check if annotation for target atom is correct")
       (check-equal?
        (generational-trees (active-branch-info level-1))
        (list annotated-lv-1)
        "check if analysis without a target atom is correct")))))

(test-case
 "horizontal reading of a generational tree"
 (let ([a-0 (atom-with-generation (abp:parse-abstract-atom "a") 0)]
       [a-1 (atom-with-generation (abp:parse-abstract-atom "a") 1)]
       [a-2 (atom-with-generation (abp:parse-abstract-atom "a") 2)]
       [b-1 (atom-with-generation (abp:parse-abstract-atom "b") 1)]
       [b-2 (atom-with-generation (abp:parse-abstract-atom "b") 2)])
   ; TODO test with 2 and 3 levels
   (check-equal?
    (horizontal-level (node a-0 '()) 0)
    (list a-0))))

(let* ([a-atom (abstract-atom 'a '())]
       [b-atom (abstract-atom 'b '())]
       [nc1 (node b-atom '())]
       [nc2 (node a-atom '())]
       [n (node a-atom (list nc1 nc2))])
  (check-equal? (descendant-renames? n a-atom) #t))

(let* ([a-atom (abstract-atom 'a '())]
       [b-atom (abstract-atom 'b '())]
       [nc1 (node b-atom '())]
       [nc2 (node a-atom '())]
       [n (node b-atom (list nc1 nc2))])
  (check-equal? (descendant-renames? n a-atom) #t))

(let* ([a-atom (abstract-atom 'a '())]
       [b-atom (abstract-atom 'b '())]
       [nc1 (node a-atom '())]
       [nc2 (node a-atom '())]
       [n (node b-atom (list nc1 nc2))])
  (check-equal? (descendant-renames? n b-atom) #f))

(let* ([a-atom (abstract-atom 'a '())]
       [b-atom (abstract-atom 'b '())]
       [c-atom (abstract-atom 'c '())]
       [nc1 (node b-atom '())]
       [ncc (node a-atom '())]
       [nc2 (node c-atom (list ncc))]
       [n (node a-atom (list nc1 nc2))])
  (check-equal? (descendant-renames? n a-atom) #t))