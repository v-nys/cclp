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

; TODO test annotating for a specific target atom
; TODO test annotating without a specific target atom

; TODO reactivate when the rest has been tested
;(test-case
; "the generational tree is computed correctly based on a branch of several nodes"
; (define-values (atom0 atom1a atom1b atom1c atom2a atom2b atom2c atom2d atom3a atom3b atom3c atom4a atom4b atom4c atom4d)
;   (values (abp:parse-abstract-atom "primes(γ1,α1)")
;           
;           (abp:parse-abstract-atom "integers(γ2,α2)")
;           (abp:parse-abstract-atom "sift(α2,α1)")
;           (abp:parse-abstract-atom "length(α1,γ1)")
;           
;           (abp:parse-abstract-atom "plus(γ2,γ3,α3)")
;           (abp:parse-abstract-atom "integers(α3,α4)")
;           (abp:parse-abstract-atom "sift([γ2|α4],α1)")
;           (abp:parse-abstract-atom "length(α1,γ1)")
;           
;           (abp:parse-abstract-atom "integers(γ4,α4)")
;           (abp:parse-abstract-atom "sift([γ2|α4],α1)")
;           (abp:parse-abstract-atom "length(α1,γ1)")
;           
;           (abp:parse-abstract-atom "integers(γ4,α4)")
;           (abp:parse-abstract-atom "filter(γ2,α4,α5)")
;           (abp:parse-abstract-atom "sift(α5,α6)")
;           (abp:parse-abstract-atom "length([γ2|α6],γ1)")))
; (define-values (clause1 clause2 full-ai1 clause3)
;   (values (pre-abstract-rule (cbp:parse-rule "primes(X,Y) :- integers(2,Z), sift(Z,Y), length(Y,X)"))
;           (pre-abstract-rule (cbp:parse-rule "integers(N,[N|I]) :- plus(N,1,M), integers(M,I)"))
;           (full-evaluation (abp:parse-abstract-atom "plus(γ1,γ2,α2)") (abp:parse-abstract-atom "plus(γ1,γ2,γ3)"))
;           (pre-abstract-rule (cbp:parse-rule "sift([N|Ints],[N|Primes]) :- filter(N,Ints,F), sift(F,Primes)"))))
; (define-values (branch-node1 branch-node2 branch-node3 branch-node4 branch-node5)
;   (values (resolution-info (list atom0) (some (cons 0 clause1)))
;           (resolution-info (list atom1a atom1b atom1c) (some (cons 0 clause2)))
;           (resolution-info (list atom2a atom2b atom2c atom2d) (some (cons 0 full-ai1)))
;           (resolution-info (list atom3a atom3b atom3c) (some (cons 1 clause3)))
;           (resolution-info (list atom4a atom4b atom4c atom4d) (none))))
; (let* ([target-atom (abp:parse-abstract-atom "sift([γ1|α1],α2)")]
;        [branch (list branch-node1 branch-node2 branch-node3 branch-node4 branch-node5)]
;        [expected4a (node (atom-with-generation atom4a 0) (list))]
;        [expected4b (node (atom-with-generation atom4b 1) (list))]
;        [expected4c (node (atom-with-generation atom4c 1) (list))]
;        [expected4d (node (atom-with-generation atom4d 0) (list))]
;        [expected3a (node (atom-with-generation atom3a 0) (list expected4a))]
;        [expected3b (node (atom-with-generation atom3b 0) (list expected4b expected4c))]
;        [expected3c (node (atom-with-generation atom3c 0) (list expected4d))]
;        [expected2a (node (atom-with-generation atom2a 0) (list))]
;        [expected2b (node (atom-with-generation atom2b 0) (list expected3a))]
;        [expected2c (node (atom-with-generation atom2c 0) (list expected3b))]
;        [expected2d (node (atom-with-generation atom2d 0) (list expected3c))]
;        [expected1a (node (atom-with-generation atom1a 0) (list expected2a expected2b))]
;        [expected1b (node (atom-with-generation atom1b 0) (list expected2c))]
;        [expected1c (node (atom-with-generation atom1c 0) (list expected2d))]
;        [expected0 (node (atom-with-generation atom0 0) (list expected1a expected1b expected1c))]
;        [actual (car (generational-trees branch))])
;   (when (not (equal? actual expected0))
;     (begin (displayln "actual:")
;            (tree-display actual node-display)
;            (displayln "expected:")
;            (tree-display expected0 node-display)
;            (check-equal? actual expected0)))))


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