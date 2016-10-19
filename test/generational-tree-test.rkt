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
(require "../src/interaction.rkt")

(define (node-display tree out)
  (print (node-label tree) out))

(test-case
 "Converting from a tree to a list representing resolution info for the active branch."
 (let* ([completed-leaf (node (tree-label '() (none) '() (pre-abstract-rule (cbp:parse-rule "foo")) #f) '())]
        [parent
         (node
          (tree-label (abp:parse-abstract-conjunction "foo") (some 0) '() #f 1)
          (list completed-leaf completed-leaf))])
   (check-equal? (active-branch-info parent) #f "all branches have been completed"))
 (let* ([completed-leaf (node (tree-label '() (none) '() (pre-abstract-rule (cbp:parse-rule "bar")) #f) '())]
        [left-child
         (node
          (tree-label
           (abp:parse-abstract-conjunction "bar") (some 0) '() (pre-abstract-rule (cbp:parse-rule "foo :- bar")) 2)
          (list completed-leaf))]
        [active-leaf
         (node
          (tree-label (abp:parse-abstract-conjunction "baz") (none) '() (pre-abstract-rule (cbp:parse-rule "quux :- baz")) #f)
          '())]
        [right-child
         (node
          (tree-label
           (abp:parse-abstract-conjunction "quux") (some 0) '() (pre-abstract-rule (cbp:parse-rule "foo :- quux")) 3)
          (list active-leaf))]
        [root
         (node
          (tree-label
           (abp:parse-abstract-conjunction "foo") (some 0) '() #f 1)
          (list left-child right-child))]
        [info1 (resolution-info (abp:parse-abstract-conjunction "foo") (some (cons 0 (cbp:parse-rule "foo :- quux"))))]
        [info2 (resolution-info (abp:parse-abstract-conjunction "quux") (some (cons 0 (cbp:parse-rule "quux :- baz"))))]
        [info3 (resolution-info (abp:parse-abstract-conjunction "baz") (none))])
   (check-equal? (active-branch-info root) (list info1 info2 info3))))

(test-case
 "the skeleton is computed correctly based on a branch of several nodes"
 (define-values (atom0
                 atom1a atom1b atom1c
                 atom2a atom2b atom2c atom2d
                 atom3a atom3b atom3c
                 atom4a atom4b atom4c atom4d
                 atom5a atom5b atom5c atom5d atom5e
                 atom6a atom6b atom6c atom6d
                 atom7a atom7b atom7c atom7d atom7e
                 atom8a atom8b atom8c atom8d
                 atom9a atom9b atom9c atom9d)
   (values (abp:parse-abstract-atom "primes(γ1,α1)") ; 0
           
           (abp:parse-abstract-atom "integers(γ2,α2)") ; 1
           (abp:parse-abstract-atom "sift(α2,α1)")
           (abp:parse-abstract-atom "length(α1,γ1)")
           
           (abp:parse-abstract-atom "plus(γ2,γ3,α3)") ; 2
           (abp:parse-abstract-atom "integers(α3,α4)")
           (abp:parse-abstract-atom "sift([γ2|α4],α1)")
           (abp:parse-abstract-atom "length(α1,γ1)")
           
           (abp:parse-abstract-atom "integers(γ4,α4)") ; 3
           (abp:parse-abstract-atom "sift([γ2|α4],α1)")
           (abp:parse-abstract-atom "length(α1,γ1)")
           
           (abp:parse-abstract-atom "integers(γ4,α4)") ; 4
           (abp:parse-abstract-atom "filter(γ2,α4,α5)")
           (abp:parse-abstract-atom "sift(α5,α6)")
           (abp:parse-abstract-atom "length([γ2|α6],γ1)")
           
           (abp:parse-abstract-atom "integers(γ4,α4)") ; 5
           (abp:parse-abstract-atom "filter(γ2,α4,α5)")
           (abp:parse-abstract-atom "sift(α5,α6)")
           (abp:parse-abstract-atom "minus(γ1,γ3,α7)")
           (abp:parse-abstract-atom "length(α6,α7)")
           
           (abp:parse-abstract-atom "integers(γ4,α4)") ; 6
           (abp:parse-abstract-atom "filter(γ2,α4,α5)")
           (abp:parse-abstract-atom "sift(α5,α6)")
           (abp:parse-abstract-atom "length(α6,γ4)")
           
           (abp:parse-abstract-atom "plus(γ4,γ3,α8)") ; 7
           (abp:parse-abstract-atom "integers(α8,α9)")
           (abp:parse-abstract-atom "filter(γ2,[γ4|α9],α5)")
           (abp:parse-abstract-atom "sift(α5,α6)")
           (abp:parse-abstract-atom "length(α6,γ4)")
           
           (abp:parse-abstract-atom "integers(γ5,α9)") ; 8
           (abp:parse-abstract-atom "filter(γ2,[γ4|α9],α5)")
           (abp:parse-abstract-atom "sift(α5,α6)")
           (abp:parse-abstract-atom "length(α6,γ4)")
           
           (abp:parse-abstract-atom "integers(γ5,α9)") ; 9
           (abp:parse-abstract-atom "filter(γ2,α9,α10)")
           (abp:parse-abstract-atom "sift([γ4|α10],α6)")
           (abp:parse-abstract-atom "length(α6,γ4)")))
 (define-values (clause1 clause2 full-ai1 clause3 clause4 full-ai2 clause5)
   (values (pre-abstract-rule (cbp:parse-rule "primes(X,Y) :- integers(2,Z), sift(Z,Y), length(Y,X)"))
           (pre-abstract-rule (cbp:parse-rule "integers(N,[N|I]) :- plus(N,1,M), integers(M,I)"))
           (full-evaluation (abp:parse-abstract-atom "plus(γ1,γ2,α2)") (abp:parse-abstract-atom "plus(γ1,γ2,γ3)"))
           (pre-abstract-rule (cbp:parse-rule "sift([N|Ints],[N|Primes]) :- filter(N,Ints,F), sift(F,Primes)"))
           (pre-abstract-rule (cbp:parse-rule "length([H|T],N) :- minus(N,1,M),length(T,M)"))
           (full-evaluation (abp:parse-abstract-atom "minus(γ1,γ2,α2)") (abp:parse-abstract-atom "minus(γ1,γ2,γ3)"))
           (pre-abstract-rule (cbp:parse-rule "filter(N,[M|I],[M|F]) :- filter(N,I,F)"))))
 (define-values (branch-node1 branch-node2 branch-node3 branch-node4 branch-node5 branch-node6
                              branch-node7 branch-node8 branch-node9 branch-node10)
   (values (resolution-info (list atom0) (some (cons 0 clause1)))
           (resolution-info (list atom1a atom1b atom1c) (some (cons 0 clause2)))
           (resolution-info (list atom2a atom2b atom2c atom2d) (some (cons 0 full-ai1)))
           (resolution-info (list atom3a atom3b atom3c) (some (cons 1 clause3)))
           (resolution-info (list atom4a atom4b atom4c atom4d) (some (cons 3 clause4)))
           (resolution-info (list atom5a atom5b atom5c atom5d atom5e) (some (cons 3 full-ai2)))
           (resolution-info (list atom6a atom6b atom6c atom6d) (some (cons 0 clause2)))
           (resolution-info (list atom7a atom7b atom7c atom7d atom7e) (some (cons 0 full-ai1)))
           (resolution-info (list atom8a atom8b atom8c atom8d) (some (cons 1 clause5)))
           (resolution-info (list atom9a atom9b atom9c atom9d) (none))))
 (let* ([branch (list branch-node1 branch-node2 branch-node3 branch-node4 branch-node5 branch-node6 branch-node7 branch-node8 branch-node9 branch-node10)]
        [expected9a (node atom9a (list))]
        [expected9b (node atom9b (list))]
        [expected9c (node atom9c (list))]
        [expected9d (node atom9d (list))]
        [expected8a (node atom8a (list expected9a))]
        [expected8b (node atom8b (list expected9b))]
        [expected8c (node atom8c (list expected9c))]
        [expected8d (node atom8d (list expected9d))]
        [expected7a (node atom7a (list))]
        [expected7b (node atom7b (list expected8a))]
        [expected7c (node atom7c (list expected8b))]
        [expected7d (node atom7d (list expected8c))]
        [expected7e (node atom7e (list expected8d))]
        [expected6a (node atom6a (list expected7a expected7b))]
        [expected6b (node atom6b (list expected7c))]
        [expected6c (node atom6c (list expected7d))]
        [expected6d (node atom6d (list expected7e))]
        [expected5a (node atom5a (list expected6a))]
        [expected5b (node atom5b (list expected6b))]
        [expected5c (node atom5c (list expected6c))]
        [expected5d (node atom5d (list))]
        [expected5e (node atom5e (list expected6d))]
        [expected4a (node atom4a (list expected5a))]
        [expected4b (node atom4b (list expected5b))]
        [expected4c (node atom4c (list expected5c))]
        [expected4d (node atom4d (list expected5d expected5e))]
        [expected3a (node atom3a (list expected4a))]
        [expected3b (node atom3b (list expected4b expected4c))]
        [expected3c (node atom3c (list expected4d))]
        [expected2a (node atom2a (list))]
        [expected2b (node atom2b (list expected3a))]
        [expected2c (node atom2c (list expected3b))]
        [expected2d (node atom2d (list expected3c))]
        [expected1a (node atom1a (list expected2a expected2b))]
        [expected1b (node atom1b (list expected2c))]
        [expected1c (node atom1c (list expected2d))]
        [expected0 (node atom0 (list expected1a expected1b expected1c))]
        [actual (generational-tree-skeleton branch)])
   (check-equal? actual (list expected0))
   (check-equal? (candidate-target-atoms expected0 9 0)
                 (list atom3b))))

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