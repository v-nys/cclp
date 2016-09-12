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
(require (prefix-in ak: "../src/typed-abstract-knowledge.rkt"))
(require "../src/generational-tree.rkt")
(require typed-racket-tree-utils/tree)
(require "../src/data-utils.rkt")

; (struct resolution-info ([conjunction : AbstractConjunction] [selection : Integer] [clause : ak:AbstractKnowledge]))
; (struct atom-with-generation ([atom : AbstractConjunct] [generation : Integer]))

(test-case "the generational tree is computed correctly based on a branch of several nodes without recursion"
  (define-values (atom0 atom1a atom1b atom1c atom2a atom2b atom2c atom2d)
    (values (abp:parse-atom "primes(γ1,α1)")
            (abp:parse-atom "integers(γ2,α2)")
            (abp:parse-atom "sift(α2,α1)")
            (abp:parse-atom "length(α1,γ1)")
            (abp:parse-atom "plus(γ2,γ3,α3)")
            (abp:parse-atom "integers(α3,α4)")
            (abp:parse-atom "sift([γ2|α4],α1)")
            (abp:parse-atom "length(α1,γ1)")))
  (define-values (clause1 clause2)
    (values (pre-abstract-rule (cbp:parse-rule "primes(X,Y) :- integers(2,Z), sift(Z,Y), length(Y,X)"))
            (pre-abstract-rule (cbp:parse-rule "integers(N,[N|I]) :- plus(N,1,M), integers(M,I)"))))
  (define-values (branch-node1 branch-node2 branch-node3)
    (values (resolution-info (list atom0) (some (cons 0 clause1)))
            (resolution-info (list atom1a atom1b atom1c) (some (cons 0 clause2)))
            (resolution-info (list atom2a atom2b atom2c atom2d) (none))))
  (let* ([target-atom (abp:parse-atom "sift([γ1|α1],α2)")]
         [branch (list branch-node1 branch-node2 branch-node3)]
         [expected2a (node (atom-with-generation atom2a 0) (list))]
         [expected2b (node (atom-with-generation atom2b 0) (list))]
         [expected2c (node (atom-with-generation atom2c 0) (list))]
         [expected2d (node (atom-with-generation atom2d 0) (list))]
         [expected1a (node (atom-with-generation atom1a 0) (list expected2a expected2b))]
         [expected1b (node (atom-with-generation atom1b 0) (list expected2c))]
         [expected1c (node (atom-with-generation atom1c 0) (list expected2d))]
         [expected0 (node (atom-with-generation atom0 0) (list expected1a expected1b expected1c))])
           (check-equal? (generational-tree branch) (list expected0))))