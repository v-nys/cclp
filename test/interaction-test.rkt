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
(require "../src/interaction.rkt")
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/data-utils.rkt")
(require racket-tree-utils/src/tree)
(require "../src/abstract-analysis.rkt")
(require "../src/cclp-interpreter.rkt")

(test-case
 "finding the most recently applied operation"
 (let ([root-only-tree
        (node
         (tree-label
          (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
          (none)
          (list)
          #f
          #f) '())])
   (check-equal? (candidate-for-undo root-only-tree) #f))
 (let* ([unwound-leaf
         (node
          (tree-label
           (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),eq(α1,α2)")
           (none)
           (list)
           #f ; doesn't matter for purpose of this test
           #f) '())]
        [unwound-tree
         (node
          (tree-label
           (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
           (some 0)
           (list)
           #f
           1) (list unwound-leaf))]
        [rewound-tree
         (node
          (tree-label
           (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
           (none)
           (list)
           #f
           #f) '())])
   (check-equal? (candidate-for-undo unwound-tree) unwound-tree)))

(test-case
 "rewinding the most recently applied operation"
 (let ([root-only-tree
        (node
         (tree-label
          (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
          (none)
          (list)
          #f
          #f) '())])
   (check-equal? (rewind root-only-tree) #f))
 (let* ([unwound-leaf
         (node
          (tree-label
           (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),eq(α1,α2)")
           (none)
           (list)
           #f ; doesn't matter for purpose of this test
           #f) '())]
        [unwound-tree
         (node
          (tree-label
           (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
           (some 0)
           (list)
           #f
           1) (list unwound-leaf))]
        [rewound-tree
         (node
          (tree-label
           (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
           (none)
           (list)
           #f
           #f) '())])
   (check-equal? (rewind unwound-tree) (cons rewound-tree rewound-tree)))
 (let* ([leaf1 (node (tree-label (list) (none) (list) #f #f) (list))]
        [leaf2 leaf1]
        [grandchild1 (node (tree-label (interpret-abstract-conjunction "quux") 0 (list) #f 3) (list leaf1))]
        [grandchild2 (node (tree-label (interpret-abstract-conjunction "zoom") 0 (list) #f 4) (list leaf2))]
        [child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 grandchild2))]
        [unwound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list child))]
        [rewound-node (node (tree-label (interpret-abstract-conjunction "zoom") (none) (list) #f #f) (list))]
        [rewound-child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 rewound-node))]
        [rewound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list rewound-child))])
   (check-equal? (rewind unwound-tree) (cons rewound-node rewound-tree)))
 (let* ([leaf1 (node (tree-label (list) (none) (list) #f #f) (list))]
        [leaf2 (node (cycle 3) (list))]
        [grandchild1 (node (tree-label (interpret-abstract-conjunction "quux") 0 (list) #f 3) (list leaf1))]
        [grandchild2 (node (tree-label (interpret-abstract-conjunction "zoom") 0 (list) #f 4) (list leaf2))]
        [grandchild3 (node (tree-label (interpret-abstract-conjunction "baz") (none) (list) #f 4) (list))]
        [child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 grandchild2 grandchild3))]
        [unwound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list child))]
        [rewound-node (node (tree-label (interpret-abstract-conjunction "zoom") (none) (list) #f #f) (list))]
        [rewound-child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 rewound-node grandchild3))]
        [rewound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list rewound-child))])
   (check-equal? (rewind unwound-tree) (cons rewound-node rewound-tree)))
 (let* ([leaf (node (widening (interpret-abstract-conjunction "foo(γ1)") #f "some message" #f) '())]
        [root-before (node (tree-label (interpret-abstract-conjunction "foo(nil)") 0 (list) #f 1) (list leaf))]
        [root-after (node (tree-label (interpret-abstract-conjunction "foo(nil)") (none) (list) #f #f) (list))])
   (check-equal? (car (rewind root-before)) root-after)
   (check-equal? (cdr (rewind root-before)) root-after)))