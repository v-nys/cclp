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

(require racket-tree-utils/src/tree)

(require (prefix-in ad: "../src/abstract-multi-domain.rkt"))
(require "../src/abstract-analysis.rkt")
(require "../src/cclp-interpreter.rkt")
(require "../src/generational-tree.rkt")
(require "../src/similarity.rkt")

(require (only-in "generational-tree-test.rkt" generational-tree-bp))

(define (horizontal-level-bp . strings-ids-and-levels)
  (match strings-ids-and-levels
    [(list) (list)]
    [(list-rest h1 h2 h3 t)
     (cons
      (identified-atom-with-generation (identified-atom (interpret-abstract-atom h1) h2) h3)
      (apply horizontal-level-bp t))]))

(test-case
 "finding level-0 instances of a target, and their complement, at a particular level"
 (check-equal?
  (dp-zero-subtree-depth-complement-at-level
   (identified-atom (ad:abstract-atom 'dp '()) 2)
   (generational-tree-bp
    (a 1 0
       (dp 2 0
           (f 4 1)
           (g 5 1))
       (b 3 0
          (c 6 0)
          (dp 7 0
              (f 8 1)
              (g 9 1)))))
   1)
  (list
   (generational-tree-bp
    (dp 2 0
        (f 4 1)
        (g 5 1)))
   1
   (list
    (generational-tree-bp
     (b 3 0
        (c 6 0)
        (dp 7 0
            (f 8 1)
            (g 9 1))))))))

(test-case
 "finding subtrees which begin with a particular atom, as well as their depth and complement"
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (identified-atom (ad:abstract-atom 'dp '()) 2)
   (generational-tree-bp
    (a 1 0
       (dp 2 0
           (f 4 1)
           (g 5 1))
       (b 3 0
          (c 6 0)
          (dp 7 0
              (f 8 1)
              (g 9 1))))))
  (list
   (generational-tree-bp
    (dp 2 0
        (f 4 1)
        (g 5 1)))
   1
   (list
    (generational-tree-bp
     (b 3 0
        (c 6 0)
        (dp 7 0
            (f 8 1)
            (g 9 1)))))))
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (identified-atom (ad:abstract-atom 'dp '()) 1)
   (generational-tree-bp
    (dp 1 0
        (a 2 1)
        (b 3 1))))
  (list
   (generational-tree-bp
    (dp 1 0
        (a 2 1)
        (b 3 1)))
   0
   (list)))
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (identified-atom (ad:abstract-atom 'dp '()) 1)
   (generational-tree-bp
    (a 2 0)))
  #f)
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (identified-atom (ad:abstract-atom 'dp '()) 1)
   (generational-tree-bp
    (a 2 0
       (b 3 0)
       (c 4 0))))
  #f))

(let* ([bottom-left (node (cycle 1) '())]
       [bottom-right (node (tree-label (list) #f (list) #f #f) '())]
       [near-bottom-right-contents
        (widening (interpret-abstract-conjunction "b(α1)") #f "test" 3)]
       [near-bottom-right (node near-bottom-right-contents (list bottom-right))]
       [near-top-right-contents
        (tree-label (interpret-abstract-conjunction "b(γ1)") 0 (list) #f 2)]
       [near-top-right (node near-top-right-contents (list near-bottom-right))]
       [top-contents
        (widening (interpret-abstract-conjunction "a(α1)") #f "test" 1)]
       [top (node top-contents (list bottom-left near-top-right))])
  (check-equal?
   (shortest-branch-containing 1 3 top)
   (list top-contents near-top-right-contents near-bottom-right-contents)))

(test-case
 "extracting the invertible function f from two related conjunctions"
 (check-equal?
  (extract-f-mapping
   3
   (cons
    (generational-tree-bp
     (collect
      ((g 1) (a 1))
      1
      0
      (collect
       ((g 2) (a 2))
       2
       1
       (collect
        ((g 4) (a 4))
        5
        2
        (collect
         ((g 6) (a 6))
         10
         3)
        (collect
         ((g 7) (a 7))
         11
         3)
        (append
         ((a 6) (a 7) (a 4))
         12
         3))
       (collect
        ((g 5) (a 5))
        6
        2
        (collect
         ((g 5) (a 5))
         13
         2))
       (append
        ((a 4) (a 5) (a 2))
        7
        2
        (append
         ((a 4) (a 5) (a 2))
         14
         2)))
      (collect
       ((g 3) (a 3))
       3
       1
       (collect
        ((g 3) (a 3))
        8
        1
        (collect
         ((g 3) (a 3))
         15
         1)))
      (append
       ((a 2) (a 3) (a 1))
       4
       1
       (append
        ((a 2) (a 3) (a 1))
        9
        1
        (append
         ((a 2) (a 3) (a 1))
         16
         1)))))
    0))
  (list 'fresh 'fresh (identity-constraint 5) 'fresh 'fresh)))

;(test-case
; "checking whether f expresses exactly the desired mapping, across all related conjunctions"
; (check-equal?
;  (invertible-function-f-applies
;   3
;   4
;   3
;   4
;   (cons
;    (generational-tree-bp
;     (collect
;      ((g 1) (a 1))
;      0
;      (collect
;       ((g 2) (a 2))
;       1
;       (collect
;        ((g 3) (a 3))
;        2
;        (collect
;         ((g 4) (a 4))
;         3
;         (collect ((g 5) (a 5)) 4)
;         (collect ((g 6) (a 6)) 4)
;         (append ((a 5) (a 6) (a 4)) 4))
;        (collect
;         ((g 7) (a 7))
;         3
;         (collect ((g 7) (a 7)) 3))
;        (append
;         ((a 4) (a 7) (a 3))
;         3
;         (append ((a 4) (a 7) (a 3)) 3)))
;       (collect
;        ((g 8) (a 8))
;        2
;        (collect
;         ((g 8) (a 8))
;         2
;         (collect ((g 8) (a 8)) 2)))
;       (append
;        ((a 3) (a 8) (a 2))
;        2
;        (append
;         ((a 3) (a 8) (a 2))
;         2
;         (append ((a 3) (a 8) (a 2)) 2))))
;      (collect
;       ((g 9) (a 9))
;       1
;       (collect
;        ((g 9) (a 9))
;        1
;        (collect
;         ((g 9) (a 9))
;         1
;         (collect ((g 9) (a 9)) 1))))
;      (append
;       ((a 2) (a 9) (a 1))
;       1
;       (append
;        ((a 2) (a 9) (a 1))
;        1
;        (append
;         ((a 2) (a 9) (a 1))
;         1
;         (append ((a 2) (a 9) (a 1)) 1))))))
;    0))
;  #t)
; (check-equal?
;  (invertible-function-f-applies
;   3
;   4
;   3
;   4
;   (cons
;    (generational-tree-bp
;     (collect
;      ((g 1) (a 1))
;      0
;      (collect
;       ((g 2) (a 2))
;       1
;       (collect
;        ((g 3) (a 3))
;        2
;        (collect
;         ((g 4) (a 4))
;         3
;         (collect ((g 5) (a 5)) 4)
;         (collect ((g 6) (a 6)) 4)
;         (append ((a 5) (a 6) (a 4)) 4))
;        (collect
;         ((g 7) (a 7))
;         3
;         (collect ((g 7) (a 7)) 3))
;        (append
;         ((a 4) (a 7) (a 3))
;         3
;         (append ((a 4) (a 7) (a 3)) 3)))
;       (collect
;        ((g 8) (a 8))
;        2
;        (collect
;         ((g 8) (a 8))
;         2
;         (collect ((g 8) (a 8)) 2)))
;       (append
;        ; not properly aliased with gen 3
;        ((a 1000) (a 8) (a 2))
;        2
;        (append
;         ((a 3) (a 8) (a 2))
;         2
;         (append ((a 3) (a 8) (a 2)) 2))))
;      (collect
;       ((g 9) (a 9))
;       1
;       (collect
;        ((g 9) (a 9))
;        1
;        (collect
;         ((g 9) (a 9))
;         1
;         (collect ((g 9) (a 9)) 1))))
;      (append
;       ((a 2) (a 9) (a 1))
;       1
;       (append
;        ((a 2) (a 9) (a 1))
;        1
;        (append
;         ((a 2) (a 9) (a 1))
;         1
;         (append ((a 2) (a 9) (a 1)) 1))))))
;    0))
;  #t))

(test-case
 "checking for correspondence between generations around and including selected one"
 (check-equal?
  (three-generation-correspondence
   2
   3
   (horizontal-level-bp
    "integers(γ1,α1)" 1 0
    "filter(γ2,α1,α2)" 2 1
    "filter(γ3,[γ4|α2],α3)" 3 2
    "filter(γ5,α3,α4)" 4 3
    "sift(α4,α5)" 5 3
    "length(α5,γ6)" 6 0)
   (horizontal-level-bp
    "integers(γ1,α1)" 7 0
    "filter(γ2,α1,α2)" 8 1
    "filter(γ3,α2,α3)" 9 2
    "filter(γ4,[γ5|α3],α4)" 10 3
    "filter(γ6,α4,α5)" 11 4
    "sift(α5,α6)" 12 4
    "length(α6,γ7)" 13 0))
  #t)
 (check-equal?
  (three-generation-correspondence
   1
   1
   (horizontal-level-bp
    "integers(γ1,α1)" 1 0
    "filter(γ2,[γ3|α1],α2)" 2 1
    "filter(γ4,α2,α3)" 3 2
    "filter(γ5,α3,α4)" 4 3
    "sift(α4,α5)" 5 3
    "length(α5,γ6)" 6 0)
   (horizontal-level-bp
    "integers(γ1,α1)" 7 0
    "filter(γ2,[γ3|α1],α2)" 8 1
    "filter(γ4,α2,α3)" 9 2
    "filter(γ5,α3,α4)" 10 3
    "filter(γ6,α4,α5)" 11 4
    "sift(α5,α6)" 12 4
    "length(α6,γ7)" 13 0))
  #t)
 (check-equal?
  (three-generation-correspondence
   1
   1
   (horizontal-level-bp
    "integers(γ1,α1)" 1 0
    "filter(γ2,[γ3|α1],α2)" 2 1
    "filter(γ4,α2,α3)" 3 2
    "filter(γ5,α3,α4)" 4 3
    "sift(α4,α5)" 5 3
    "length(α5,γ6)" 6 0)
   (horizontal-level-bp
    "integers(γ1,α1)" 7 0
    "filter(γ2,[γ3|α1],α2)" 8 1
    "filter(γ4,α2,α3)" 9 2
    "filter(γ5,α3,α4)" 10 3
    "filter(γ6,α4,α5)" 11 4
    "sift(α5,α6)" 12 4
    "length(γ6,γ7)" 13 0)) ; difference in preceding generation
  #f)
 (check-equal?
  (three-generation-correspondence
   1
   1
   (horizontal-level-bp
    "integers(γ1,α1)" 1 0
    "filter(γ2,[γ3|α1],α2)" 2 1
    "filter(γ4,α2,α3)" 3 2
    "filter(γ5,α3,α4)" 4 3
    "sift(α4,α5)" 5 3
    "length(α5,γ6)" 6 0)
   (horizontal-level-bp
    "integers(γ1,α1)" 7 0
    "filter(γ2,[γ3|α1],α2)" 8 1
    "filter(γ4,α2,γ3)" 9 2 ; difference in subsequent generation
    "filter(γ5,α3,α4)" 10 3
    "filter(γ6,α4,α5)" 11 4
    "sift(α5,α6)" 12 4
    "length(α6,γ7)" 13 0))
  #f))