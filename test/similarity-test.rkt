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

(define-syntax (generational-tree-bp stx)
  (syntax-case stx ()
    ; we often don't need args for the tests and they involve more boilerplate, so there are two patterns
    [(_ (SYM (ARG ...) GEN TREE ...))
     #'(node
        (atom-with-generation (ad:abstract-atom 'SYM (list (generational-atom-arg-bp ARG) ...)) GEN)
        (list (generational-tree-bp TREE) ...))]
    [(_ (SYM GEN TREE ...))
     #'(node
        (atom-with-generation (ad:abstract-atom 'SYM '()) GEN)
        (list (generational-tree-bp TREE) ...))]))

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
 "finding level-0 instances of a target, and their complement, at a particular level"
 (check-equal?
  (dp-zero-subtree-depth-complement-at-level
   (ad:abstract-atom 'dp '())
   (generational-tree-bp
    (a 0
       (dp 0
           (f 1)
           (g 1))
       (b 0
          (c 0)
          (dp 0
              (f 1)
              (g 1)))))
   1)
  (list
   (generational-tree-bp
    (dp 0
        (f 1)
        (g 1)))
   1
   (list
    (generational-tree-bp
     (b 0
        (c 0)
        (dp 0
            (f 1)
            (g 1))))))))

(test-case
 "finding subtrees which begin with a particular atom, as well as their depth and complement"
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (ad:abstract-atom 'dp '())
   (generational-tree-bp
    (a 0
       (dp 0
           (f 1)
           (g 1))
       (b 0
          (c 0)
          ; note: this occurrence is ignored
          ; real trees should only contain one exact instance of dp
          ; may want to guarantee this later on with UID
          (dp 0
              (f 1)
              (g 1))))))
  (list
   (generational-tree-bp
    (dp 0
        (f 1)
        (g 1)))
   1
   (list
    (generational-tree-bp
     (b 0
        (c 0)
        (dp 0
            (f 1)
            (g 1)))))))
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (ad:abstract-atom 'dp '())
   (generational-tree-bp
    (dp 0
        (a 1)
        (b 1))))
  (list
   (generational-tree-bp
    (dp 0
        (a 1)
        (b 1)))
   0
   (list)))
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (ad:abstract-atom 'dp '())
   (generational-tree-bp
    (a 0)))
  #f)
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (ad:abstract-atom 'dp '())
   (generational-tree-bp
    (a 0
       (b 0)
       (c 0))))
  #f))

(let* ([bottom-left (node (cycle 1) '())]
       [bottom-right (node (tree-label (list) #f (list) #f #f) '())]
       [near-bottom-right-contents
        (widening (interpret-abstract-conjunction "b(α1)") #f "test" 3)]
       [near-bottom-right (node near-bottom-right-contents (list bottom-right))]
       [near-top-right-contents
        (tree-label (interpret-abstract-conjunction "b(γ1)") 0 (list) #f 2)]
       [near-top-right (node near-top-right-contents (list near-bottom-right))]
       [top-right-contents
        (widening (interpret-abstract-conjunction "a(α1)") #f "test" 1)]
       [top-right (node top-right-contents (list bottom-left near-top-right))])
  (check-equal?
   (shortest-branch-with-indices (list 1 3) top-right)
   (list top-right-contents near-top-right-contents near-bottom-right-contents)))

; TODO still need a test for "wrap"
; does queens have this? maybe some other problem?
(test-case
 "extracting the invertible function g from two related conjunctions"
 (check-equal?
  (extract-g-mapping
   3
   (cons
    (generational-tree-bp
     (collect
      ((g 1) (a 1))
      0
      (collect
       ((g 2) (a 2))
       1
       (collect
        ((g 4) (a 4))
        2
        (collect
         ((g 6) (a 6))
         3)
        (collect
         ((g 7) (a 7))
         3)
        (append
         ((a 6) (a 7) (a 4))
         3))
       (collect
        ((g 5) (a 5))
        2
        (collect
         ((g 5) (a 5))
         2))
       (append
        ((a 4) (a 5) (a 2))
        2
        (append
         ((a 4) (a 5) (a 2))
         2)))
      (collect
       ((g 3) (a 3))
       1
       (collect
        ((g 3) (a 3))
        1
        (collect
         ((g 3) (a 3))
         1)))
      (append
       ((a 2) (a 3) (a 1))
       1
       (append
        ((a 2) (a 3) (a 1))
        1
        (append
         ((a 2) (a 3) (a 1))
         1)))))
    0))
  ; note: fresh is w.r.t. previous generation
  ; argument number is flattened
  ; is this sufficiently accurate?
  ; what happens when there is a wrap constraint and subsequent conjunctions are not renamings?
  ; this cannot happen under depth-k abstraction - so subsequent conjunctions have to be renamings
  (list 'fresh 'fresh (identity-constraint 5) 'fresh 'fresh)))

(test-case
 "inverting a function which expresses the relation between two conjunctions"
 (check-equal?
  (invert-relation (list 'fresh (identity-constraint 3) 'fresh))
  (list 'fresh 'fresh (identity-constraint 2)))
 (check-equal?
  (invert-relation (list 'fresh (wrapper-constraint 'successor 3) 'fresh))
  (list 'fresh 'fresh (unwrapper-constraint 'successor 2))))

(test-case
 "checking whether f expresses exactly the desired mapping, across all related conjunctions"
 (check-equal?
  #t
  #f))