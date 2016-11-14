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

(require "../src/abstract-multi-domain.rkt")
(require "../src/abstract-analysis.rkt")
(require "../src/cclp-interpreter.rkt")
(require "../src/generational-tree.rkt")
(require "../src/similarity.rkt")

; makes defining non-degenerated trees (with atoms without args) much easier
(define-syntax (generational-tree-bp stx)
  (syntax-case stx ()
    [(_ (SYM GEN TREE ...))
     #'(node
        (atom-with-generation (abstract-atom 'SYM '()) GEN)
        (list (generational-tree-bp TREE) ...))]))

(test-case
 "finding level-0 instances of a target, and their complement, at a particular level"
 (check-equal?
  (dp-zero-subtree-depth-complement-at-level
   (abstract-atom 'dp '())
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
   (abstract-atom 'dp '())
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
   (abstract-atom 'dp '())
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
   (abstract-atom 'dp '())
   (generational-tree-bp
    (a 0)))
  #f)
 (check-equal?
  (find-dp-zero-subtree-depth-complement
   (abstract-atom 'dp '())
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

