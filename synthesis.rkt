; MIT License
;
; Copyright (c) 2017 Vincent Nys
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
(require
  scribble/srcdoc
  (only-in cclp/interaction analysis-tree proceed)
  cclp/abstract-analysis
  cclp/concrete-domain
  racket-tree-utils/src/tree)
(require (for-doc scribble/manual))

(define (A-nodes tree)
  (define (A-nodes* tree)
    (match tree
      [(node (cycle i) (list))
       (set i)]
      [(node lbl ch)
       (apply set-union (set) (map A-nodes* ch))]))
  (match tree
    [(node lbl (list-rest c cs))
     (apply
      set-union
      (set (label-index lbl))
      (map A-nodes* (cons c cs)))]
    [(node lbl (list))
     (set)]))
(module+ test
  (require
    rackunit
    repeated-application
    (prefix-in permsort: cclp-programs/permutation-sort))
  (let ([ps-tree
         (analysis-tree
          (apply↑* proceed permsort:initial-program-analysis))])
    (check-equal?
     (A-nodes ps-tree)
     (set 1 5))))
(provide
 (proc-doc/names
  A-nodes
  (-> node? set?)
  (tree)
  @{Collects the indices of all nodes in @racket[tree] which can correspond to a synthesized state.}))