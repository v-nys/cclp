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
(require "permutation-sort.rkt")
(require "io-utils.rkt")
(require racket-tree-utils/src/tree racket-tree-utils/src/printer)

;(: main (-> Void))
(define (main)
  (define new-program "Analyze a new program.")
  (define continue-previous "Continue previous analysis.")
  (define selection (prompt-for-answer "What do you want to do?" new-program continue-previous))
  (begin
    (cond [(equal? selection new-program) (void)]
          [(equal? selection continue-previous) (void)])
    (main)))

;(: analyze-new-program (-> Void))
(define (analyze-new-program)
  (define perm-sort "Permutation sort.")
  (define sameleaves "Sameleaves.")
  (define go-back "None, return to the top level.")
  (define selection (prompt-for-answer "Which program do you want to analyze?" perm-sort sameleaves go-back))
  (cond ;[(equal? selection perm-sort) (interact-with (treeify "permsort(g_1,a_1)"))]
        [(equal? selection sameleaves) (void)]
        [(equal? selection go-back) (void)]))