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

#lang at-exp racket

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define-syntax-rule (while condition body0 body ...)
  (letrec ([f (λ () (when condition body0 body ... (f)))]) (f)))
(module+ test
  (require rackunit)
  (check-equal?
   (let ([val 0])
     (while
      (< val 10)
      (set! val (add1 val)))
     val)
   10))
(provide
 (form-doc
  (while condition body0 body ...)
  @{Iteration construct, as commonly used in imperative languages.}))

(define-syntax-rule (until condition body0 body ...)
  (while (not condition) body0 body ...))
(provide
 (form-doc
  (until condition body0 body ...)
  @{Iteration construct, as commonly used in imperative languages.}))