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

(require syntax/parse)
(require "abstract-multi-domain.rkt")

(define (interpret-abstract-atom str) #f)
(provide
 (proc-doc/names
  interpret-abstract-atom
  (-> string? abstract-atom?)
  (str)
  @{Interpret an abstract atom from a string @racket[str] (in Prolog-style notation, but with abstract variables) to a data structure for the control compiler.}))

(define (interpret-abstract-term-syntax term-stx)
  (syntax-parse term-stx
    [((~literal abstract-variable) NESTED-VAR)
     (interpret-abstract-variable-syntax #'NESTED-VAR)]))


(define (interpret-abstract-variable-syntax var-stx)
  (syntax-parse var-stx
    [((~literal abstract-variable-a) A-SYMBOL A-INDEX)
     (a (syntax->datum #'A-INDEX))]
    [((~literal abstract-variable-g) G-SYMBOL G-INDEX)
     (g (syntax->datum #'G-INDEX))]))
