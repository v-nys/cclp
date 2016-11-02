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
(require (only-in "cclp-parser.rkt" make-rule-parser))
(require (only-in "cclp-reader.rkt" tokenize))

(require "abstract-multi-domain.rkt")

;abstract-atom-with-args : SYMBOL OPEN-PAREN abstract-term (COMMA abstract-term)* CLOSE-PAREN

; the "interpret" functions are all basically the same thing
; could use macros to avoid boilerplate?

(define (interpret-abstract-conjunction str) '())
(provide interpret-abstract-conjunction)

(define (interpret-abstract-atom str)
  (define atom-parse (make-rule-parser abstract-atom))
  (interpret-abstract-atom-syntax (tokenize str)))

(define (interpret-abstract-atom-syntax atom-stx)
  (syntax-parse atom-stx
    [((~literal abstract-atom-without-args) SYMBOL)
     (abstract-atom (syntax->datum #'SYMBOL) (list))]
    [((~literal abstract-atom-with-args) SYMBOL "(" ARG-OR-SEP ... ")")
     (abstract-atom
      (syntax->datum #'SYMBOL)
      (map interpret-abstract-term-syntax (syntax->list #'(ARG-OR-SEP ...))))]))
(provide
 (proc-doc/names
  interpret-abstract-atom
  (-> string? abstract-atom?)
  (str)
  @{Interpret an abstract atom from a string @racket[str] (in Prolog-style notation, but with abstract variables) to a data structure for the control compiler.}))

(define (interpret-abstract-term str)
  (define term-parse (make-rule-parser abstract-term))
  (interpret-abstract-term-syntax (tokenize str)))

(define (interpret-abstract-term-syntax term-stx)
  (syntax-parse term-stx
    [((~literal abstract-variable) NESTED-VAR)
     (interpret-abstract-variable-syntax #'NESTED-VAR)]
    [((~literal abstract-function-term)
      ((~literal abstract-number-term)
       ((~literal abstract-number) NUM)))
     (syntax->datum #'NUM)]
    [((~literal abstract-function-term) FUNCTOR)
     (abstract-function (syntax->datum #'FUNCTOR) '())]
    [((~literal abstract-function-term) FUNCTOR "(" ARG-OR-SEP ... ")")
     (abstract-function
      (syntax->datum #'FUNCTOR)
      (map interpret-abstract-term-syntax (syntax->list #'(ARG-OR-SEP ...))))]
    [((~literal abstract-lplist) "[]")
     (abstract-function 'nil '())]
    [((~literal abstract-lplist) "[" TERM "]")
     (abstract-function
      'cons
      (list (interpret-abstract-term-syntax #'TERM) (abstract-function 'nil '())))]
    [((~literal abstract-lplist) "[" TERM0 "," REST ... "]")
     (abstract-function
      'cons
      (list
       (interpret-abstract-term-syntax #'TERM0)
       (interpret-abstract-term-syntax #'(abstract-lplist "[" REST ... "]"))))]
    [((~literal abstract-lplist) "[" TERM0 "|" REST "]")
     (abstract-function
      'cons
      (list (interpret-abstract-term-syntax #'TERM0)
            (interpret-abstract-term-syntax #'REST)))]))

(provide
 (proc-doc/names
  interpret-abstract-term
  (-> string? abstract-term?)
  (str)
  @{Interpret an abstract term from a string @racket[str] (in Prolog-style notation, but with abstract variables) to a data structure for the control compiler.}))

(define (interpret-abstract-variable-syntax var-stx)
  (syntax-parse var-stx
    [((~literal abstract-variable-a) A-SYMBOL A-INDEX)
     (a (syntax->datum #'A-INDEX))]
    [((~literal abstract-variable-g) G-SYMBOL G-INDEX)
     (g (syntax->datum #'G-INDEX))]))
