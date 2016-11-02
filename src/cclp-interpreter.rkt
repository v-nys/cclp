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
(require (only-in "cclp-reader.rkt" all-tokens))
(require (prefix-in ad: "abstract-multi-domain.rkt"))
(require (only-in racket-list-utils/utils odd-elems))


; the "interpret" functions are all basically the same thing
; could use macros to avoid boilerplate?

(define (interpret-abstract-conjunction str)
  (define conjunction-parse (make-rule-parser abstract-conjunction))
  (define parsed (conjunction-parse (all-tokens str)))
  (interpret-abstract-conjunction-syntax parsed))

(define (interpret-abstract-conjunction-syntax con-stx)
  (syntax-parse con-stx
    [((~literal abstract-conjunction) ATOM0 ATOM-OR-COMMA ...)
     (map interpret-abstract-atom-syntax (odd-elems (syntax->list #'(ATOM0 ATOM-OR-COMMA ...))))]))

(provide interpret-abstract-conjunction)

(define (interpret-abstract-atom str)
  (define atom-parse (make-rule-parser abstract-atom))
  (define parsed (atom-parse (all-tokens str)))
  (interpret-abstract-atom-syntax parsed))

(define (interpret-abstract-atom-syntax atom-stx)
  (syntax-parse atom-stx
    [((~literal abstract-atom) ((~literal abstract-atom-without-args) SYMBOL))
     (ad:abstract-atom (string->symbol (syntax->datum #'SYMBOL)) (list))]
    [((~literal abstract-atom) ((~literal abstract-atom-with-args) SYMBOL "(" ARG-OR-SEP ... ")"))
     (ad:abstract-atom
      (string->symbol (syntax->datum #'SYMBOL))
      (map interpret-abstract-term-syntax (odd-elems (syntax->list #'(ARG-OR-SEP ...)))))]))
(provide
 (proc-doc/names
  interpret-abstract-atom
  (-> string? ad:abstract-atom?)
  (str)
  @{Interpret an abstract atom from a string @racket[str] (in Prolog-style notation, but with abstract variables) to a data structure for the control compiler.}))

(define (interpret-abstract-term str)
  (define term-parse (make-rule-parser abstract-term))
  (interpret-abstract-term-syntax (term-parse (all-tokens str))))

(define (interpret-abstract-term-syntax term-stx)
  (syntax-parse term-stx
    [((~literal abstract-term) ((~literal abstract-variable) NESTED-VAR))
     (interpret-abstract-variable-syntax #'NESTED-VAR)]
    [((~literal abstract-term) ((~literal abstract-function-term)
      ((~literal abstract-number-term)
       ((~literal abstract-number) NUM))))
     (syntax->datum #'NUM)]
    [((~literal abstract-term) ((~literal abstract-function-term) FUNCTOR))
     (ad:abstract-function (string->symbol (syntax->datum #'FUNCTOR)) '())]
    [((~literal abstract-term) ((~literal abstract-function-term) FUNCTOR "(" ARG-OR-SEP ... ")"))
     (ad:abstract-function
      (string->symbol (syntax->datum #'FUNCTOR))
      (map interpret-abstract-term-syntax (odd-elems (syntax->list #'(ARG-OR-SEP ...)))))]
    [((~literal abstract-term) ((~literal abstract-lplist) "[" "]"))
     (ad:abstract-function 'nil '())]
    [((~literal abstract-term) ((~literal abstract-lplist) "[" TERM "]"))
     (ad:abstract-function
      'cons
      (list (interpret-abstract-term-syntax #'TERM) (ad:abstract-function 'nil '())))]

    [((~literal abstract-term) ((~literal abstract-lplist) "[" TERM0 "," REST ... "]"))
     (ad:abstract-function
      'cons
      (list
       (interpret-abstract-term-syntax #'TERM0)
       (interpret-abstract-term-syntax #'(abstract-term (abstract-lplist "[" REST ... "]")))))]
    [((~literal abstract-term) ((~literal abstract-lplist) "[" TERM0 "|" REST "]"))
     (ad:abstract-function
      'cons
      (list (interpret-abstract-term-syntax #'TERM0)
            (interpret-abstract-term-syntax #'(abstract-term REST))))]))

(provide
 (proc-doc/names
  interpret-abstract-term
  (-> string? ad:abstract-term?)
  (str)
  @{Interpret an abstract term from a string @racket[str] (in Prolog-style notation, but with abstract variables) to a data structure for the control compiler.}))

(define (interpret-abstract-variable-syntax var-stx)
  (syntax-parse var-stx
    [((~literal abstract-variable-a) A-SYMBOL A-INDEX)
     (ad:a (syntax->datum #'A-INDEX))]
    [((~literal abstract-variable-g) G-SYMBOL G-INDEX)
     (ad:g (syntax->datum #'G-INDEX))]))
