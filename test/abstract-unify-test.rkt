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
(require (for-syntax "../src/fullai-parser.rkt") (for-syntax "../src/fullai-reader.rkt") (for-syntax "../src/fullai-expander.rkt"))
(require "../src/fullai-expander.rkt")
(require (for-syntax syntax/strip-context))
(require "../src/abstract-unify.rkt")

(require "../src/abstract-multi-domain.rkt")

(check-true (occurs (a 1) (parse-atom "foo(bar(α1))")))
(check-false (occurs (a 1) (parse-atom "foo(bar(α2))")))
(check-true (occurs (g 1) (parse-atom "foo(bar(γ1))")))

(define-syntax (parse-atom stx)
  (define atom-parse (make-rule-parser abstract-atom-with-args))
  (syntax-case stx () [(_ the-atom)
                       (with-syntax ([PARSE-TREE (replace-context #'() (atom-parse (all-tokens "foo(bar(α1))")))])
                         #'PARSE-TREE)]))