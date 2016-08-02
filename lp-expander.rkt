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

#lang br
(require (prefix-in cd: "concrete-domain.rkt"))
(require (for-syntax syntax/parse))

(define-syntax (lp-program stx)
  (syntax-parse stx
    [(_) #'(list)]
    [(_ _KNOWLEDGE _PERIOD _MOREKNOWLEDGE ...) #'(cons _KNOWLEDGE (lp-program _MOREKNOWLEDGE ...))]))
(provide lp-program)

; why doesn't odd-elements need to be defined for the syntax phase?
; because we include it *in* the syntax - we don't use it *for* the syntax
(define (odd-elements lst)
  (match lst [(list a) (list a)]
             [(list-rest a b c) (cons a c)]))

(define-syntax (atom stx)
  (syntax-parse stx
    [(_ symbol) #'(cd:atom (quote symbol) '())]
    [(_ symbol open-paren arg1 ... close-paren) #'(cd:atom (quote symbol) (odd-elements (list arg1 ...)))]
    ; TODO deal with the case of arithmetic operators
    ))
(provide atom)

(define-syntax (term stx)
  (syntax-parse stx
    ; clunky, but it should deal with variables now
    [(_ VAR-OR-LIST-OR-MISC-FUNCTION) (if (string? (syntax->datum #'VAR-OR-LIST-OR-MISC-FUNCTION)) #'(cd:variable (syntax->datum #'VAR-OR-LIST-OR-MISC-FUNCTION)) #'VAR-OR-LIST-OR-MISC-FUNCTION)]))
(provide term)

(define-syntax (function-term stx)
  (syntax-parse stx
    [(_ symbol) #'(cd:function (quote symbol) '())]
    [(_ symbol open-paren arg1 ... close-paren) #'(cd:function (quote symbol) (odd-elements (list arg1 ...)))]
    ))
(provide function-term)

(define-syntax (lplist stx)
  (syntax-parse stx
    ; only supports empty lists right now
    [(_ open-paren close-paren) #'(cd:function "nil" '())]
    ))
(provide lplist)

(define #'(lp-module-begin _PARSE-TREE ...)
  #'(#%module-begin
     _PARSE-TREE ...))
(provide (rename-out [lp-module-begin #%module-begin]) #%top-interaction)