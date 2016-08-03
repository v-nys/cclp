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
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require (for-syntax syntax/parse))
(require (for-syntax racket/match))

; helper function
; takes a syntax object, returns a list containing a syntax object for every other argument
(define-syntax (odd-elems-as-list stx)
  (syntax-case stx ()
    [(_ arg0) #'(list arg0)]
    [(_ arg0 arg1 arg2 ...) #'(cons arg0 (odd-elems-as-list arg2 ...))]))

; a logic program is just a list of clauses
(define-syntax (lp-program stx)
  (syntax-parse stx
    [(_) #'(list)]
    [(_ _KNOWLEDGE _PERIOD _MOREKNOWLEDGE ...) #'(cons _KNOWLEDGE (lp-program _MOREKNOWLEDGE ...))]))
(provide lp-program)

(define-syntax (atom stx)
  (syntax-parse stx
    [(_ symbol) #'(cd:atom (quote symbol) '())]
    [(_ symbol "(" arg ... ")") #'(cd:atom (quote symbol) (odd-elems-as-list arg ...))]))
(provide atom)

(define-syntax (arithmetic-test stx)
  (syntax-parse stx
    [(_ arg1 op arg2) #'(cd:atom (quote op) (list arg1 arg2))]))
(provide arithmetic-test)

(define-syntax (term stx)
  (syntax-parse stx
    [(_ VAR-OR-LIST-OR-MISC-FUNCTION) #'VAR-OR-LIST-OR-MISC-FUNCTION]))
(provide term)

(define-syntax-rule (variable VARIABLE-NAME) (cd:variable (quote VARIABLE-NAME)))
(provide variable)

(define-syntax-rule (number NUMBER) (cd:function (number->string (quote NUMBER)) '()))
(provide number)

(define-syntax (function-term stx)
  (syntax-parse stx
    [(_ symbol:str) #'(cd:function (quote symbol) '())]
    [(_ symbol) #'symbol]
    [(_ symbol "(" arg ... ")") #'(cd:function (quote symbol) (odd-elems-as-list arg ...))]
    ))
(provide function-term)

(define-syntax (lplist stx)
  (syntax-parse stx
    [(_ open-paren close-paren) #'(cd:function "nil" '())]
    [(_ open-paren term0 close-paren) #'(cd:function "cons" (list term0 (cd:function "nil" '())))]
    [(_ open-paren term0 comma-or-sep rest ... close-paren) (if (equal? "," (syntax->datum #'comma-or-sep))
                                                                #'(cd:function "cons" (list term0 (lplist open-paren rest ... close-paren)))
                                                                #'(cd:function "cons" (list term0 rest ...)))]))
(provide lplist)

(define-syntax-rule (rule atom ":-" conjunction) (ck:rule atom conjunction))
(provide rule)

(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_ conjunct ...) #'(list conjunct ...)]))
(provide conjunction)

(define #'(lp-module-begin _PARSE-TREE ...)
  #'(#%module-begin
     _PARSE-TREE ...))
(provide (rename-out [lp-module-begin #%module-begin]) #%top-interaction)