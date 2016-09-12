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

(require (prefix-in ad: "abstract-multi-domain.rkt")) ; for abstract variables, functions, atoms,...
(require (prefix-in as: "abstract-substitution.rkt")) ; because output patterns can be obtained by applying a subsitution
(require (prefix-in fai: "fullai-domain.rkt")) ; for obvious reasons
(require "syntax-utils.rkt") ; to filter out odd elements
(require (for-syntax syntax/parse))

(define-syntax-rule (fullai-program rule ...) (list rule ...))
(provide fullai-program)

(define-syntax-rule (fullai-rule-with-body atom "->" subst ".") (fai:FullAIRule atom subst))
(provide fullai-rule-with-body)

(define-syntax-rule (fullai-rule-without-body atom ".") (fai:FullAIRule atom (list)))
(provide fullai-rule-without-body)

(define-syntax-rule (abstract-atom-with-args symbol "(" arg ... ")") (ad:abstract-atom (quote symbol) (odd-elems-as-list arg ...)))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-term specific-term) specific-term)
(provide abstract-term)

(define-syntax-rule (abstract-variable specific-var) specific-var)
(provide abstract-variable)

(define-syntax-rule (abstract-variable-a "α" index) (ad:a (quote index)))
(provide abstract-variable-a)

(define-syntax-rule (abstract-variable-g "γ" index) (ad:g (quote index)))
(provide abstract-variable-g)

(define-syntax (abstract-function-term stx)
  (syntax-parse stx
    [(_ symbol:str) #'(ad:abstract-function (quote symbol) '())] ; e.g. nil (in the abstract domain)
    [(_ num-term) #'num-term] ; e.g. 9 (in the abstract domain)
    [(_ symbol "(" arg ... ")") #'(ad:abstract-function (quote symbol) (odd-elems-as-list arg ...))]))
(provide abstract-function-term)

(define-syntax-rule (number-term NUMBER) (ad:function (number->string (quote NUMBER)) '()))
(provide number-term)

(define-syntax (abstract-lplist stx)
  (syntax-parse stx
    [(_ open-paren close-paren) #'(ad:abstract-function "nil" '())]
    [(_ open-paren term0 close-paren) #'(ad:abstract-function "cons" (list term0 (ad:abstract-function "nil" '())))]
    [(_ open-paren term0 "," rest ... close-paren) #'(ad:abstract-function "cons" (list term0 (abstract-lplist open-paren rest ... close-paren)))]
    [(_ open-paren term0 "|" rest ... close-paren) #'(ad:abstract-function "cons" (list term0 rest ...))]))
(provide abstract-lplist)

; empty substitutions make sense if we can just scratch the abstract atom
; e.g. lte(g1,g2) just disappears and does not need a substitution
(define-syntax (substitution stx)
  (syntax-parse stx
    [(_) (list)]
    [(_ lhs0 lhs1 ...) #'(odd-elems-as-list lhs0 lhs1 ...)]))
(provide substitution)

(define-syntax-rule (substitution-pair lhs "/" rhs) (as:abstract-equality lhs rhs))
(provide substitution-pair)

(define (test) (display "test!"))
(provide test)

(define #'(lp-module-begin _PARSE-TREE ...)
  #'(#%module-begin
     _PARSE-TREE ...))
(provide (rename-out [lp-module-begin #%module-begin]) #%top-interaction)