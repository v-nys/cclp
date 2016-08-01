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
(require (prefix-in k: "abstract-knowledge.rkt"))
(require (prefix-in d: "abstract-multi-domain.rkt"))

; first order of business: abstractlp-program is followed by PAIRS of _KNOWLEDGE and _PERIOD
; how do we transform it into just a list of the (transformed) _KNOWLEDGE pattern variables?
; see Fear of Macros - this requires syntax pattern matching
; pattern is a (possibly empty) list of ((atom or rule) followed by period)
; so if the pattern is the empty list, return no syntax
; if it has two pieces (the second of which is a period), transform the first and ignore the second
; if it has more, "concatenate" the syntax?
(define-syntax (abstractlp-program stx)
  #'"this is an abstract LP")
(provide abstractlp-program)

(define #'(knowledge _ATOM-OR-RULE _PERIOD)
  #'(begin _ATOM-OR-RULE))
(provide knowledge)

(define #'(abstractlp-module-begin _PARSE-TREE ...)
  #'(#%module-begin
     _PARSE-TREE ...))
(provide (rename-out [abstractlp-module-begin #%module-begin])
         #%top-interaction)