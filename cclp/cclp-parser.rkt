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

; note: grammar could be reduced in size (e.g. abstract-number-term is redundant)
; original goal was to write preprior-pair so that components were regular abstract atoms, too
; this is beyond my current understanding of Racket macros

; TODO: update any SYMBOL entries to allow for integration with AMB-AVAR-SYMBOL-A and -G

#lang brag
cclp-program : PROGRAM-DELIMITER cclp-program-section [FULL-EVALUATION-DELIMITER cclp-full-evaluation-section] [PARTIAL-ORDER-DELIMITER cclp-partial-order-section] [K-DELIMITER NUMBER] QUERY-DELIMITER cclp-abstract-atom

cclp-program-section : (cclp-rule PERIOD)*
cclp-atom : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN cclp-termlist CLOSE-PAREN]
cclp-termlist : cclp-term (COMMA cclp-term)*
cclp-term : cclp-variable | cclp-function-term | cclp-lplist
cclp-variable : VARIABLE-IDENTIFIER
cclp-function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN cclp-termlist CLOSE-PAREN]) | cclp-number-term
cclp-number-term : NUMBER
cclp-lplist : OPEN-LIST-PAREN [cclp-term (COMMA cclp-term)* [LIST-SEPARATOR (cclp-lplist | cclp-variable)]] CLOSE-LIST-PAREN
cclp-rule : (cclp-atom IMPLIES cclp-conjunction) | cclp-atom
cclp-conjunction : cclp-atom (COMMA cclp-atom)*

cclp-full-evaluation-section : (cclp-fullai-rule-with-body | cclp-fullai-rule-without-body)+
cclp-fullai-rule-with-body : cclp-abstract-atom-with-args LEADS-TO (cclp-abstract-substitution | cclp-fail) PERIOD
cclp-fullai-rule-without-body : cclp-abstract-atom-with-args PERIOD
cclp-abstract-atom-with-args : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) OPEN-PAREN cclp-abstract-termlist CLOSE-PAREN
cclp-abstract-termlist : cclp-abstract-term (COMMA cclp-abstract-term)*
cclp-abstract-term : cclp-abstract-variable | cclp-abstract-function-term | cclp-abstract-lplist
cclp-abstract-variable : (AMB-AVAR-SYMBOL-A NUMBER) | (AMB-AVAR-SYMBOL-G NUMBER)
cclp-abstract-number : NUMBER
cclp-abstract-function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN cclp-abstract-termlist CLOSE-PAREN]) | cclp-abstract-number
cclp-abstract-atom-without-args : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G)

cclp-abstract-lplist : OPEN-LIST-PAREN [cclp-abstract-term (COMMA cclp-abstract-term)* [LIST-SEPARATOR (cclp-abstract-lplist | cclp-abstract-variable)]] CLOSE-LIST-PAREN
cclp-abstract-substitution : cclp-abstract-substitution-pair (COMMA cclp-abstract-substitution-pair)*
cclp-abstract-substitution-pair : cclp-abstract-variable SLASH cclp-abstract-term
cclp-abstract-atom : cclp-abstract-atom-with-args | cclp-abstract-atom-without-args

cclp-partial-order-section : cclp-partial-ordering-pair+
cclp-partial-ordering-pair : cclp-abstract-atom LT cclp-abstract-atom

cclp-fail : SYMBOL
