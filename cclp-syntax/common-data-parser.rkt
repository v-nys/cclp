; MIT License
;
; Copyright (c) 2016-2018 Vincent Nys
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

; TODO: grammar could be reduced in size (e.g. abstract-number-term is redundant)
; original goal was to write preprior-pair so that components were regular abstract atoms, too
; this is beyond my current understanding of Racket macros

; TODO: update any SYMBOL entries to allow for integration with AMB-AVAR-SYMBOL-A and -G

#lang brag
ag-atom : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN ag-termlist CLOSE-PAREN]
ag-termlist : ag-term (COMMA ag-term)*
ag-term : ag-variable | ag-function-term | ag-lplist
ag-variable : VARIABLE-IDENTIFIER
ag-function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN ag-termlist CLOSE-PAREN]) | ag-number-term
ag-number-term : NUMBER
ag-lplist : OPEN-LIST-PAREN [ag-term (COMMA ag-term)* [LIST-SEPARATOR (ag-lplist | ag-variable)]] CLOSE-LIST-PAREN
ag-rule : ((ag-atom IMPLIES ag-conjunction) | ag-atom) NUMBER
ag-conjunction : ag-conjunct (COMMA ag-conjunct)*
ag-conjunct : ag-atom | ag-concrete-multi
ag-concrete-multi : SYMBOL OPEN-PAREN ag-termlist CLOSE-PAREN

ag-fullai-rule : ag-fullai-rule-with-body | ag-fullai-rule-without-body
ag-fullai-rule-with-body : ag-abstract-atom-with-args LEADS-TO (ag-abstract-substitution | ag-fail) NUMBER
ag-fullai-rule-without-body : ag-abstract-atom-with-args NUMBER
ag-abstract-atom-with-args : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) OPEN-PAREN ag-abstract-termlist CLOSE-PAREN
ag-abstract-termlist : ag-abstract-term (COMMA ag-abstract-term)*
ag-abstract-term : ag-abstract-variable | ag-abstract-function-term | ag-abstract-lplist
ag-abstract-variable : (AMB-AVAR-SYMBOL-A NUMBER) | (AMB-AVAR-SYMBOL-G NUMBER)
ag-abstract-number : NUMBER
ag-abstract-function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN ag-abstract-termlist CLOSE-PAREN]) | ag-abstract-number
ag-abstract-lplist : OPEN-LIST-PAREN [ag-abstract-term (COMMA ag-abstract-term)* [LIST-SEPARATOR (ag-abstract-lplist | ag-abstract-variable)]] CLOSE-LIST-PAREN
ag-abstract-atom-without-args : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G)
ag-fail : SYMBOL

ag-abstract-conjunction : ag-abstract-conjunct (COMMA ag-abstract-conjunct)*
ag-abstract-conjunct : ag-abstract-atom | ag-multi-abstraction
ag-abstract-atom : ag-abstract-atom-with-args | ag-abstract-atom-without-args
ag-multi-abstraction : SYMBOL OPEN-PAREN ag-parameterized-abstract-conjunction COMMA SYMBOL COMMA ag-init COMMA ag-consecutive COMMA ag-final COMMA NUMBER CLOSE-PAREN
ag-parameterized-abstract-conjunction : ag-parameterized-abstract-atom (COMMA ag-parameterized-abstract-atom)*
ag-parameterized-abstract-atom : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) OPEN-PAREN ag-parameterized-abstract-termlist CLOSE-PAREN
ag-parameterized-abstract-term : ag-parameterized-abstract-variable | ag-parameterized-abstract-function-term | ag-parameterized-abstract-lplist
ag-parameterized-abstract-variable : (AMB-AVAR-SYMBOL-A LT NUMBER COMMA ag-symbolic-index COMMA NUMBER GT) | (AMB-AVAR-SYMBOL-G LT NUMBER COMMA ag-symbolic-index COMMA NUMBER GT)
ag-symbolic-index : NUMBER | SYMBOL | IPLUSONE
ag-parameterized-abstract-variable-g : AMB-AVAR-SYMBOL-G LT NUMBER COMMA ag-symbolic-index COMMA NUMBER GT
ag-init : CURLY-OPEN [ag-parameterized-abstract-variable EQ ag-abstract-term (COMMA ag-parameterized-abstract-variable EQ ag-abstract-term)*] CURLY-CLOSE
ag-consecutive : CURLY-OPEN [ag-parameterized-abstract-variable EQ ag-parameterized-abstract-term (COMMA ag-parameterized-abstract-variable EQ ag-parameterized-abstract-term)*] CURLY-CLOSE
ag-final : CURLY-OPEN [ag-parameterized-abstract-variable EQ ag-abstract-term (COMMA ag-parameterized-abstract-variable EQ ag-abstract-term)*] CURLY-CLOSE
ag-parameterized-abstract-function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN ag-parameterized-abstract-termlist CLOSE-PAREN]) | ag-parameterized-abstract-number-term
ag-parameterized-abstract-termlist : ag-parameterized-abstract-term (COMMA ag-parameterized-abstract-term)*
ag-parameterized-abstract-number-term : NUMBER
ag-parameterized-abstract-lplist : OPEN-LIST-PAREN [ag-parameterized-abstract-term (COMMA ag-parameterized-abstract-term)* [LIST-SEPARATOR (ag-parameterized-abstract-lplist | ag-parameterized-abstract-variable)]] CLOSE-LIST-PAREN

ag-abstract-substitution : CURLY-OPEN [ag-abstract-substitution-pair (COMMA ag-abstract-substitution-pair)*] CURLY-CLOSE
ag-abstract-substitution-pair : ag-abstract-term SLASH ag-abstract-term