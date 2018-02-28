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
atom : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]
term : variable | function-term | lplist
variable : VARIABLE-IDENTIFIER
function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]) | number-term
number-term : NUMBER
lplist : OPEN-LIST-PAREN [term (COMMA term)* [LIST-SEPARATOR (lplist | variable)]] CLOSE-LIST-PAREN
rule : (atom IMPLIES conjunction) | atom
conjunction : atom (COMMA atom)*

fullai-rule : fullai-rule-with-body | fullai-rule-without-body
fullai-rule-with-body : abstract-atom-with-args LEADS-TO (abstract-substitution | fail) NUMBER
fullai-rule-without-body : abstract-atom-with-args NUMBER
abstract-atom-with-args : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) OPEN-PAREN abstract-termlist CLOSE-PAREN
abstract-termlist : abstract-term (COMMA abstract-term)*
abstract-term : abstract-variable | abstract-function-term | abstract-lplist
abstract-variable : (AMB-AVAR-SYMBOL-A NUMBER) | (AMB-AVAR-SYMBOL-G NUMBER)
abstract-number : NUMBER
abstract-function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN abstract-termlist CLOSE-PAREN]) | abstract-number
abstract-lplist : OPEN-LIST-PAREN [abstract-term (COMMA abstract-term)* [LIST-SEPARATOR (abstract-lplist | abstract-variable)]] CLOSE-LIST-PAREN
abstract-atom-without-args : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G)
fail : SYMBOL

abstract-conjunction : abstract-conjunct (COMMA abstract-conjunct)*
abstract-conjunct : abstract-atom | multi-abstraction
abstract-atom : abstract-atom-with-args | abstract-atom-without-args
multi-abstraction : SYMBOL OPEN-PAREN parameterized-abstract-conjunction SYMBOL init consecutive final NUMBER CLOSE-PAREN
parameterized-abstract-conjunction : parameterized-abstract-atom (COMMA parameterized-abstract-atom)*
parameterized-abstract-atom : (SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) OPEN-PAREN parameterized-abstract-termlist CLOSE-PAREN
parameterized-abstract-term : parameterized-abstract-variable | parameterized-abstract-function-term | parameterized-abstract-lplist
parameterized-abstract-variable : (AMB-AVAR-SYMBOL-A LT NUMBER COMMA symbolic-index COMMA NUMBER GT) | (AMB-AVAR-SYMBOL-G LT NUMBER COMMA symbolic-index COMMA NUMBER GT)
symbolic-index : NUMBER | SYMBOL | IPLUSONE
parameterized-abstract-variable-g : AMB-AVAR-SYMBOL-G LT NUMBER COMMA symbolic-index COMMA NUMBER GT
init : CURLY-OPEN [parameterized-abstract-variable EQ abstract-term (COMMA parameterized-abstract-variable EQ abstract-term)*] CURLY-CLOSE
consecutive : CURLY-OPEN [parameterized-abstract-variable EQ parameterized-abstract-term (COMMA parameterized-abstract-variable EQ parameterized-abstract-term)*] CURLY-CLOSE
final : CURLY-OPEN [parameterized-abstract-variable EQ abstract-term (COMMA parameterized-abstract-variable EQ abstract-term)*] CURLY-CLOSE
parameterized-abstract-function-term : ((SYMBOL | AMB-AVAR-SYMBOL-A | AMB-AVAR-SYMBOL-G) [OPEN-PAREN parameterized-abstract-termlist CLOSE-PAREN]) | parameterized-abstract-number-term
parameterized-abstract-termlist : parameterized-abstract-term (COMMA parameterized-abstract-term)*
parameterized-abstract-number-term : NUMBER
parameterized-abstract-lplist : OPEN-LIST-PAREN [parameterized-abstract-term (COMMA parameterized-abstract-term)* [LIST-SEPARATOR (parameterized-abstract-lplist | parameterized-abstract-variable)]] CLOSE-LIST-PAREN

abstract-substitution : CURLY-OPEN abstract-substitution-pair (COMMA abstract-substitution-pair)* CURLY-CLOSE
abstract-substitution-pair : abstract-variable SLASH abstract-term