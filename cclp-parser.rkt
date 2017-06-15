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

; should prohibit constants of the form symXYZ, where XYZ is a natural number

#lang brag
cclp-program : PROGRAM-DELIMITER program-section [FULL-EVALUATION-DELIMITER full-evaluation-section] [CONCRETE-CONSTANTS-DELIMITER concrete-constants-section] QUERY-DELIMITER abstract-atom

program-section : (rule PERIOD)*
atom : SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]
term : variable | function-term | lplist
variable : VARIABLE-IDENTIFIER
function-term : (SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]) | number-term
number-term : NUMBER
lplist : OPEN-LIST-PAREN [term (COMMA term)* [LIST-SEPARATOR (lplist | variable)]] CLOSE-LIST-PAREN
rule : (atom IMPLIES conjunction) | atom
conjunction : atom (COMMA atom)*

full-evaluation-section : (fullai-rule-with-body | fullai-rule-without-body)+
fullai-rule-with-body : abstract-atom-with-args LEADS-TO (abstract-substitution | fail) PERIOD
fullai-rule-without-body : abstract-atom-with-args PERIOD
abstract-atom-with-args : SYMBOL OPEN-PAREN abstract-term (COMMA abstract-term)* CLOSE-PAREN
abstract-atom-without-args : SYMBOL

abstract-term : abstract-variable | abstract-function-term | abstract-lplist
abstract-variable : abstract-variable-a | abstract-variable-g
abstract-variable-a : AVAR-SYMBOL-A NUMBER
abstract-variable-g : AVAR-SYMBOL-G NUMBER
abstract-number-term : abstract-number
abstract-number : NUMBER
abstract-function-term : (SYMBOL [OPEN-PAREN abstract-term (COMMA abstract-term)* CLOSE-PAREN]) | abstract-number-term

abstract-lplist : OPEN-LIST-PAREN [abstract-term (COMMA abstract-term)* [LIST-SEPARATOR (abstract-lplist | abstract-variable)]] CLOSE-LIST-PAREN
abstract-substitution : abstract-substitution-pair (COMMA abstract-substitution-pair)*
abstract-substitution-pair : abstract-variable SLASH abstract-term
abstract-atom : abstract-atom-with-args | abstract-atom-without-args
abstract-conjunction : abstract-atom (COMMA abstract-atom)*

concrete-constants-section : concrete-constant+
concrete-constant: SYMBOL

fail : SYMBOL