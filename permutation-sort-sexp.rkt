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
#lang s-exp "abstractlp-expander.rkt"
(abstractlp-program
  (rule (atom "sort" "(" (term "X") "," (term "Y") ")") ":-" (conjunction (atom "perm" "(" (term "X") "," (term "Y") ")") "," (atom "ord" "(" (term "Y") ")")))
  "."
  (atom "perm" "(" (term (list "[" "]")) "," (term (list "[" "]")) ")")
  "."
  (rule
   (atom "perm" "(" (term (list "[" (term "X") "|" "Y" "]")) "," (term (list "[" (term "U") "|" "V" "]")) ")")
   ":-"
   (conjunction (atom "del" "(" (term "U") "," (term (list "[" (term "X") "|" "Y" "]")) "," (term "W") ")") "," (atom "perm" "(" (term "W") "," (term "V") ")")))
  "."
  (atom "del" "(" (term "X") "," (term (list "[" (term "X") "|" "Y" "]")) "," (term "Y") ")")
  "."
  (rule
   (atom "del" "(" (term "X") "," (term (list "[" (term "Y") "|" "U" "]")) "," (term (list "[" (term "Y") "|" "V" "]")) ")")
   ":-"
   (conjunction (atom "del" "(" (term "X") "," (term "U") "," (term "V") ")")))
  "."
  (atom "ord" "(" (term (list "[" "]")) ")")
  "."
  (atom "ord" "(" (term (list "[" (term "X") "]")) ")")
  "."
  (rule
   (atom "ord" "(" (term (list "[" (term "X") "," (term "Y") "|" "Z" "]")) ")")
   ":-"
   (conjunction (atom (term "X") "=<" (term "Y")) "," (atom "ord" "(" (term (list "[" (term "Y") "|" "Z" "]")) ")")))
  ".")