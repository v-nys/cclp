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
(require rackunit
         "../src/domain-switching.rkt"
         "../src/abstract-multi-domain.rkt"
         "../src/concrete-domain.rkt"
         "../src/data-utils.rkt"
         "abstract-domain-boilerplate.rkt"
         "concrete-domain-boilerplate.rkt"
         (prefix-in ak: "../src/abstract-knowledge.rkt"))

(check-equal?
 (get-maximum-abstract-var
  a?
  avar-index
  (list (g 1) (a 2) (g 5) (a 9) (a 6) (g 14)))
 (some 9)
 "Find the biggest a, where there is one")
(check-equal? (get-maximum-abstract-var g? avar-index (list (g 1) (a 2) (g 5) (a 9) (a 6) (g 14))) (some 14) "Find the biggest g, where there is one")
(check-equal? (get-maximum-abstract-var g? avar-index (list (a 1) (a 2) (a 5) (a 9) (a 6) (a 14))) (none) "Find the biggest g, where there is none")

(check-equal? (pre-abstract (variable "A")) (a 1) "single new variable case")
(check-equal? (pre-abstract (function "dummy" '())) (g 1) "single new constant case")

(check-equal? (pre-abstract-aux-constant (function "dummy" '()) (hash)) (cons (g 1) (hash (function "dummy" '()) (g 1))) "case of constant with no existing mapping")
(check-equal? (pre-abstract-aux-constant 3 (hash)) (cons (g 1) (hash 3 (g 1))) "case of constant with no existing mapping")
(check-equal? (pre-abstract-aux-constant (function "dummy" '()) (hash (function "dummy" '()) (g 1))) (cons (g 1) (hash (function "dummy" '()) (g 1))) "case of constant with an existing mapping")
(check-equal? (pre-abstract-aux-constant (function "dummy2" '()) (hash (function "dummy1" '()) (g 1))) (cons (g 2) (hash (function "dummy1" '()) (g 1) (function "dummy2" '()) (g 2))) "case of constant when there is a mapping for another constant")

(let ([abstract-args (list (a 1) (a 1) (a 2) (g 1) (g 2) (g 1))]
      ; should be able to do this more concisely using #lang lp building blocks, roughly as (expand (parse 'function "dummy(A,A,dummy2,dummy3,dummy2)"))
      [concrete-args (list (variable "A") (variable "A") (variable "B") (function "dummy2" '()) (function "dummy3" '()) (function "dummy2" '()))])
     (check-equal? (pre-abstract (function "dummy" concrete-args)) (abstract-function "dummy" abstract-args) "abstracting a complex term"))

(check-equal? (pre-abstract-rule (parse-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)"))
              (ak:abstract-rule (parse-abstract-atom "collect(tree(α1,α2),α3)") (list (parse-abstract-atom "collect(α1,α4)") (parse-abstract-atom "collect(α2,α5)") (parse-abstract-atom "append(α4,α5,α3)"))))
