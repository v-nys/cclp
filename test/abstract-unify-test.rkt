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
(require rackunit)
(require (for-syntax "../src/fullai-parser.rkt") (for-syntax "../src/fullai-reader.rkt") (for-syntax "../src/fullai-expander.rkt"))
(require "../src/fullai-expander.rkt")
(require (for-syntax syntax/strip-context))
(require "../src/abstract-unify.rkt")
(require "../src/data-utils.rkt")

(require "../src/abstract-multi-domain.rkt")
(require "../src/abstract-substitution.rkt")

(require "domain-boilerplate.rkt")

; the occurs check
(check-true (occurs (g 1) (parse-term "γ1")))
(check-false (occurs (g 1) (parse-term "γ2")))
(check-true (occurs (a 1) (parse-term "α1")))
(check-false (occurs (a 1) (parse-term "α2")))

(check-false (occurs (a 1) (parse-term "someconstant")))
(check-false (occurs (g 1) (parse-term "someconstant")))

(check-true (occurs (a 1) (parse-term "foo(bar(α1))")))
(check-false (occurs (a 1) (parse-term "foo(bar(α2))")))
(check-true (occurs (g 1) (parse-term "foo(bar(γ1))")))
(check-false (occurs (g 1) (parse-term "foo(bar(γ2))")))

(check-true (occurs (a 1) (parse-atom "foo(bar(α1))")))
(check-false (occurs (a 1) (parse-atom "foo(bar(α2))")))
(check-true (occurs (g 1) (parse-atom "foo(bar(γ1))")))
(check-false (occurs (g 1) (parse-atom "foo(bar(γ2))")))

; abstract unification
;(define-syntax-rule (term-equality-list t1 t2) (list (abstract-equality (parse-term t1) (parse-term t2))))
(define-syntax (term-equality-list stx)
  (syntax-case stx ()
    [(_) #'(list)]
    [(_ (t1 t2) rest ...) #'(cons (abstract-equality (parse-term t1) (parse-term t2)) (term-equality-list rest ...))]))
(check-equal? (abstract-unify (term-equality-list ("foo" "bar"))) (none) "unification of different functions")
(check-equal? (abstract-unify (term-equality-list ("foo" "foo"))) (some (list)) "unification of identical functions")
(check-equal? (abstract-unify (term-equality-list ("α1" "α2"))) (some (term-equality-list ("α1" "α2"))) "unification of equivalent variables")
(check-equal? (abstract-unify (term-equality-list ("α1" "γ1"))) (some (term-equality-list ("α1" "γ1"))) "unification of most general with more specific variable")
(check-equal? (abstract-unify (term-equality-list ("foo(bar(α1))" "foo(bar(γ1))"))) (some (term-equality-list ("α1" "γ1"))) "unification of nested terms")
(check-equal? (abstract-unify (term-equality-list ("γ1" "node(α1)"))) (some (term-equality-list ("α1" "γ2") ("γ1" "node(γ2)"))) "unification introducing new variable")

(check-equal? (abstract-unify (term-equality-list ("[γ1]" "[γ2]"))) (some (term-equality-list ("γ1" "γ2"))) "unification of lists")
(check-equal? (abstract-unify (term-equality-list ("append(α13,α14,α12)" "append(α19,α20,α18)"))) (some (term-equality-list ("α13" "α19") ("α14" "α20") ("α12" "α18"))) "unification of renamings respects the order of the arguments")
(check-equal? (abstract-unify (term-equality-list ("collect(γ3,α3)" "collect(node(α1),cons(α1,γ4))"))) (some (term-equality-list ("α1" "γ5") ("γ3" "node(γ5)") ("α3" "cons(γ5,γ4)"))))

(test-case "unification of more complex terms"
           (check-true (some? (abstract-unify (term-equality-list ("collect(γ1,α4)" "collect(tree(α5,α6),α7)")))))
           (check-equal? (list->set (some-v (abstract-unify (term-equality-list ("collect(γ1,α4)" "collect(tree(α5,α6),α7)")))))
                         (list->set (term-equality-list ("α5" "γ6") ("α6" "γ7") ("γ1" "tree(γ6,γ7)") ("α4" "α7")))))

; TODO unification for multi?
; TODO think of more scenarios?