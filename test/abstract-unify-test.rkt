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
(require "../src/abstract-unify.rkt")
(require "../src/data-utils.rkt")

(require "../src/abstract-multi-domain.rkt")
(require "../src/abstract-substitution.rkt")

(require "abstract-domain-boilerplate.rkt")
(require "../src/cclp-interpreter.rkt")

(require (for-syntax syntax/parse))

(require (for-syntax (only-in "../src/abstract-substitution.rkt" asubst)))

; the occurs check
(check-true (occurs (g 1) (interpret-abstract-term "γ1")))
(check-false (occurs (g 1) (interpret-abstract-term "γ2")))
(check-true (occurs (a 1) (interpret-abstract-term "α1")))
(check-false (occurs (a 1) (interpret-abstract-term "α2")))

(check-false (occurs (a 1) (interpret-abstract-term "someconstant")))
(check-false (occurs (g 1) (interpret-abstract-term "someconstant")))

(check-true (occurs (a 1) (interpret-abstract-term "foo(bar(α1))")))
(check-false (occurs (a 1) (interpret-abstract-term "foo(bar(α2))")))
(check-true (occurs (g 1) (interpret-abstract-term "foo(bar(γ1))")))
(check-false (occurs (g 1) (interpret-abstract-term "foo(bar(γ2))")))

(check-true (occurs (a 1) (interpret-abstract-atom "foo(bar(α1))")))
(check-false (occurs (a 1) (interpret-abstract-atom "foo(bar(α2))")))
(check-true (occurs (g 1) (interpret-abstract-atom "foo(bar(γ1))")))
(check-false (occurs (g 1) (interpret-abstract-atom "foo(bar(γ2))")))

(check-equal? (abstract-unify (asubst (foo bar)) 0) (none) "unification of different functions")
(check-equal? (abstract-unify (asubst (foo foo)) 0) (some (list)) "unification of identical functions")
(check-equal? (abstract-unify (asubst ((a 1) (a 2))) 0) (some (asubst ((a 1) (a 2)))) "unification of equivalent variables")
(check-equal? (abstract-unify (asubst ((a 1) (g 1))) 0) (some (asubst ((a 1) (g 1)))) "unification of most general with more specific variable")
(check-equal? (abstract-unify (asubst ((foo [(bar [(a 1)])]) (foo [(bar [(g 1)])]))) 0) (some (asubst ((a 1) (g 1)))) "unification of nested terms")
;(check-equal? (abstract-unify (term-equality-list ("γ1" "node(α1)")) 0) (some (term-equality-list ("α1" "γ2") ("γ1" "node(γ2)"))) "unification introducing new variable")

(check-equal? (abstract-unify (asubst ((cons [(g 1) nil]) (cons [(g 2) nil]))) 0) (some (asubst ((g 1) (g 2)))) "unification of lists")
(check-equal? (abstract-unify (asubst ((append [(a 13) (a 14) (a 12)]) (append [(a 19) (a 20) (a 18)]))) 0) (some (asubst ((a 13) (a 19)) ((a 14) (a 20)) ((a 12) (a 18)))) "unification of renamings respects the order of the arguments")
;(check-equal? (abstract-unify (term-equality-list ("collect(γ3,α3)" "collect(node(α1),cons(α1,γ4))")) 0) (some (term-equality-list ("α1" "γ5") ("γ3" "node(γ5)") ("α3" "cons(γ5,γ4)"))))

(test-case "unification of more complex terms"
           (check-true
            (some?
             (abstract-unify
              (asubst ((collect [(g 1) (a 4)]) (collect [(tree [(a 5) (a 6)]) (a 7)]))) 0)))
           (check-equal? (list->set (some-v (abstract-unify (asubst ((collect [(g 1) (a 4)]) (collect [(tree [(a 5) (a 6)]) (a 7)]))) 0)))
                         (list->set (asubst ((a 5) (g 6)) ((a 6) (g 7)) ((g 1) (tree [(g 6) (g 7)])) ((a 4) (a 7))))))

; TODO demonstrate what happens when bad offset is supplied
; TODO unification for multi?
; TODO think of more scenarios?