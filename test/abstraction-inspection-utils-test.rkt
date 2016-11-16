#lang racket
(require rackunit)
(require "abstract-domain-boilerplate.rkt")
(require "../src/abstract-multi-domain.rkt")
(require "../src/abstraction-inspection-utils.rkt")
(require "../src/abstract-knowledge.rkt")
(require "../src/cclp-interpreter.rkt")

(check-equal? (assemble-var-indices g? (interpret-abstract-term "α1")) (set))
(check-equal? (assemble-var-indices a? (interpret-abstract-term "γ1")) (set))
(check-equal? (assemble-var-indices g? (interpret-abstract-term "foo(bar)")) (set))

(check-equal? (assemble-var-indices g? (abstract-function "bar" (list))) (set))
(check-equal?
 (assemble-var-indices g? (abstract-function "foo" (list (abstract-function "bar" '()))))
 (set))

(check-equal? (assemble-var-indices a? (interpret-abstract-term "foo(bar)")) (set))
(check-equal? (assemble-var-indices g? (interpret-abstract-term "foo(bar(γ1,γ2,α3,α4))")) (set 1 2))
(check-equal? (assemble-var-indices a? (interpret-abstract-term "foo(bar(γ1,γ2,α3,α4))")) (set 3 4))
(check-equal? (assemble-var-indices g? (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")) (set 1 2))
(check-equal? (assemble-var-indices a? (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")) (set 3 4))

(check-equal?
 (assemble-var-indices
  g?
  (list
   (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")
   (interpret-abstract-atom "foo(bar(γ5,γ6,α7,α8))")))
 (set 1 2 5 6))
(check-equal?
 (assemble-var-indices
  a?
  (list
   (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")
   (interpret-abstract-atom "foo(bar(γ5,γ6,α7,α8))")))
 (set 3 4 7 8))

(check-equal?
 (assemble-var-indices
  a?
  (full-evaluation (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                   (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
 (set 1 2))
(check-equal?
 (assemble-var-indices
  g?
  (full-evaluation (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                   (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
 (set 1 2 3 4))

(check-equal?
 (contains-subterm? (interpret-abstract-conjunction "bar(α2),foo(q(γ7),α1)") (g 7)) #t)
(check-equal?
 (contains-subterm? (interpret-abstract-conjunction "bar(α2),foo(q(γ7),α1)") (g 6)) #f)