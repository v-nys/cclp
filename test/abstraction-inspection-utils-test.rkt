#lang racket
(require rackunit)
(require "abstract-domain-boilerplate.rkt")
(require "../src/abstract-multi-domain.rkt")
(require "../src/abstraction-inspection-utils.rkt")
(require "../src/abstract-knowledge.rkt")

(check-equal? (assemble-var-indices g? (parse-abstract-term "α1")) (set))
(check-equal? (assemble-var-indices a? (parse-abstract-term "γ1")) (set))
(check-equal? (assemble-var-indices g? (parse-abstract-term "foo(bar)")) (set))

(check-equal? (assemble-var-indices g? (abstract-function "bar" (list))) (set))
(check-equal?
 (assemble-var-indices g? (abstract-function "foo" (list (abstract-function "bar" '()))))
 (set))

(check-equal? (assemble-var-indices a? (parse-abstract-term "foo(bar)")) (set))
(check-equal? (assemble-var-indices g? (parse-abstract-term "foo(bar(γ1,γ2,α3,α4))")) (set 1 2))
(check-equal? (assemble-var-indices a? (parse-abstract-term "foo(bar(γ1,γ2,α3,α4))")) (set 3 4))
(check-equal? (assemble-var-indices g? (parse-abstract-atom "foo(bar(γ1,γ2,α3,α4))")) (set 1 2))
(check-equal? (assemble-var-indices a? (parse-abstract-atom "foo(bar(γ1,γ2,α3,α4))")) (set 3 4))
(check-equal?
 (assemble-var-indices
  a?
  (full-evaluation (parse-abstract-atom "del(α1,[γ1|γ2],α2)")
                   (parse-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
 (set 3 4))
(check-equal?
 (assemble-var-indices
  g?
  (full-evaluation (parse-abstract-atom "del(α1,[γ1|γ2],α2)")
                   (parse-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
 (set 1 2 3 4))