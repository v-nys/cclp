#lang racket
(require rackunit)
(require "domain-boilerplate.rkt")
(require "../src/abstract-multi-domain.rkt")
(require "../src/abstraction-inspection-utils.rkt")

(check-equal? (assemble-var-indices g? (parse-term "α1")) (set))
(check-equal? (assemble-var-indices a? (parse-term "γ1")) (set))
(check-equal? (assemble-var-indices g? (parse-term "foo(bar)")) (set))
(check-equal? (assemble-var-indices a? (parse-term "foo(bar)")) (set))
(check-equal? (assemble-var-indices g? (parse-term "foo(bar(γ1,γ2,α3,α4))")) (set 1 2))
(check-equal? (assemble-var-indices a? (parse-term "foo(bar(γ1,γ2,α3,α4))")) (set 3 4))
(check-equal? (assemble-var-indices g? (parse-atom "foo(bar(γ1,γ2,α3,α4))")) (set 1 2))
(check-equal? (assemble-var-indices a? (parse-atom "foo(bar(γ1,γ2,α3,α4))")) (set 3 4))