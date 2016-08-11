#lang racket
(require rackunit)
(require "domain-boilerplate.rkt")

(require "../src/abstract-domain-ordering.rkt")

(check-true (>=-extension (parse-term "α1") (parse-term "γ1")))
(check-true (>=-extension (parse-term "α1") (parse-term "α2")))
(check-false (>=-extension (parse-term "γ1") (parse-term "α1")))
(check-true (>=-extension (parse-term "γ1") (parse-term "γ1")))
(check-true (>=-extension (parse-term "nil") (parse-term "nil")))
(check-false (>=-extension (parse-term "nil") (parse-term "nonnil")))
(check-true (>=-extension (parse-term "foo(γ1,bar(γ2,α1,γ1))") (parse-term "foo(nil,bar(nonnil,γ3,nil))")))
(check-false (>=-extension (parse-term "foo(γ1,bar(γ2,α1,γ1))") (parse-term "foo(nil,bar(nonnil,γ3,nonnil))")))
(check-true (>=-extension (parse-atom "foo(γ1,bar(γ2,α1,γ1))") (parse-atom "foo(nil,bar(nonnil,γ3,nil))")))
(check-false (>=-extension (parse-atom "foo(γ1,bar(γ2,α1,γ1))") (parse-atom "foo(nil,bar(nonnil,γ3,nonnil))")))
(check-false (>=-extension (parse-term "foo(γ1,bar(γ2,α1,γ1))") (parse-atom "foo(nil,bar(nonnil,γ3,nil))")) "terms and atoms cannot be unified")