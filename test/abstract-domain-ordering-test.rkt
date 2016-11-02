#lang racket
(require rackunit)
(require "abstract-domain-boilerplate.rkt")
(require "../src/abstract-domain-ordering.rkt")
(require "../src/cclp-interpreter.rkt")

(check-true (>=-extension (interpret-abstract-term "α1") (interpret-abstract-term "γ1")))
(check-true (>=-extension (interpret-abstract-term "α1") (interpret-abstract-term "α2")))
(check-false (>=-extension (interpret-abstract-term "γ1") (interpret-abstract-term "α1")))
(check-true (>=-extension (interpret-abstract-term "γ1") (interpret-abstract-term "γ1")))
(check-true (>=-extension (interpret-abstract-term "nil") (interpret-abstract-term "nil")))
(check-false (>=-extension (interpret-abstract-term "nil") (interpret-abstract-term "nonnil")))
(check-true
 (>=-extension
  (interpret-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
  (interpret-abstract-term "foo(nil,bar(nonnil,γ3,nil))")))
(check-false
 (>=-extension
  (interpret-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
  (interpret-abstract-term "foo(nil,bar(nonnil,γ3,nonnil))")))
(check-true
 (>=-extension
  (interpret-abstract-atom "foo(γ1,bar(γ2,α1,γ1))")
  (interpret-abstract-atom "foo(nil,bar(nonnil,γ3,nil))")))
(check-false
 (>=-extension
  (interpret-abstract-atom "foo(γ1,bar(γ2,α1,γ1))")
  (interpret-abstract-atom "foo(nil,bar(nonnil,γ3,nonnil))")))
(check-false
 (>=-extension
  (interpret-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
  (interpret-abstract-atom "foo(nil,bar(nonnil,γ3,nil))")))
(check-true
 (>=-extension
  (interpret-abstract-atom "sift([γ1|α1],α2)")
  (interpret-abstract-atom "sift([γ2|α4],α1)"))) ; renaming should be implicit
(check-true
 (>=-extension
  (list (interpret-abstract-atom "sift([γ1|α1],α2)"))
  (list (interpret-abstract-atom "sift([γ2|α4],α1)"))))
(check-false
 (>=-extension
  (list (interpret-abstract-atom "sift([γ1|γ2],α2)"))
  (list (interpret-abstract-atom "sift([γ3|α4],α1)"))))
(check-true
 (>=-extension
  (list
   (interpret-abstract-atom "sift([γ1|α1],α2)")
   (interpret-abstract-atom "filter(α2,α3)"))
  (list
   (interpret-abstract-atom "sift([γ2|α4],α1)")
   (interpret-abstract-atom "filter(α1,α5)"))))
(check-false
 (>=-extension
  (list
   (interpret-abstract-atom "sift([γ1|α1],α2)")
   (interpret-abstract-atom "filter(α2,α3)"))
  (list
   (interpret-abstract-atom "sift([γ2|α4],α1)")
   (interpret-abstract-atom "filter(α6,α5)"))))