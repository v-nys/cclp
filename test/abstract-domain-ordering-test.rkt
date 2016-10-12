#lang racket
(require rackunit)
(require "abstract-domain-boilerplate.rkt")
(require "../src/abstract-domain-ordering.rkt")

(check-true (>=-extension (parse-abstract-term "α1") (parse-abstract-term "γ1")))
(check-true (>=-extension (parse-abstract-term "α1") (parse-abstract-term "α2")))
(check-false (>=-extension (parse-abstract-term "γ1") (parse-abstract-term "α1")))
(check-true (>=-extension (parse-abstract-term "γ1") (parse-abstract-term "γ1")))
(check-true (>=-extension (parse-abstract-term "nil") (parse-abstract-term "nil")))
(check-false (>=-extension (parse-abstract-term "nil") (parse-abstract-term "nonnil")))
(check-true
 (>=-extension
  (parse-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
  (parse-abstract-term "foo(nil,bar(nonnil,γ3,nil))")))
(check-false
 (>=-extension
  (parse-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
  (parse-abstract-term "foo(nil,bar(nonnil,γ3,nonnil))")))
(check-true
 (>=-extension
  (parse-abstract-atom "foo(γ1,bar(γ2,α1,γ1))")
  (parse-abstract-atom "foo(nil,bar(nonnil,γ3,nil))")))
(check-false
 (>=-extension
  (parse-abstract-atom "foo(γ1,bar(γ2,α1,γ1))")
  (parse-abstract-atom "foo(nil,bar(nonnil,γ3,nonnil))")))
(check-false
 (>=-extension
  (parse-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
  (parse-abstract-atom "foo(nil,bar(nonnil,γ3,nil))")))
(check-true
 (>=-extension
  (parse-abstract-atom "sift([γ1|α1],α2)")
  (parse-abstract-atom "sift([γ2|α4],α1)"))) ; renaming should be implicit
(check-true
 (>=-extension
  (list (parse-abstract-atom "sift([γ1|α1],α2)"))
  (list (parse-abstract-atom "sift([γ2|α4],α1)"))))
(check-false
 (>=-extension
  (list (parse-abstract-atom "sift([γ1|γ2],α2)"))
  (list (parse-abstract-atom "sift([γ3|α4],α1)"))))
(check-true
 (>=-extension
  (list
   (parse-abstract-atom "sift([γ1|α1],α2)")
   (parse-abstract-atom "filter(α2,α3)"))
  (list
   (parse-abstract-atom "sift([γ2|α4],α1)")
   (parse-abstract-atom "filter(α1,α5)"))))
(check-false
 (>=-extension
  (list
   (parse-abstract-atom "sift([γ1|α1],α2)")
   (parse-abstract-atom "filter(α2,α3)"))
  (list
   (parse-abstract-atom "sift([γ2|α4],α1)")
   (parse-abstract-atom "filter(α6,α5)"))))