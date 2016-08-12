#lang racket
(require rackunit)
(require "../src/abstract-renaming.rkt")
(require "../src/domain-switching.rkt")
(require "concrete-domain-boilerplate.rkt")
(require "domain-boilerplate.rkt")
(require (prefix-in ak: "../src/abstract-knowledge.rkt"))

; TODO could check more directly if I could parse abstract rules
(test-case "the right abstract rule should be obtained"
  (let* ([rule (parse-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)")]
         [abstract-conjunction (list (parse-atom "collect(γ1,α1)"))]
         [abstract-rule (pre-abstract-rule rule)]
         [renamed-abstract-rule (rename-apart abstract-rule abstract-conjunction)]
         [expected (ak:rule (parse-atom "collect(tree(α6,α7),α8)") (list (parse-atom "collect(α6,α9)") (parse-atom "collect(α7,α10)") (parse-atom "append(α9,α10,α8)")))])
           (check-equal? abstract-rule (ak:rule (parse-atom "collect(tree(α1,α2),α3)") (list (parse-atom "collect(α1,α4)") (parse-atom "collect(α2,α5)") (parse-atom "append(α4,α5,α3)"))))
           (check-equal? renamed-abstract-rule expected)))