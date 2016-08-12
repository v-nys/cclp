#lang racket
(require rackunit
         "../src/data-utils.rkt"
         "domain-boilerplate.rkt"
         "concrete-domain-boilerplate.rkt"
         "../src/abstract-multi-domain.rkt"
         "../src/domain-switching.rkt"
         "../src/abstract-resolve.rkt"
         (prefix-in ck: "../src/concrete-knowledge.rkt")
         (prefix-in ak: "../src/abstract-knowledge.rkt")
         "../src/abstract-renaming.rkt"
         "../src/abstract-substitution.rkt")
(check-equal? (abstract-step (parse-atom "collect(γ1,α1)")
                             (rename-apart (pre-abstract-rule (parse-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)"))
                                           (list (parse-atom "collect(γ1,α1)")))
                             0)
              (some (2-tuple (list (abstract-equality (a 6) (g 7)) (abstract-equality (a 7) (g 8)) (abstract-equality (g 1) (parse-term "tree(γ7,γ8)")) (abstract-equality (a 1) (a 8))) (list (parse-atom "collect(γ7,α9)") (parse-atom "collect(γ8,α10)") (parse-atom "append(α9,α10,α8)")))))