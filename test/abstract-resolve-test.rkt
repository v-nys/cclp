#lang racket
(require rackunit "../src/data-utils.rkt" "domain-boilerplate.rkt" "concrete-domain-boilerplate.rkt" "../src/domain-switching.rkt" "../src/abstract-resolve.rkt" (prefix-in ck: "../src/concrete-knowledge.rkt") (prefix-in ak: "../src/abstract-knowledge.rkt"))
(check-equal? (abstract-step (parse-atom "collect(γ1,α1)")
                             (pre-abstract-rule (parse-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)")))
              (2-tuple (none) (list (parse-atom "collect(γ2,α2)") (parse-atom "collect(γ3,α3)") (parse-atom "append(α2,α3,α4)"))))