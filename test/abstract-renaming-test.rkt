#lang racket
(require rackunit)
(require "../src/abstract-renaming.rkt")
(require "../src/domain-switching.rkt")
(require "concrete-domain-boilerplate.rkt")
(require "abstract-domain-boilerplate.rkt")
(require (prefix-in ak: "../src/abstract-knowledge.rkt"))

(test-case
 "the right abstract rule should be obtained"
 (let* ([rule (parse-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)")]
        [abstract-conjunction (list (parse-abstract-atom "collect(γ1,α1)"))]
        [abstract-rule (pre-abstract-rule rule)]
        [renamed-abstract-rule (rename-apart abstract-rule abstract-conjunction)]
        [expected
         (ak:abstract-rule (parse-abstract-atom "collect(tree(α6,α7),α8)")
                           (list (parse-abstract-atom "collect(α6,α9)")
                                 (parse-abstract-atom "collect(α7,α10)")
                                 (parse-abstract-atom "append(α9,α10,α8)")))])
   (check-equal?
    abstract-rule
    (ak:abstract-rule (parse-abstract-atom "collect(tree(α1,α2),α3)")
                      (list (parse-abstract-atom "collect(α1,α4)")
                            (parse-abstract-atom "collect(α2,α5)")
                            (parse-abstract-atom "append(α4,α5,α3)"))))
   (check-equal? renamed-abstract-rule expected)))

(test-case
 "a properly renamed full evaluation should be obtained"
 (let* ([full-eval (ak:full-evaluation (parse-abstract-atom "del(α1,[γ1|γ2],α2)")
                                       (parse-abstract-atom "del(γ3,[γ1|γ2],γ4)"))]
        [abstract-conjunction
         (parse-abstract-conjunction "del(α12,[γ18|γ19],α14),perm(α14,α13),ord([γ3,α12|α13])")]
        [renamed-abstract-rule (rename-apart full-eval abstract-conjunction)]
        [expected
         (ak:full-evaluation (parse-abstract-atom "del(α15,[γ20|γ21],α16)")
                             (parse-abstract-atom "del(γ22,[γ20|γ21],γ23)"))])
   (check-equal?
    renamed-abstract-rule
    expected)))
