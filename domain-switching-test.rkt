#lang racket
(require rackunit "domain-switching.rkt" "abstract-multi-domain.rkt" "concrete-domain.rkt")
(check-equal? (get-maximum-abstract-var a? avar-index (list (g 1) (a 2) (g 5) (a 9) (a 6) (g 14))) (some 9) "Find the biggest a, where there is one")
(check-equal? (get-maximum-abstract-var g? avar-index (list (g 1) (a 2) (g 5) (a 9) (a 6) (g 14))) (some 14) "Find the biggest g, where there is one")
(check-equal? (get-maximum-abstract-var g? avar-index (list (a 1) (a 2) (a 5) (a 9) (a 6) (a 14))) (none) "Find the biggest g, where there is none")

(check-equal? (pre-abstract (variable "A")) (a 1))
(check-equal? (pre-abstract (function "dummy" '())) (g 1))

(let ([abstract-args (list (a 1) (a 1) (g 1) (g 2) (g 1))]
      ; should be able to do this more concisely using #lang lp building blocks, roughly as (expand (parse 'function "dummy(A,A,dummy2,dummy3,dummy2)"))
      [concrete-args (list (variable "A") (variable "A") (function "dummy2" '()) (function "dummy3" '()) (function "dummy2" '()))])
     (check-equal? (pre-abstract (function "dummy" concrete-args)) (abstract-function "dummy" abstract-args)))