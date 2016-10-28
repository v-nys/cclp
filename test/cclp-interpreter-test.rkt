#lang racket

(require rackunit)

(require "../src/abstract-multi-domain.rkt")
(require "../src/cclp-interpreter.rkt")

(check-equal?
 (interpret-abstract-atom "safe([γ1,γ2|α1])")
 (abstract-atom
  'safe
  (list (abstract-function 'cons (list (g 1) (abstract-function 'cons (list (g 2) (a 1))))))))

(check-equal?
 (interpret-abstract-term "foo(γ1)")
 (abstract-function 'foo (list (g 1))))

(check-equal?
 (interpret-abstract-conjunction "safe([γ1,γ2|α1]),perm(γ1,α1)")
 (list
  (abstract-atom
   'safe
   (list (abstract-function 'cons (list (g 1) (abstract-function 'cons (list (g 2) (a 1)))))))
  (abstract-atom
   'perm
   (list (g 1) (a 1)))))