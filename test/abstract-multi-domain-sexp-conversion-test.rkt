#lang racket
(require rackunit)
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/abstract-multi-domain-sexp-conversion.rkt")

(check-equal? (abstract-domain-elem->sexp (abp:parse-abstract-atom "integers(γ1,α1)"))
              '(integers (γ 1) (α 1)))
(check-equal? (sexp->abstract-atom '(integers (γ 1) (α 1)))
              (abp:parse-abstract-atom "integers(γ1,α1)"))