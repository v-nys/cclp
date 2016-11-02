#lang racket
(require rackunit)
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/abstract-multi-domain-sexp-conversion.rkt")
(require "../src/cclp-interpreter.rkt")

(check-equal? (abstract-domain-elem->sexp (interpret-abstract-atom "b"))
              '(b))
(check-equal? (abstract-domain-elem->sexp (interpret-abstract-atom "integers(γ1,α1)"))
              '(integers (γ sym1) (α sym1)))
(check-equal? (abstract-domain-elem->sexp
               (interpret-abstract-atom "atom_containing_list([γ1,γ2|γ3])"))
              '(atom_containing_list (cons (γ sym1) (cons (γ sym2) (γ sym3)))))
(check-equal? (abstract-domain-elem->sexp (interpret-abstract-atom "atom_containing_list([])"))
              '(atom_containing_list (nil)))
(check-equal? (sexp->abstract-atom '(integers (γ sym1) (α sym1)))
              (interpret-abstract-atom "integers(γ1,α1)"))
(check-equal? (sexp->abstract-atom '(a))
              (interpret-abstract-atom "a"))

(check-equal? (abstract-domain-elem->sexp (list)) '())
(check-equal?
 (abstract-domain-elem->sexp
  (list (interpret-abstract-atom "a") (interpret-abstract-atom "b") (interpret-abstract-atom "c")))
 '(cons (a) (cons (b) (cons (c) ()))))