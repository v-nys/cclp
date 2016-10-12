#lang racket
(require rackunit)
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/abstract-multi-domain-sexp-conversion.rkt")

(check-equal? (abstract-domain-elem->sexp (abp:parse-abstract-atom "b"))
              '(b))
(check-equal? (abstract-domain-elem->sexp (abp:parse-abstract-atom "integers(γ1,α1)"))
              '(integers (γ sym1) (α sym1)))
(check-equal? (abstract-domain-elem->sexp
               (abp:parse-abstract-atom "atom_containing_list([γ1,γ2|γ3])"))
              '(atom_containing_list (cons (γ sym1) (cons (γ sym2) (γ sym3)))))
(check-equal? (abstract-domain-elem->sexp (abp:parse-abstract-atom "atom_containing_list([])"))
              '(atom_containing_list (nil)))
(check-equal? (sexp->abstract-atom '(integers (γ sym1) (α sym1)))
              (abp:parse-abstract-atom "integers(γ1,α1)"))
(check-equal? (sexp->abstract-atom '(a))
              (abp:parse-abstract-atom "a"))

(check-equal? (abstract-domain-elem->sexp (list)) '())
(check-equal?
 (abstract-domain-elem->sexp
  (list (abp:parse-abstract-atom "a") (abp:parse-abstract-atom "b") (abp:parse-abstract-atom "c")))
 '(cons (a) (cons (b) (cons (c) ()))))