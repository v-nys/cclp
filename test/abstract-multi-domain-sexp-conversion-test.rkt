#lang racket
(require rackunit)
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/abstract-multi-domain-sexp-conversion.rkt")

(check-equal? (abstract-domain-elem->sexp (abp:parse-abstract-atom "integers(γ1,α1)"))
              '(integers (γ 1) (α 1)))
(check-equal? (abstract-domain-elem->sexp (abp:parse-abstract-atom "atom_containing_list([γ1,γ2|γ3])"))
              '(atom_containing_list ))
(check-equal? (abstract-domain-elem->sexp (abp:parse-abstract-atom "atom_containing_list([])"))
              '(atom_containing_list ()))
(check-equal? (sexp->abstract-atom '(integers (γ 1) (α 1)))
              (abp:parse-abstract-atom "integers(γ1,α1)"))