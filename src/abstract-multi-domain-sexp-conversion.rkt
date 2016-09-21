#lang racket
(require rackunit)
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/abstract-multi-domain-sexp-conversion.rkt")

(define (domain-elem->sexp elem))
(provide (contract-out [domain-elem->sexp (-> abstract-domain-elem? sexp?)]))

(define (sexp->abstract-term sexp))
(define (sexp->abstract-atom sexp))
(provide (contract-out [sexp->abstract-atom (-> sexp? abstract-atom?)]))