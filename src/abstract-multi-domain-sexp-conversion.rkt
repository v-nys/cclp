#lang racket

(define (domain-elem->sexp elem))
(provide (contract-out [domain-elem->sexp (-> abstract-domain-elem? sexp?)]))

(define (sexp->abstract-term sexp))
(define (sexp->abstract-atom sexp))
(provide (contract-out [sexp->abstract-atom (-> sexp? abstract-atom?)]))