#lang racket
(require "abstract-multi-domain.rkt")

(define (abstract-domain-elem->sexp elem)
  (match elem
    [(a i) (list 'α i)]
    [(g i) (list 'γ i)]
    [(abstract-atom symbol args) (cons symbol (map abstract-domain-elem->sexp args))]
    [(abstract-function functor args) (cons functor (map abstract-domain-elem->sexp args))]))
; can the contract be made more strict?
(provide (contract-out [abstract-domain-elem->sexp (-> abstract-domain-elem? list?)]))

(define (sexp->abstract-term sexp)
  (match sexp
    [(list 'α i) (a i)]
    [(list 'γ i) (g i)]
    [(list-rest symbol args) (abstract-function symbol (map sexp->abstract-term args))]))

(define (sexp->abstract-atom sexp)
  (abstract-atom (car sexp)
                 (map sexp->abstract-term (cdr sexp))))
(provide (contract-out [sexp->abstract-atom (-> list? abstract-atom?)]))

(define (abstract-conjunction->sexp con)
  (foldr (λ (atm acc) (list 'cons (abstract-domain-elem->sexp atm) acc)) '() con))
(provide (contract-out [abstract-conjunction->sexp (-> (listof abstract-atom?) list?)]))