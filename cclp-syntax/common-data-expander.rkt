#lang racket
(require
  (for-syntax syntax/parse)
  (prefix-in ad: cclp-common-data/abstract-multi-domain)
  (only-in cclp-analysis/syntax-utils odd-elems-as-list))

(define-syntax-rule (abstract-atom-with-args symbol "(" arg ... ")")
  (ad:abstract-atom (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide abstract-atom-without-args)

(define-syntax (abstract-atom stx)
  (syntax-parse stx [(_ args-or-nothing) (syntax/loc stx args-or-nothing)]))
(provide abstract-atom)

(define-syntax-rule (abstract-term specific-term) specific-term)
(provide abstract-term)

(define-syntax-rule (abstract-variable specific-var) specific-var)
(provide abstract-variable)

; M.O.: string is hier overbodig...
(define-syntax-rule (abstract-variable-a "a" index) (ad:a (quote index)))
(provide abstract-variable-a)

(define-syntax-rule (abstract-variable-g "g" index) (ad:g (quote index)))
(provide abstract-variable-g)

(define-syntax (abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "[]") '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 (ad:abstract-function (string->symbol "[]") '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 (abstract-lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 rest)))]))
(provide abstract-lplist)

(define-syntax (abstract-conjunction stx)
  (syntax-parse stx
    [(_ conjunct ...) (syntax/loc stx (odd-elems-as-list conjunct ...))]))
(provide abstract-conjunction)