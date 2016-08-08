#lang racket
(require rackunit)
(require (for-syntax "../src/fullai-parser.rkt") (for-syntax "../src/fullai-reader.rkt") (for-syntax "../src/fullai-expander.rkt"))
(require "../src/fullai-expander.rkt")
(require (for-syntax syntax/strip-context))

(require "../src/abstract-multi-domain.rkt")

(define (occurs x y) #t)

(check-true (occurs (a 1) (parse-atom "foo(bar(α1))")))
(check-false (occurs (a 1) (parse-atom "foo(bar(α2))")))
(check-true (occurs (g 1) (parse-atom "foo(bar(γ1))")))

(define-syntax (parse-atom stx)
  (define atom-parse (make-rule-parser abstract-atom-with-args))
  (syntax-case stx () [(_ the-atom)
                       (with-syntax ([PARSE-TREE (replace-context #'() (atom-parse (all-tokens "foo(bar(α1))")))])
                         #'PARSE-TREE)]))