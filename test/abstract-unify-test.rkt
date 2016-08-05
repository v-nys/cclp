#lang racket
(require rackunit)
(require "../src/fullai-reader.rkt" "../src/fullai-expander.rkt" "../src/abstract-atom-parser.rkt" "../src/abstract-multi-domain.rkt")

(define (occurs x y) #t)

; TODO parse these guys correctly
(check-true (occurs (a 1) "foo(bar(α1))"))
(check-false (occurs (a 1) "foo(bar(α2))"))
(check-true (occurs (g 1) "foo(bar(γ1))"))