#lang racket
(define test-regexp #rx".+-test\\.rkt$")
(map (λ (m) (dynamic-require m #f)) (filter (λ (p) (regexp-match test-regexp p)) (directory-list)))