#lang racket
(module reader racket
  (require (only-in cclp-analysis/cclp-reader read-syntax))
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'cclp/colorer 'color-cclp)]
        [else default]))
    handle-query)
  (provide read-syntax get-info))
