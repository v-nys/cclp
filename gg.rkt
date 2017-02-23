#lang br/quicklang
(require
  "gg-parser.rkt"
  "gg-tokenizer.rkt")
(module+ reader
  (define (read-syntax source-path input-port)
    (define parse-tree (parse source-path (make-tokenizer input-port source-path)))
    (strip-bindings
     #`(module gg-mod cclp/gg-expander
         #,parse-tree)))
  (define (get-info port mod line col pos)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'cclp/gg-colorer 'gg-colorer)]
        [else default]))
    handle-query)
  (provide read-syntax))