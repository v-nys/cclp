#lang br/quicklang
(require
  "at-parser.rkt"
  "at-tokenizer.rkt")
(module+ reader
  (define (read-syntax source-path input-port)
    (define parse-tree (parse source-path (make-tokenizer input-port source-path)))
    (strip-bindings
     #`(module at-mod cclp/at-expander
         #,parse-tree)))
  (define (get-info port mod line col pos)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'cclp/at-colorer 'at-colorer)]
        [else default]))
    handle-query)
  (provide read-syntax get-info))