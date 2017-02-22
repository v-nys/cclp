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
  (provide read-syntax))