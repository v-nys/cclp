#lang br/quicklang
(require
  "ggs-parser.rkt"
  "gg-tokenizer.rkt")
(module+ reader
  (define (read-syntax source-path input-port)
    (define parse-tree (parse source-path (make-tokenizer input-port source-path)))
    (strip-bindings
     #`(module ggs-mod cclp/ggs-expander
         #,parse-tree)))
  (provide read-syntax))