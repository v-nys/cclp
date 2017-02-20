#lang br/quicklang
(require
  "at-parser.rkt"
  "at-tokenizer.rkt")
(define (read-syntax source-path input-port)
  (define parse-tree (parse source-path (make-tokenizer input-port)))
  (strip-bindings
   #`(module at-mod cclp/at-expander
       #,parse-tree)))
(provide read-syntax)