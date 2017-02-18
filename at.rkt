#lang br/quicklang
(require
  "at-parser.rkt"
  "at-tokenizer.rkt")
(define (read-syntax source-path input-port)
  (define parse-tree (parse source-path (make-tokenizer input-port)))
  (strip-bindings
   (with-syntax
       ([_PARSE-TREE parse-tree])
     #'(module at-mod cclp/at-expander
         _PARSE-TREE))))
(provide read-syntax)