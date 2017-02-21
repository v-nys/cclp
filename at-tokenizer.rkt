#lang br

(require
  brag/support
  "at-lexer.rkt")
(define (make-tokenizer input-port [path #f])
  (port-count-lines! input-port)
  (lexer-file-path path)
  (define (next-token) (top-lexer input-port))
  next-token)
(provide make-tokenizer)