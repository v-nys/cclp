#lang br

(require
  brag/support
  "at-lexer.rkt")

(define (make-tokenizer input-port)
  (port-count-lines! input-port)
  (define (next-token) (at-lexer input-port))
  next-token)
(provide make-tokenizer)