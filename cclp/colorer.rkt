#lang br
(require brag/support syntax-color/racket-lexer)

(define cclp-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [(:or "{PROGRAM}" "{FULL EVALUATION}" "{QUERY}" "{PARTIAL ORDER}" "{K}")
    (values lexeme 'hash-colon-keyword #f (pos lexeme-start) (pos lexeme-end))]
   [any-char (values lexeme 'no-color #f (pos lexeme-start) (pos lexeme-end))]))

(define (color-cclp port offset coloring-mode)
  (define-values (str cat paren start end)
    (cclp-lexer port))
  (values str cat paren start end 0 #f))
(provide color-cclp)