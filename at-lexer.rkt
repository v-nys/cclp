#lang br

(require brag/support
         syntax/strip-context)

(define at-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [(:+ whitespace) (token lexeme #:skip? #t)]
   [(:+ numeric) (token 'NUMBER (string->number lexeme))]
   [(:seq
     (char-range "A" "Z")
     (:* (:or (char-range "a" "z") (char-range "A" "Z") numeric "_")))
    (token 'VARIABLE-IDENTIFIER lexeme)]
   [(:-
     (:seq (char-range "a" "z") (:* (:or (char-range "a" "z") (char-range "A" "Z") numeric  "_")))
     (:or (:seq "g" (:+ numeric))
          (:seq "a" (:+ numeric)))
     "multi")
    (token 'SYMBOL lexeme)]
   [(:or "#t" "#f" "(" ")" "[" "]" "|" "{" "}" "," "/" "." "*" "□" "->" "<" ">" ":-" "!CY" "!GEN" "multi") (token lexeme)]
   [(:seq "g" (:+ numeric)) (token 'AVAR-G (string->number (substring lexeme 1)))]
   [(:seq "a" (:+ numeric)) (token 'AVAR-A (string->number (substring lexeme 1)))]
   [(from/to "%" "\n") (token 'COMMENT lexeme #:skip? #t)]))

(module+ test
  (require rackunit)
  (define (lex str) (apply-lexer at-lexer str))
  (check-equal?
   (lex "g 1")
   (list
    (srcloc-token (token 'SYMBOL "g") (srcloc 'string #f #f 1 1))
    (srcloc-token (token " " #:skip? #t) (srcloc 'string #f #f 2 1))
    (srcloc-token (token 'NUMBER 1) (srcloc 'string #f #f 3 1))))
  (check-equal?
   (lex "g1")
   (list (srcloc-token (token 'AVAR "g1") (srcloc 'string #f #f 1 2))))
  (check-equal?
   (lex "multiabc")
   (list (srcloc-token (token 'SYMBOL "multiabc") (srcloc 'string #f #f 1 8))))
  (check-equal?
   (lex "multi()")
   (list
    (srcloc-token (token "multi") (srcloc 'string #f #f 1 5))
    (srcloc-token (token "(") (srcloc 'string #f #f 6 1))
    (srcloc-token (token ")") (srcloc 'string #f #f 7 1))))
  (check-equal?
   (lex "hello % world\ntest")
   (list
    (srcloc-token (token 'SYMBOL "hello") (srcloc 'string #f #f 1 5))
    (srcloc-token (token " " #:skip? #t) (srcloc 'string #f #f 6 1))
    (srcloc-token (token 'COMMENT "% world\n" #:skip? #t) (srcloc 'string #f #f 7 8))
    (srcloc-token (token 'SYMBOL "test") (srcloc 'string #f #f 15 4)))))
(provide at-lexer)