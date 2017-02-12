#lang br

(require brag/support
         syntax/strip-context)

(define-lex-abbrev digits (:+ (char-set "0123456789"))) ; numeric includes non-latin scripts,...

(define parameterized-variable-lexer
  (lexer
   [(eof) (begin (set! lexer-state 'unparameterized) eof)]
   [(:or "a" "g" "<" "," "i" "i+1" "L") (token lexeme lexeme)]
   [">" (begin (set! lexer-state 'unparameterized) (token lexeme lexeme))]
   [(:+ digits) (token 'NUMBER (string->number lexeme))]))

(define (unget port num)
  (file-position port (- (file-position port) num)))
(define at-lexer
  (lexer
   [(eof) eof]
   [(:+ whitespace) (token lexeme #:skip? #t)]
   [(:+ digits) (token 'NUMBER (string->number lexeme))]
   [(:seq
     (char-range "A" "Z")
     (:* (:or (char-range "a" "z") (char-range "A" "Z") digits "_")))
    (token 'VARIABLE-IDENTIFIER lexeme)]
   [(:-
     (:seq (char-range "a" "z") (:* (:or (char-range "a" "z") (char-range "A" "Z") digits  "_")))
     (:or (:seq "g" (:+ digits))
          (:seq "a" (:+ digits)))
     (:or (:seq "g" "<" (:+ digits) "," (:* whitespace) (:or "1" "i" "i+1" "L") "," (:* whitespace) (:+ digits) ">")
          (:seq "a" "<" (:+ digits) "," (:* whitespace) (:or "1" "i" "i+1" "L") "," (:* whitespace) (:+ digits) ">"))
     "multi")
    (token 'SYMBOL lexeme)]
   [(:or "(" ")" "[" "]" "|" "{" "}" "," "/" "." "*" "□" "->" "<" ">" ":-" "!CY" "!GEN" "multi") (token lexeme lexeme)]
   ["#t" (token 'BOOLEAN #t)]
   ["#f" (token 'BOOLEAN #f)]
   [(:seq "g" (:+ digits)) (token 'AVAR-G (string->number (substring lexeme 1)))]
   [(:seq "a" (:+ digits)) (token 'AVAR-A (string->number (substring lexeme 1)))]
   [(from/to "%" "\n") (token 'COMMENT lexeme #:skip? #t)]))

(define lexer-state 'unparameterized)
(define top-lexer
  (let ([unparameterized-lexer at-lexer]
        [parameterized-lexer parameterized-variable-lexer])
    (λ (input-port)
      (begin
        (let* ([peeked (peek-string 2 0 input-port)] ; HACK!
               [peeked-param (and (not (eof-object? peeked)) (regexp-match #rx"^g<|a<" peeked))])
        (if (or peeked-param (eq? lexer-state 'parameterized))
            (begin (set! lexer-state 'parameterized) (parameterized-lexer input-port))
            (unparameterized-lexer input-port)))))))

(module+ test
  (require rackunit)
  (define (lex str) (apply-lexer top-lexer (open-input-string str))) ; use a port so we can unget
  (define (lex-param str) (apply-lexer parameterized-variable-lexer (open-input-string str)))
  (check-equal?
   (lex "g 1")
   (list
    (srcloc-token (token 'SYMBOL "g") (srcloc 'string #f #f 1 1))
    (srcloc-token (token " " #:skip? #t) (srcloc 'string #f #f 2 1))
    (srcloc-token (token 'NUMBER 1) (srcloc 'string #f #f 3 1))))
  (check-equal?
   (lex "g1")
   (list (srcloc-token (token 'AVAR-G 1) (srcloc 'string #f #f 1 2))))
  (check-equal?
   (lex "multiabc")
   (list (srcloc-token (token 'SYMBOL "multiabc") (srcloc 'string #f #f 1 8))))
  (check-equal?
   (lex "multi()")
   (list
    (srcloc-token (token "multi" "multi") (srcloc 'string #f #f 1 5))
    (srcloc-token (token "(" "(") (srcloc 'string #f #f 6 1))
    (srcloc-token (token ")" ")") (srcloc 'string #f #f 7 1))))
  (check-equal?
   (lex "hello % world\ntest")
   (list
    (srcloc-token (token 'SYMBOL "hello") (srcloc 'string #f #f 1 5))
    (srcloc-token (token " " #:skip? #t) (srcloc 'string #f #f 6 1))
    (srcloc-token (token 'COMMENT "% world\n" #:skip? #t) (srcloc 'string #f #f 7 8))
    (srcloc-token (token 'SYMBOL "test") (srcloc 'string #f #f 15 4))))
  (check-equal?
   (lex-param "g<1,i+1,3>")
   (list
    (srcloc-token (token "g" "g") (srcloc 'string #f #f 1 1))
    (srcloc-token (token "<" "<") (srcloc 'string #f #f 2 1))
    (srcloc-token (token 'NUMBER 1) (srcloc 'string #f #f 3 1))
    (srcloc-token (token "," ",") (srcloc 'string #f #f 4 1))
    (srcloc-token (token "i+1" "i+1") (srcloc 'string #f #f 5 3))
    (srcloc-token (token "," ",") (srcloc 'string #f #f 8 1))
    (srcloc-token (token 'NUMBER 3) (srcloc 'string #f #f 9 1))
    (srcloc-token (token ">" ">") (srcloc 'string #f #f 10 1))))
  (check-equal?
   (lex "g<1,i+1,3>")
   (list
    (void)
    (srcloc-token (token "g" "g") (srcloc 'string #f #f 1 1))
    (srcloc-token (token "<" "<") (srcloc 'string #f #f 2 1))
    (srcloc-token (token 'NUMBER 1) (srcloc 'string #f #f 3 1))
    (srcloc-token (token "," ",") (srcloc 'string #f #f 4 1))
    (srcloc-token (token "i+1" "i+1") (srcloc 'string #f #f 5 3))
    (srcloc-token (token "," ",") (srcloc 'string #f #f 8 1))
    (srcloc-token (token 'NUMBER 3) (srcloc 'string #f #f 9 1))
    (srcloc-token (token ">" ">") (srcloc 'string #f #f 10 1)))))
(provide at-lexer)