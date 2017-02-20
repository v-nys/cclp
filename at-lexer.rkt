#lang br

(require brag/support
         syntax/strip-context)

(define-lex-abbrev digits (:+ (char-set "0123456789"))) ; numeric includes non-latin scripts,...

(define parameterized-variable-lexer
  (lexer-srcloc
   [(eof) (begin (set! lexer-state 'unparameterized) (return-without-srcloc eof))]
   [(:or "a" "g" "<" "," "i" "i+1" "L") (token lexeme lexeme)]
   [">" (begin (set! lexer-state 'unparameterized) (token lexeme lexeme))]
   [(:+ digits) (token 'NUMBER (string->number lexeme))]))

(define (unget port num)
  (file-position port (- (file-position port) num)))
(define at-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
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
(define (top-lexer input-port)
  (let* ([peeked (peek-string 2 0 input-port)] ; HACK!
         [peeked-param (and (not (eof-object? peeked)) (regexp-match #rx"^g<|a<" peeked))])
    (if (or peeked-param (eq? lexer-state 'parameterized))
        (begin (set! lexer-state 'parameterized) (parameterized-variable-lexer input-port))
        (at-lexer input-port))))

(module+ test
  (require rackunit)
  (define (lex str) (apply-lexer top-lexer (open-input-string str))) ; use a port so we can unget
  (define (lex-param str) (apply-lexer parameterized-variable-lexer (open-input-string str)))
  (check-equal?
   (lex "g 1")
   (list
    (token 'SYMBOL "g")
    (token " " #:skip? #t)
    (token 'NUMBER 1)))
  (check-equal?
   (lex "g1")
   (list (token 'AVAR-G 1)))
  (check-equal?
   (lex "multiabc")
   (list (token 'SYMBOL "multiabc")))
  (check-equal?
   (lex "multi()")
   (list
    (token "multi" "multi")
    (token "(" "(")
    (token ")" ")")))
  (check-equal?
   (lex "hello % world\ntest")
   (list
    (token 'SYMBOL "hello")
    (token " " #:skip? #t)
    (token 'COMMENT "% world\n" #:skip? #t)
    (token 'SYMBOL "test")))
  (check-equal?
   (lex-param "g<1,i+1,3>")
   (list
    (token "g" "g")
    (token "<" "<")
    (token 'NUMBER 1)
    (token "," ",")
    (token "i+1" "i+1")
    (token "," ",")
    (token 'NUMBER 3)
    (token ">" ">")))
  (check-equal?
   (lex "g<1,i+1,3>")
   (lex-param "g<1,i+1,3>"))
  (check-equal?
   (lex "multi((abc(a<1,i,1>")
   (list
    (token "multi" "multi")
    (token "(" "(")
    (token "(" "(")
    (token 'SYMBOL "abc")
    (token "(" "(")
    (token "a" "a")
    (token "<" "<")
    (token 'NUMBER 1)
    (token "," ",")
    (token "i" "i")
    (token "," ",")
    (token 'NUMBER 1)
    (token ">" ">"))))
(provide top-lexer)