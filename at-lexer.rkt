#lang br

(require brag/support
         syntax/strip-context)

(define-lex-abbrev digits (:+ (char-set "0123456789"))) ; numeric includes non-latin scripts,...

(define parameterized-variable-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [(:or "a" "g" "<" "," "i" "i+1" "L" ">") (token lexeme lexeme)]
   [(:+ digits) (token 'NUMBER (string->number lexeme))]))

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
   [(:or (:seq "g" "<" (:+ digits) "," (:* whitespace) (:or "1" "i" "i+1" "L") "," (:* whitespace) (:+ digits) ">")
         (:seq "a" "<" (:+ digits) "," (:* whitespace) (:or "1" "i" "i+1" "L") "," (:* whitespace) (:+ digits) ">"))
    ; this is wrong
    ; it also loses source location
    ; should rewind characters, apply parameterized variable lexer to port, not lexeme
    ; question is: how do I switch back and forth?
    ; at-lexer returns a lexer -> can I make it return a different one next time?
    ; e.g. set a flag and return void if the regex matches
    (return-without-srcloc (parameterized-variable-lexer (open-input-string lexeme)))]
   [(from/to "%" "\n") (token 'COMMENT lexeme #:skip? #t)]))

(module+ test
  (require rackunit)
  (define (lex str) (apply-lexer at-lexer (open-input-string str))) ; use a port so we can unget
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
   (lex-param "g<1,i+1,3>")))
(provide at-lexer)