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
(define gg-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [(:+ whitespace) (token lexeme #:skip? #t)]
   [(:+ digits) (token 'NUMBER (string->number lexeme))]
   [(:-
     (:seq (char-range "a" "z") (:* (:or (char-range "a" "z") (char-range "A" "Z") digits  "_")))
     (:or (:seq "g" (:+ digits))
          (:seq "a" (:+ digits)))
     (:or (:seq "g" "<" (:+ digits) "," (:* whitespace) (:or "1" "i" "i+1" "L") "," (:* whitespace) (:+ digits) ">")
          (:seq "a" "<" (:+ digits) "," (:* whitespace) (:or "1" "i" "i+1" "L") "," (:* whitespace) (:+ digits) ">"))
     "multi")
    (token 'SYMBOL lexeme)]
   [(:or "(" ")" "[" "]" "|" "{" "}" "," "/" "." "->" "+" "-" ":" "*" "multi" "NODES" "EDGES") (token lexeme lexeme)]
   ["#t" (token 'BOOLEAN #t)]
   ["#f" (token 'BOOLEAN #f)]
   [(:seq "g" (:+ digits)) (token 'AVAR-G (string->number (substring lexeme 1)))]
   [(:seq "a" (:+ digits)) (token 'AVAR-A (string->number (substring lexeme 1)))]
   [(from/to "%" "\n") (token 'COMMENT lexeme #:skip? #t)]))

(define lexer-state 'unparameterized)
(define (top-lexer input-port)
  (let* ([peeked (peek-string 2 0 input-port)]
         [peeked-param (and (not (eof-object? peeked)) (regexp-match #rx"^g<|a<" peeked))])
    (if (or peeked-param (eq? lexer-state 'parameterized))
        (begin (set! lexer-state 'parameterized) (parameterized-variable-lexer input-port))
        (gg-lexer input-port))))
(provide top-lexer)