#lang br
(require brag/support
         (prefix-in re- br-parser-tools/lex-sre))

(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [whitespace (token 'WS lexeme #:skip? #t)]
       ["{PROGRAM}" (token 'PROGRAM-DELIMITER lexeme)]
       ["{FULL EVALUATION}" (token 'FULL-EVALUATION-DELIMITER lexeme)]
       ["{PARTIAL ORDER}" (token 'PARTIAL-ORDER-DELIMITER lexeme)]
       ["{K}" (token 'K-DELIMITER lexeme)]
       ["{QUERY}" (token 'QUERY-DELIMITER lexeme)]
       [(re-seq (char-range "A" "Z") (re-* (re-or (re-or (re-or (char-range "a" "z") (char-range "A" "Z")) numeric) "_"))) (token 'VARIABLE-IDENTIFIER lexeme)]
       [(re--
         (re-seq (char-range "a" "z") (re-* (re-or (re-or (re-or (char-range "a" "z") (char-range "A" "Z")) numeric) "_")))
         (re-seq "a" (re-* numeric))
         (re-seq "g" (re-* numeric)))
        (token 'SYMBOL lexeme)]
       ["a" (token 'AMB-AVAR-SYMBOL-A lexeme)]
       ["g" (token 'AMB-AVAR-SYMBOL-G lexeme)]
       [(re-seq numeric (re-* numeric)) (token 'NUMBER (string->number lexeme))]
       ["->" (token 'LEADS-TO lexeme)]
       ["/" (token 'SLASH lexeme)]
       ["(" (token 'OPEN-PAREN lexeme)]
       [")" (token 'CLOSE-PAREN lexeme)]
       ["[" (token 'OPEN-LIST-PAREN lexeme)]
       ["]" (token 'CLOSE-LIST-PAREN lexeme)]
       ["|" (token 'LIST-SEPARATOR lexeme)]
       ["," (token 'COMMA lexeme)]
       [":-" (token 'IMPLIES lexeme)]
       ["i+1" (token 'IPLUSONE lexeme)] ; may seem odd, but plus is never used else
       ["{" (token 'CURLY-OPEN lexeme)]
       ["}" (token 'CURLY-CLOSE lexeme)]
       ["<" (token 'LT lexeme)]
       [">" (token 'GT lexeme)]
       ["=" (token 'EQ lexeme)]
       ["." (token 'PERIOD lexeme)]
       [(eof) eof]))
    (get-token input-port))
  next-token)
(provide tokenize)

(define (all-tokens str)
  (define (exhaust t)
    (let ([next (t)])
      (if (equal? (position-token-token next) eof) (list next) (cons next (exhaust t)))))
  (let* ([string-port (open-input-string str)]
         [tokenizer (tokenize string-port)])
    (exhaust tokenizer)))
(provide all-tokens)