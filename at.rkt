#lang br

(require parser-tools/lex brag/support)
(require (prefix-in re- parser-tools/lex-sre))
(require syntax/strip-context)

(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [whitespace (token 'WS lexeme #:skip? #t)]
       ["(" (token 'OPEN-PAREN lexeme)]
       [")" (token 'CLOSE-PAREN lexeme)]
       ["[" (token 'OPEN-RECTANGULAR-PAREN lexeme)]
       ["]" (token 'CLOSE-RECTANGULAR-PAREN lexeme)]
       ["|" (token 'LIST-SEPARATOR lexeme)]
       ["{" (token 'OPEN-CURLY-PAREN lexeme)]
       ["}" (token 'CLOSE-CURLY-PAREN lexeme)]
       ["," (token 'COMMA lexeme)]
       [(re-seq numeric (re-* numeric)) (token 'NUMBER (string->number lexeme))]
       ["/" (token 'SLASH lexeme)]
       ["?" (token 'QUESTION-MARK lexeme)]
       ["&" (token 'AMPERSAND lexeme)]
       ["." (token 'PERIOD lexeme)]
       ["*" (token 'ASTERISK lexeme)]
       ["□" (token 'EMPTY-GOAL lexeme)]
       [(re-seq
         (char-range "A" "Z")
         (re-* (re-or (re-or (re-or (char-range "a" "z") (char-range "A" "Z")) numeric) "_")))
        (token 'VARIABLE-IDENTIFIER lexeme)]
       [; this makes an exception for g... and a...
        (re--
         (re-seq (char-range "a" "z") (re-* (re-or (re-or (re-or (char-range "a" "z") (char-range "A" "Z")) numeric) "_")))
         (re-or (re-seq "g" (re-+ numeric))
                (re-seq "a" (re-+ numeric)))) (token 'SYMBOL lexeme)]
       ["a" (token 'AVAR-SYMBOL-A lexeme)]
       ["g" (token 'AVAR-SYMBOL-G lexeme)]
       ["->" (token 'LEADS-TO lexeme)]
       [">" (token 'GT lexeme)]
       [":-" (token 'IMPLIES lexeme)]
       [(eof) eof]
       [(re-seq "%" (re-* (char-complement "\n"))) (token 'COMMENT lexeme #:skip? #t)]))
    (get-token input-port))
  next-token)
(provide tokenize)

(define (all-tokens str)
  (define (exhaust t)
    (let ([next (t)])
      (if (equal? (position-token-token next) eof)
          (list next)
          (cons next (exhaust t)))))
  (let* ([string-port (open-input-string str)]
         [tokenizer (tokenize string-port)])
    (exhaust tokenizer)))
(provide all-tokens)

(require "at-parser.rkt")
(define (read-syntax source-path input-port)
  (define parse-tree (parse source-path (tokenize input-port)))
  (strip-context
   (with-syntax
       ([_PARSE-TREE parse-tree])
     #'(module at-mod cclp/at-expander
         _PARSE-TREE))))
(provide read-syntax)