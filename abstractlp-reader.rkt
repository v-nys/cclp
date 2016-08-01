#lang br
(require parser-tools/lex brag/support)
(require (prefix-in re- parser-tools/lex-sre))

(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [whitespace (token 'WS lexeme #:skip? #t)]
       [(re-seq upper-case (re-* (re-or alphabetic numeric))) (token 'VARIABLE-IDENTIFIER lexeme)]
       [(re-seq lower-case (re-* (re-or alphabetic numeric))) (token 'SYMBOL lexeme)]
       [(re-or "<" ">" "=<" ">=" "is") (token 'ARITHMETIC-OP lexeme)] ; only need this for now
       [(re-seq "%" (re-* (char-complement "\n"))) (token 'COMMENT lexeme #:skip? #t)]
       ["(" (token 'OPEN-PAREN)]
       [")" (token 'CLOSE-PAREN)]
       ["[" (token 'OPEN-LIST-PAREN)]
       ["]" (token 'CLOSE-LIST-PAREN)]
       ["|" (token 'LIST-SEPARATOR)]
       ["," (token 'COMMA)]
       ["←" (token 'IMPLIES)]
       ["." (token 'PERIOD)]
       [(eof) eof]))
    (get-token input-port))
  next-token)

; for easy testing
(define (all-tokens str)
  (define (exhaust t)
    (let ([next (t)])
      (if (equal? (position-token-token next) eof) (list next) (cons next (exhaust t)))))
  (let* ([string-port (open-input-string str)]
         [tokenizer (tokenize string-port)])
    (exhaust tokenizer)))
(provide all-tokens)

(require "abstractlp-parser.rkt")
(define (read-syntax source-path input-port)
  (define parse-tree (parse source-path (tokenize input-port)))
  (strip-context
    (inject-syntax ([#'_PARSE-TREE parse-tree])
                  #'(module bf-mod "abstractlp-expander.rkt"
                      _PARSE-TREE))))
(provide read-syntax)