; MIT License
;
; Copyright (c) 2016 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

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
       ["(" (token 'OPEN-PAREN lexeme)]
       [")" (token 'CLOSE-PAREN lexeme)]
       ["[" (token 'OPEN-LIST-PAREN lexeme)]
       ["]" (token 'CLOSE-LIST-PAREN lexeme)]
       ["|" (token 'LIST-SEPARATOR lexeme)]
       ["," (token 'COMMA lexeme)]
       [":-" (token 'IMPLIES lexeme)]
       ["." (token 'PERIOD lexeme)]
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
                  #'(module abstractlp-mod "abstractlp-expander.rkt"
                      _PARSE-TREE))))
(provide read-syntax)