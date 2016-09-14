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
       ["PROGRAM:" (token 'PROGRAM-DELIMITER lexeme)]
       ["FULL EVALUATION:" (token 'FULL-EVALUATION-DELIMITER lexeme)]
       ["PREPRIOR:" (token 'PREPRIOR-DELIMITER lexeme)]
       ["α" (token 'AVAR-SYMBOL-A lexeme)] ; to avoid conflict with potential constants a and g
       ["γ" (token 'AVAR-SYMBOL-G lexeme)] ; same
       [(re-seq (char-range "A" "Z") (re-* (re-or (re-or (re-or (char-range "a" "z") (char-range "A" "Z")) numeric) "_"))) (token 'VARIABLE-IDENTIFIER lexeme)]
       [(re-seq (char-range "a" "z") (re-* (re-or (re-or (re-or (char-range "a" "z") (char-range "A" "Z")) numeric) "_"))) (token 'SYMBOL lexeme)]
       [(re-seq numeric (re-* numeric)) (token 'NUMBER (string->number lexeme))]
       [(re-seq "%" (re-* (char-complement "\n"))) (token 'COMMENT lexeme #:skip? #t)]
       ["->" (token 'LEADS-TO lexeme)]
       ["/" (token 'SLASH lexeme)]
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

(require "cclp-parser.rkt")
(define (read-syntax source-path input-port)
  (define parse-tree (parse source-path (tokenize input-port)))
  (strip-context
    (with-syntax ([_PARSE-TREE parse-tree])
                  #'(module lp-mod "cclp-expander.rkt"
                      _PARSE-TREE))))
(provide read-syntax)