#lang racket
(require
  (for-syntax "../src/fullai-parser.rkt")
  (for-syntax "../src/fullai-reader.rkt") ; for all-tokens
  (for-syntax syntax/strip-context) ; for replace-context
  (for-syntax "../src/fullai-expander.rkt")
  "../src/fullai-expander.rkt"
  )

; think the current error is a side-effect of breaking hygiene
; I am writing abstract-atom-with-args when the expander is in scope
(define-syntax (parse-atom stx)
  (define atom-parse (make-rule-parser abstract-atom-with-args))
  (syntax-case stx () [(_ THE-ATOM)
                       (with-syntax ([PARSE-TREE (replace-context #'() (atom-parse (all-tokens (syntax->datum #'THE-ATOM))))])
                         #'PARSE-TREE)]))
(provide parse-atom)

(define-syntax (parse-term stx)
  (define term-parse (make-rule-parser abstract-term))
  (syntax-case stx () [(_ THE-TERM)
                       (with-syntax ([PARSE-TREE (replace-context #'() (term-parse (all-tokens (syntax->datum #'THE-TERM))))])
                         #'PARSE-TREE)]))
(provide parse-term)