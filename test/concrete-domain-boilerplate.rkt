#lang racket
(require
  (for-syntax "../src/cclp-parser.rkt")
  (for-syntax "../src/cclp-reader.rkt") ; for all-tokens
  (for-syntax syntax/strip-context) ; for replace-context
  (for-syntax "../src/cclp-expander.rkt")
  "../src/cclp-expander.rkt")

(define-syntax (parse-rule stx)
  (define rule-parse (make-rule-parser rule))
  (syntax-case stx () [(_ THE-RULE)
                       (with-syntax ([PARSE-TREE (replace-context #'() (rule-parse (all-tokens (syntax->datum #'THE-RULE))))])
                         #'PARSE-TREE)]))
(provide parse-rule)
