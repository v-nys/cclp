#lang racket
(require
  (for-syntax "../src/lp-parser.rkt")
  (for-syntax "../src/lp-reader.rkt") ; for all-tokens
  (for-syntax syntax/strip-context) ; for replace-context
  (for-syntax "../src/lp-expander.rkt")
  "../src/lp-expander.rkt")

(define-syntax (parse-rule stx)
  (define rule-parse (make-rule-parser rule))
  (syntax-case stx () [(_ THE-RULE)
                       (with-syntax ([PARSE-TREE (replace-context #'() (rule-parse (all-tokens (syntax->datum #'THE-RULE))))])
                         #'PARSE-TREE)]))
(provide parse-rule)