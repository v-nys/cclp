#lang racket
(require
  (for-syntax "../src/cclp-parser.rkt")
  (for-syntax (only-in "../src/cclp-reader.rkt" all-tokens))
  (for-syntax (only-in syntax/strip-context replace-context))
  (for-syntax "../src/cclp-expander.rkt")
  "../src/abstract-substitution.rkt"
  "../src/cclp-expander.rkt")
(require (prefix-in ad: "../src/abstract-multi-domain.rkt"))
(require syntax/parse)
(require "../src/cclp-interpreter.rkt")

; should have a "baseline" prior relation model object
; interpreting a model is then just a matter of interpreting rules and adding them to the model's list of rules!
(define-syntax (parse-prior-relation stx)
  (define prior-section-parse (make-rule-parser preprior-section))
  (syntax-case stx ()
    [(_ THE-SECTION)
     (with-syntax
         ([PARSE-TREE
           (replace-context
            #'() (prior-section-parse (all-tokens (syntax->datum #'THE-SECTION))))])
       #'PARSE-TREE)]))
(provide parse-prior-relation)