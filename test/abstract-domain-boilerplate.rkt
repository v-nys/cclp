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

(define-syntax (term-equality-list stx)
  (syntax-case stx ()
    [(_) #'(list)]
    [(_ (t1 t2) rest ...)
     #'(cons (abstract-equality (interpret-abstract-term t1) (interpret-abstract-term t2))
             (term-equality-list rest ...))]))
(provide term-equality-list)