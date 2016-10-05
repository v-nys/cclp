#lang racket
(require
  (for-syntax "../src/cclp-parser.rkt")
  (for-syntax (only-in "../src/cclp-reader.rkt" all-tokens))
  (for-syntax (only-in syntax/strip-context replace-context))
  (for-syntax "../src/cclp-expander.rkt")
  "../src/cclp-expander.rkt")

(define-syntax (parse-abstract-atom stx)
  (define atom-parse (make-rule-parser abstract-atom))
  (syntax-case stx ()
    [(_ THE-ATOM)
     (with-syntax
         ([PARSE-TREE
           (replace-context #'() (atom-parse (all-tokens (syntax->datum #'THE-ATOM))))])
       #'PARSE-TREE)]))
(provide parse-abstract-atom)

(define-syntax (parse-abstract-term stx)
  (define term-parse (make-rule-parser abstract-term))
  (syntax-case stx ()
    [(_ THE-TERM)
     (with-syntax
         ([PARSE-TREE
           (replace-context #'() (term-parse (all-tokens (syntax->datum #'THE-TERM))))])
       #'PARSE-TREE)]))
(provide parse-abstract-term)

(define-syntax (parse-abstract-substitution stx)
  (define substitution-parse (make-rule-parser abstract-substitution))
  (syntax-case stx ()
    [(_ THE-SUBSTITUTION)
     (with-syntax
         ([PARSE-TREE
           (replace-context
            #'()
            (substitution-parse (all-tokens (syntax->datum #'THE-SUBSTITUTION))))])
       #'PARSE-TREE)]))
(provide parse-abstract-substitution)

(define-syntax (parse-abstract-conjunction stx)
  (define conjunction-parse (make-rule-parser abstract-conjunction))
  (syntax-case stx ()
    [(_ THE-CONJUNCTION)
     (with-syntax
         ([PARSE-TREE
           (replace-context
            #'()
            (conjunction-parse (all-tokens (syntax->datum #'THE-CONJUNCTION))))])
       #'PARSE-TREE)]))
(provide parse-abstract-conjunction)

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

(parse-prior-relation "ord,perm")