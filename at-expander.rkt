#lang br
(require (for-syntax syntax/parse))

(define-syntax (at-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE ...)
     (syntax/loc stx (#%module-begin
        (define at _PARSE-TREE ...)))]))
(provide (rename-out [at-module-begin #%module-begin]) #%top-interaction)

(define-syntax (top stx)
  (syntax-parse stx
    [_ #'3]))
(provide top)