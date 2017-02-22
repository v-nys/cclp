#lang br/quicklang
(require (for-syntax syntax/strip-context))
(require graph)
(require "generational-graph.rkt")

(define-macro (gg-module-begin (gg (nodes-section NODE-LINE ...) (edges-section EDGE-LINE ...)))
  (with-syntax ([REPLACED (replace-context caller-stx #'val)])
    (syntax/loc caller-stx
      (#%module-begin
       (nodes-section NODE-LINE ...)
       (define REPLACED (edges-section EDGE-LINE ...))
       (provide REPLACED)))))
(provide (rename-out [gg-module-begin #%module-begin]) #%top-interaction)

(define-macro (nodes-section NODE-LINE ...) #'(begin NODE-LINE ...))
(provide nodes-section)

(define-macro (node-line NUMBER CONJUNCT)
  (with-pattern ([NODE-NUM (prefix-id "node-" #'NUMBER)])
    #'(define NODE-NUM (identified-abstract-conjunct CONJUNCT NUMBER))))
(provide node-line)

(require (only-in "gg-expander.rkt" edges-section edge-line))
(provide edges-section edge-line)

(require (only-in "at-expander.rkt" abstract-atom abstract-function abstract-g-variable abstract-a-variable))
(provide abstract-atom abstract-function abstract-g-variable abstract-a-variable)
(require (only-in "at-expander.rkt" multi-abstraction parameterized-abstract-conjunction
                  parameterized-abstract-atom parameterized-abstract-function
                  parameterized-abstract-a-variable parameterized-abstract-g-variable
                  parameterized-abstract-list init init-pair consecutive consecutive-pair
                  final final-pair))
(provide multi-abstraction parameterized-abstract-conjunction
         parameterized-abstract-atom parameterized-abstract-function
         parameterized-abstract-a-variable parameterized-abstract-g-variable
         parameterized-abstract-list init init-pair consecutive consecutive-pair
         final final-pair)