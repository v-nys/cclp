#lang br
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket-list-utils/utils odd-elems)))

(require (prefix-in ad: "abstract-multi-domain.rkt"))
(require "abstract-analysis.rkt")
(require racket-tree-utils/src/tree)

(define-syntax (at-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE ...)
     (syntax/loc stx (#%module-begin _PARSE-TREE ...))]))
(provide (rename-out [at-module-begin #%module-begin]) #%top-interaction)

(define-syntax (top stx)
  (syntax-parse stx
    [(_ WS-STX ... AT-STX)
     (syntax/loc stx AT-STX)]))
(provide top)

(define-syntax (at stx)
  (syntax-parse stx
    [(_ "(" LABEL-STACK-OPT-ORIGIN-STX WS-PREFIXED-SUBTREES-STX ")")
     (syntax/loc stx
       (node
        LABEL-STACK-OPT-ORIGIN-STX
        WS-PREFIXED-SUBTREES-STX))]))
(provide at)

(define-syntax (label-stack-opt-origin stx)
  (syntax-parse stx
    [(_ LABEL-STX _WS-STX-1 STACK-STX _WS-STX-2 OPT-ORIGIN-STX)
     (begin
       (syntax/loc stx
       (struct-copy tree-label LABEL-STX [preprior-stack STACK-STX])))]))
(provide label-stack-opt-origin)

(define-syntax (opt-origin stx)
  (syntax-parse stx
    [(_) (syntax/loc stx (cons (list) #f))]))
(provide opt-origin)

(define-syntax (at-label stx)
  (syntax-parse stx
    [(_ IDX-STX "." ACON-P-SEL-STX)
     (syntax/loc stx
       (tree-label
        (car ACON-P-SEL-STX)
        (cdr ACON-P-SEL-STX) (list) #f (quote IDX-STX) (list)))]
    [(_ ACON-P-SEL-STX)
     (syntax/loc stx
       (tree-label ACON-P-SEL-STX #f (list) #f #f (list)))]))
(provide at-label)

(define-syntax (acon-with-potential-selection stx)
  (syntax-parse stx
    [(_ NESTED-STX) (syntax/loc stx NESTED-STX)]))
(provide acon-with-potential-selection)

(define-syntax (acon-with-selection stx)
  (syntax-parse stx
    [(_ "*" SELECTED-ATOM-STX "*")
     (syntax/loc stx (cons (list SELECTED-ATOM-STX) 0))]))
(provide acon-with-selection)

(define-syntax (acon-without-selection stx)
  (syntax-parse stx
    [(_ "□")
     (syntax/loc stx (list))]
    [(_ NONEMPTY-ACON-WITHOUT-SELECTION-STX)
     (syntax/loc stx NONEMPTY-ACON-WITHOUT-SELECTION-STX)]))
(provide acon-without-selection)

(define-syntax (nonempty-acon-without-selection stx)
  (syntax-parse stx
    [(_ ATOM-STX) (syntax/loc stx (list ATOM-STX))]
    [(_ ATOM-STX "," OPT-WS-STX REST-STX ...)
     (syntax/loc stx
       (cons ATOM-STX (nonempty-acon-without-selection REST-STX ...)))]))
(provide nonempty-acon-without-selection)

(define-syntax (abstract-atom stx)
  (syntax-parse stx
    [(_ NESTED-STX)
     (syntax/loc stx NESTED-STX)]))
(provide abstract-atom)

(define-syntax-rule (abstract-atom-with-args symbol "(" arg ... ")")
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide abstract-atom-without-args)

;graph-stack : OPEN-RECTANGULAR-PAREN [graph (opt-ws AMPERSAND opt-ws graph)*] CLOSE-RECTANGULAR-PAREN
;graph : QUESTION-MARK | precedence (COMMA opt-ws precedence)*
;precedence : abstract-atom opt-ws GT opt-ws abstract-atom

(define-syntax (graph-stack stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (list))]
    [(_ "[" GRAPH-STX "]")
     ; TODO deal with this
     (syntax/loc stx (list))]
    [(_ "[" GRAPH-STX _OPT-WS-1-STX "&" _OPT-WS-2-STX REST-GRAPH-STX ... "]")
     ; TODO deal with this
     (syntax/loc stx (list))]))
(provide graph-stack)

(define-syntax (ws-prefixed-subtrees stx)
  (syntax-parse stx
    [(_) (syntax/loc stx (list))]
    [(_ WS-STX CHILD-STX REST-STX ...)
     (syntax/loc stx (cons CHILD-STX (ws-prefixed-subtrees REST-STX ...)))]))
(provide ws-prefixed-subtrees)