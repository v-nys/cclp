#lang br
(require (for-syntax syntax/parse))
(require "abstract-multi-domain.rkt")
(require "abstract-analysis.rkt")
(require racket-tree-utils/src/tree)

(define-syntax (at-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE ...)
     (syntax/loc stx (#%module-begin '_PARSE-TREE ...))]))
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

(define-syntax (label-stack-opt-origin stx)
  (syntax-parse stx
    ; TODO case with origin
    [(LABEL-STX _WS-STX-1 STACK-STX _WS-STX-2)
     (syntax/loc stx
       (struct-copy tree-label LABEL-STX [preprior-stack STACK-STX]))]))

(define-syntax (at-label stx)
  (syntax-parse stx
    [(_ NUM-STX "." ACON-P-SEL-STX)
     (syntax/loc stx
       (tree-label (car ACON-P-SEL-STX) (cdr ACON-P-SEL-STX) (list) #f NUM-STX (list)))]))

(define-syntax (acon-with-potential-selection stx)
  (syntax-parse stx
    [(_ NESTED-STX) (syntax/loc stx NESTED-STX)]))

(define-syntax (acon-with-selection stx)
  (syntax-parse stx
    [(_ "*" SELECTED-ATOM-STX "*")
     (syntax/loc stx SELECTED-ATOM-STX)]))

(define-syntax (abstract-atom stx)
  (syntax-parse stx
    [(_ NESTED-STX)
     (syntax/loc stx NESTED-STX)]))
(provide abstract-atom)



;graph-stack : OPEN-RECTANGULAR-PAREN [graph (opt-ws AMPERSAND opt-ws graph)*] CLOSE-RECTANGULAR-PAREN
;graph : QUESTION-MARK | precedence (COMMA opt-ws precedence)*
;precedence : abstract-atom opt-ws GT opt-ws abstract-atom

(define-syntax (graph-stack stx)
  (syntax-parse stx
    [("[" "]")
     (syntax/loc stx (list))]
    [("[" GRAPH-STX "]")
     ; TODO deal with this
     (syntax/loc stx (list))]
    [("[" GRAPH-STX _OPT-WS-1-STX "&" _OPT-WS-2-STX REST-GRAPH-STX ... "]")
     ; TODO deal with this
     (syntax/loc stx (list))]))

;(define-syntax (graph stx)
;  (syntax-parse stx
;    [("?") (syntax/loc stx (mk-preprior-model))]))

; TODO
(define-syntax (ws-prefixed-subtrees stx) (syntax/loc stx #'(list)))