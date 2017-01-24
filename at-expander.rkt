#lang br
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket-list-utils/utils odd-elems)))

(require (prefix-in ad: "abstract-multi-domain.rkt"))
(require (prefix-in as: "abstract-substitution.rkt"))
(require (prefix-in cd: "concrete-domain.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require (prefix-in faid: "fullai-domain.rkt"))
(require "preprior-graph.rkt")
(require "abstract-analysis.rkt")
(require racket-tree-utils/src/tree)
(require (only-in graph add-vertex! add-edge!))

(define-syntax (at-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE)
     (syntax/loc stx (#%module-begin _PARSE-TREE))]))
(provide (rename-out [at-module-begin #%module-begin]) #%top-interaction)

(define-syntax (top stx)
  (syntax-parse stx
    [(_ WS-STX ... AT-STX)
     (syntax/loc stx AT-STX)]))
(provide top)

(define-syntax (at stx)
  (syntax-parse stx
    [(_ "(" LABEL-STACK-OPT-ORIGIN-STX ")")
     (syntax/loc stx
       (node
        LABEL-STACK-OPT-ORIGIN-STX
        (list)))]
    [(_ "(" LABEL-STACK-OPT-ORIGIN-STX _ SUBTREES-STX ")")
     (syntax/loc stx
       (node
        LABEL-STACK-OPT-ORIGIN-STX
        SUBTREES-STX))]))
(provide at)

(define-syntax (subtrees stx)
  (syntax-parse stx
    [(_ AT-STX) (syntax/loc stx (list AT-STX))]
    [(_ AT-STX _ REST-STX ...+)
     (syntax/loc stx (cons AT-STX (subtrees REST-STX ...)))]))
(provide subtrees)

(define-syntax (label-stack-opt-origin stx)
  (syntax-parse stx
    [(_ LABEL-STX _ STACK-STX)
     (syntax/loc stx
       (struct-copy tree-label LABEL-STX [preprior-stack STACK-STX]))]
    [(_ LABEL-STX _ STACK-STX _ SUBST-STX _ KNOWLEDGE-STX)
     (syntax/loc stx
       (struct-copy
        tree-label
        LABEL-STX
        [preprior-stack STACK-STX]
        [substitution SUBST-STX]
        [rule KNOWLEDGE-STX]))]))
(provide label-stack-opt-origin)

; TODO double-check these!

(define-syntax (at-label stx)
  (syntax-parse stx
    [(_ IDX-STX "." ACON-P-SEL-STX)
     (syntax/loc stx
       (tree-label
        (car ACON-P-SEL-STX)
        (cdr ACON-P-SEL-STX) (list) #f (quote IDX-STX) (list)))]
    [(_ ACON-P-SEL-STX) ; no index -> no selection
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
     (syntax/loc stx (cons (list SELECTED-ATOM-STX) 0))]
    [(_ PRECEDING-CONJUNCTION "," "*" SELECTED-ATOM-STX "*")
     (syntax/loc stx
       (cons (append PRECEDING-CONJUNCTION (list SELECTED-ATOM-STX))
             (length PRECEDING-CONJUNCTION)))]
    [(_ PRECEDING-CONJUNCTION "," "*" SELECTED-ATOM-STX "*" "," REMAINING-CONJUNCTION)
     (syntax/loc stx
       (cons (append PRECEDING-CONJUNCTION (cons SELECTED-ATOM-STX REMAINING-CONJUNCTION))
             (length PRECEDING-CONJUNCTION)))]
    [(_ "*" SELECTED-ATOM-STX "*" "," REMAINING-CONJUNCTION)
     (syntax/loc stx
       (cons (cons SELECTED-ATOM-STX REMAINING-CONJUNCTION) 0))]))
(provide acon-with-selection)

(define-syntax (abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (ad:abstract-function 'nil '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (ad:abstract-function 'cons (list term0 (ad:abstract-function 'nil '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (ad:abstract-function 'cons (list term0 (abstract-lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest "]")
     (syntax/loc stx (ad:abstract-function 'cons (list term0 rest)))]))
(provide abstract-lplist)

(define-syntax (acon-without-selection stx)
  (syntax-parse stx
    [(_ "□")
     (syntax/loc stx (list))]
    [(_ NONEMPTY-ACON-WITHOUT-SELECTION-STX)
     (syntax/loc stx NONEMPTY-ACON-WITHOUT-SELECTION-STX)]))
(provide acon-without-selection)

(define-syntax (nonempty-acon-without-selection stx)
  (syntax-parse stx
    [(_) (syntax/loc stx (list))]
    [(_ ATOM-STX) (syntax/loc stx (list ATOM-STX))]
    [(_ ATOM-STX "," REST-STX ...)
     (syntax/loc stx
       (cons ATOM-STX (nonempty-acon-without-selection REST-STX ...)))]))
(provide nonempty-acon-without-selection)

(define-syntax (abstract-atom stx)
  (syntax-parse stx
    [(_ NESTED-STX)
     (syntax/loc stx NESTED-STX)]))
(provide abstract-atom)

;(define-syntax-rule (abstract-atom-with-args symbol "(" arg ... ")")
;  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(define-syntax (abstract-atom-with-args stx)
  (syntax-parse stx
    [(_ SYM-STX "(" ARG ")")
     (syntax/loc stx
       (ad:abstract-atom
        (string->symbol (quote SYM-STX))
        (list ARG)))]
    [(_ SYM-STX "(" ARG "," COMMA-OR-ARG ... ")")
     (syntax/loc stx
       (ad:abstract-atom
        (string->symbol (quote SYM-STX))
        (cons ARG (ad:abstract-atom-args (abstract-atom-with-args SYM-STX "(" COMMA-OR-ARG ... ")")))))]))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide abstract-atom-without-args)

(define-syntax (substitution stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx (list))]
    [(_ "{" PAIR-STX "}") (syntax/loc stx (list PAIR-STX))]
    [(_ "{" PAIR-STX "," COMMA-OR-PAIR-STX ... "}")
     (syntax/loc stx
       (cons PAIR-STX (substitution "{" COMMA-OR-PAIR-STX ... "}")))]))
(provide substitution)

(define-syntax-rule (substitution-pair lhs "/" rhs)
  (as:abstract-equality lhs rhs))
(provide substitution-pair)

(define-syntax (knowledge stx)
  (syntax-parse stx
    [(_ NESTED-STX ".")
     (syntax/loc stx NESTED-STX)]))
(provide knowledge)

(define-syntax (rule stx)
  (syntax-parse stx
    [(_ ATOM-STX)
     (syntax/loc stx (ck:rule ATOM-STX (list)))]
    [(_ ATOM-STX _ ":-" _ CON-STX)
     (syntax/loc stx (ck:rule ATOM-STX CON-STX))]))
(provide rule)

(define-syntax (atom stx)
  (syntax-parse stx
    [(_ SYMBOL-STX)
     (syntax/loc stx
       (cd:atom (string->symbol (quote SYMBOL-STX)) (list)))]
    [(_ SYMBOL-STX "(" ARG ")")
     (syntax/loc stx
       (cd:atom (string->symbol (quote SYMBOL-STX)) (list ARG)))]
    [(_ SYMBOL-STX "(" ARG "," ARG-OR-COMMA ... ")")
     (syntax/loc stx
       (cd:atom
        (string->symbol (quote SYMBOL-STX))
        (cons ARG (cd:atom-args (atom SYMBOL-STX "(" ARG-OR-COMMA ... ")")))))]))
(provide atom)

(define-syntax (term stx)
  (syntax-parse stx
    [(_ VAR-OR-LIST-OR-MISC-FUNCTION)
     (syntax/loc stx VAR-OR-LIST-OR-MISC-FUNCTION)]))
(provide term)

(define-syntax-rule (variable VARIABLE-NAME) (cd:variable (string->symbol (quote VARIABLE-NAME))))
(provide variable)

(define-syntax (function-term stx)
  (syntax-parse stx
    [(_ symbol:str)
     (syntax/loc stx (cd:function (string->symbol (quote symbol)) '()))]
    [(_ num-term) (syntax/loc stx num-term)] ; these are just plain numbers
    [(_ symbol "(" arg ... ")")
     (syntax/loc stx (cd:function (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))]))
(provide function-term)

(define-syntax (lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (cd:function 'nil '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (cd:function 'cons (list term0 (cd:function 'nil '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (cd:function 'cons (list term0 (lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest ... "]")
     (syntax/loc stx (cd:function 'cons (list term0 rest ...)))]))
(provide lplist)

(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_ CONJUNCT-STX) (syntax/loc stx (list CONJUNCT-STX))]
    [(_ CONJUNCT-STX "," COMMA-OR-CONJUNCT-STX ...)
     (syntax/loc stx (cons CONJUNCT-STX (conjunction COMMA-OR-CONJUNCT-STX ...)))]))
(provide conjunction)

(define-syntax-rule (number-term TERM)
  (cd:function (quote TERM) '()))
(provide number-term)

(define-syntax-rule (abstract-term specific-term) specific-term)
(provide abstract-term)

(define-syntax (abstract-variable stx)
  (syntax-parse stx
    [(_ "a" NUM-STX)
     (syntax/loc stx (ad:a (quote NUM-STX)))]
    [(_ "g" NUM-STX)
     (syntax/loc stx (ad:g (quote NUM-STX)))]))
(provide abstract-variable)

(define-syntax (abstract-function-term stx)
  (syntax-parse stx
    [(_ symbol:str) (syntax/loc stx (ad:abstract-function (string->symbol (quote symbol)) '()))]
    [(_ num-term) (syntax/loc stx num-term)]
    [(_ symbol "(" arg ... ")")
     (syntax/loc stx (ad:abstract-function (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))]))
(provide abstract-function-term)

(define-syntax-rule (abstract-number NUMBER)
  (ad:abstract-function (quote NUMBER) '()))
(provide abstract-number)

(define-syntax-rule (abstract-number-term TERM) TERM)
(provide abstract-number-term)

(define-syntax (fullai-rule stx)
  (syntax-parse stx
    [(_ NESTED-STX) (syntax/loc stx NESTED-STX)]))
(provide fullai-rule)

(define-syntax (fullai-rule-with-body stx)
  (syntax-parse stx
    [(_ AATOM-STX _ "->" _ SUBST-STX)
     (syntax/loc stx
       (full-ai-rule->full-evaluation
        (faid:full-ai-rule AATOM-STX SUBST-STX)))]))
(provide fullai-rule-with-body)

(define-syntax (graph-stack stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (list))]
    [(_ "[" GRAPH-STX "]")
     (syntax/loc stx (list GRAPH-STX))]
    [(_ "[" GRAPH-STX _ "&" _ REST-GRAPH-STX ... "]")
     (syntax/loc stx (cons GRAPH-STX (graph-stack "[" REST-GRAPH-STX ... "]")))]))
(provide graph-stack)

(define-syntax (graph stx)
  (syntax-parse stx
    [(_) (syntax/loc stx (mk-preprior-graph))]
    [(_ "?") (syntax/loc stx (mk-preprior-graph))]
    [(_ PRECEDENCE-STX) (syntax/loc stx (PRECEDENCE-STX (mk-preprior-graph)))]
    [(_ PRECEDENCE-STX "," _ REST-STX ...) (syntax/loc stx (PRECEDENCE-STX (graph REST-STX ...)))]))
(provide graph)

(define-syntax (precedence stx)
  (syntax-parse stx
    [(_ AATOM-STX-1 _ ">" _ AATOM-STX-2)
     (syntax/loc stx (λ (g) (begin (add-vertex! g AATOM-STX-1) (add-vertex! g AATOM-STX-2) (add-edge! g AATOM-STX-1 AATOM-STX-2) g)))]))
(provide precedence)