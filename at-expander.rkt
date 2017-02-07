#lang br
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket-list-utils/utils odd-elems)))
(require (only-in sugar/coerce ->symbol))

(require (prefix-in ad: "abstract-multi-domain.rkt"))
(require (prefix-in as: "abstract-substitution.rkt"))
(require (prefix-in cd: "concrete-domain.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require (prefix-in faid: "fullai-domain.rkt"))
(require "preprior-graph.rkt")
(require "abstract-analysis.rkt")
(require racket-tree-utils/src/tree)
(require (for-syntax syntax/strip-context))
(require "data-utils.rkt")
(require (only-in "syntax-utils.rkt" odd-elems-as-list))

(define-syntax (at-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE)
     (with-syntax ([REPLACED (replace-context stx #'val)])
       (syntax/loc stx
         (#%module-begin
          (define REPLACED _PARSE-TREE)
          (provide REPLACED))))]))
(provide (rename-out [at-module-begin #%module-begin]) #%top-interaction)

(define-syntax (top stx)
  (syntax-parse stx
    [(_ WS-STX ... AT-STX)
     (syntax/loc stx AT-STX)]))
(provide top)

(define-syntax (at stx)
  (syntax-parse stx
    [(_ "(" LABEL-EDGES-ORIGIN-STX ")")
     (syntax/loc stx
       (node
        LABEL-EDGES-ORIGIN-STX
        (list)))]
    [(_ "(" LABEL-EDGES-ORIGIN-STX _ SUBTREES-STX ")")
     (syntax/loc stx
       (node
        LABEL-EDGES-ORIGIN-STX
        SUBTREES-STX))]))
(provide at)

(define-syntax (label-edges-origin stx)
  (syntax-parse stx #:literals (graph-edges substitution knowledge)
    [(_ LABEL-STX)
     (syntax/loc stx LABEL-STX)]
    [(_ LABEL-STX _ (graph-edges EDGE-STX ...))
     (syntax/loc stx
       (struct-copy tree-label LABEL-STX [introduced-edges (graph-edges EDGE-STX ...)]))]
    [(_ LABEL-STX _ (substitution SUBST-STX ...) _ (knowledge KNOWLEDGE-STX ...))
     (syntax/loc stx
       (struct-copy
        tree-label
        LABEL-STX
        [substitution (substitution SUBST-STX ...)]
        [rule (knowledge KNOWLEDGE-STX ...)]))]
    [(_ LABEL-STX _ (graph-edges EDGE-STX ...) _ (substitution SUBST-STX ...) _ (knowledge KNOWLEDGE-STX ...))
     (syntax/loc stx
       (struct-copy
        tree-label
        LABEL-STX
        [introduced-edges (graph-edges EDGE-STX ...)]
        [substitution (substitution SUBST-STX ...)]
        [rule (knowledge KNOWLEDGE-STX ...)]))]))
(provide label-edges-origin)

(define-syntax (at-label stx)
  (syntax-parse stx #:literals (acon-with-selection acon-without-selection)
    [(_ NUMBER:number "." (acon-with-selection ACON-STX ...))
     (syntax/loc stx
       (tree-label
        (car (acon-with-selection ACON-STX ...))
        (cdr (acon-with-selection ACON-STX ...)) (list) #f (quote NUMBER) (list)))]
    [(_ NUMBER:number "." (acon-without-selection ACON-STX ...))
     (syntax/loc stx
       (tree-label (acon-without-selection ACON-STX ...) (none) (list) #f (quote NUMBER) (list)))]
    [(_ (acon-without-selection ACON-STX ...))
     (syntax/loc stx
       (tree-label (acon-without-selection ACON-STX ...) (none) (list) #f #f (list)))]))
(provide at-label)

(define-syntax (acon-with-selection stx)
  (syntax-parse stx
    [(_ "*" SELECTED-ATOM-STX "*")
     (syntax/loc stx (cons (list SELECTED-ATOM-STX) (some 0)))]
    [(_ PRECEDING-CONJUNCTION "," "*" SELECTED-ATOM-STX "*")
     (syntax/loc stx
       (cons (append PRECEDING-CONJUNCTION (list SELECTED-ATOM-STX))
             (some (length PRECEDING-CONJUNCTION))))]
    [(_ PRECEDING-CONJUNCTION "," "*" SELECTED-ATOM-STX "*" "," REMAINING-CONJUNCTION)
     (syntax/loc stx
       (cons (append PRECEDING-CONJUNCTION (cons SELECTED-ATOM-STX REMAINING-CONJUNCTION))
             (some (length PRECEDING-CONJUNCTION))))]
    [(_ "*" SELECTED-ATOM-STX "*" "," REMAINING-CONJUNCTION)
     (syntax/loc stx
       (cons (cons SELECTED-ATOM-STX REMAINING-CONJUNCTION) (some 0)))]))
(provide acon-with-selection)

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
    [(_ SYM-STX)
     (syntax/loc stx (ad:abstract-atom (string->symbol (quote SYM-STX)) (list)))]
    [(_ SYM-STX "(" ARG ")")
     (syntax/loc stx
       (ad:abstract-atom
        (string->symbol (quote SYM-STX))
        (list ARG)))]
    [(_ SYM-STX "(" ARG "," COMMA-OR-ARG ... ")")
     (syntax/loc stx
       (ad:abstract-atom
        (string->symbol (quote SYM-STX))
        (cons ARG (ad:abstract-atom-args (abstract-atom SYM-STX "(" COMMA-OR-ARG ... ")")))))]))
(provide abstract-atom)

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
    [(_ NUMBER:number) (syntax/loc stx (ad:abstract-function (->symbol (quote NUMBER))))]
    [(_ symbol:str "(" arg ... ")")
     (syntax/loc stx (ad:abstract-function (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))]))
(provide abstract-function-term)

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

(define-syntax (graph-edges stx)
  (syntax-parse stx
    [(_ "[" PRECEDENCE-STX "]")
     (syntax/loc stx (list PRECEDENCE-STX))]
    [(_ "[" PRECEDENCE-STX "," _ REST-PRECEDENCE-STX ... "]")
     (syntax/loc stx (cons PRECEDENCE-STX (graph-edges "[" REST-PRECEDENCE-STX ... "]")))]))
(provide graph-edges)

(define-syntax (precedence stx)
  (syntax-parse stx
    [(_ AATOM-STX-1 _ "<" _ AATOM-STX-2)
     (syntax/loc stx
       (cons AATOM-STX-1 AATOM-STX-2))]))
(provide precedence)

(define-syntax (substitution stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx (list))]
    [(_ "{" PAIR-STX "}") (syntax/loc stx (list PAIR-STX))]
    [(_ "{" PAIR-STX "," WS COMMA-OR-PAIR-OR-WS-STX ... "}")
     (syntax/loc stx
       (cons PAIR-STX (substitution "{" COMMA-OR-PAIR-OR-WS-STX ... "}")))]))
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
    [(_ NUMBER:number) (syntax/loc stx (cd:function (->symbol (quote NUMBER)) (list)))]
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

(define-syntax (fullai-rule stx)
  (syntax-parse stx
    [(_ AATOM-STX _ "->" _ SUBST-STX)
     (syntax/loc stx
       (full-ai-rule->full-evaluation
        (faid:full-ai-rule AATOM-STX SUBST-STX)))]))
(provide fullai-rule)

(define-syntax (widening-edges stx)
  (syntax-parse stx #:literals (graph-edges acon-with-selection acon-without-selection case-split-edges)
    ; sketchy trick to disambiguate, despite same grammatical structure (as CS and WI are tokenized the same way)
    [(_ "CS" STX ...) (syntax/loc stx (case-split-edges "CS" STX ...))]
    [(_ "WI" _ NUMBER:number "." (acon-with-selection ACON-STX ...) _ (graph-edges EDGE-STX ...))
     (syntax/loc stx
       (widening
        (car (acon-with-selection ACON-STX ...))
        (cdr (acon-with-selection ACON-STX ...))
        "no message"
        (quote NUMBER)
        (graph-edges EDGE-STX ...)))]
    [(_ "WI" _ NUMBER:number "." (acon-with-selection ACON-STX ...))
     (syntax/loc stx
       (widening
        (car (acon-with-selection ACON-STX ...))
        (cdr (acon-with-selection ACON-STX ...))
        "no message"
        (quote NUMBER)
        (list)))]
    [(_ "WI" _ NUMBER:number "." (acon-without-selection ACON-STX ...))
     (syntax/loc stx
       (widening
        (acon-without-selection ACON-STX ...)
        (none)
        "no message"
        (quote NUMBER)
        (list)))]
    [(_ "WI" _ (acon-without-selection ACON-STX ...))
     (syntax/loc stx
       (widening
        (acon-without-selection ACON-STX ...)
        (none)
        "no message"
        #f
        (list)))]))
(provide widening-edges)

(define-syntax (case-split-edges stx)
  (syntax-parse stx #:literals (graph-edges acon-with-selection acon-without-selection)
    [(_ "CS" _ NUMBER:number "." (acon-with-selection ACON-STX ...) _ (graph-edges EDGE-STX ...))
     (syntax/loc stx
       (case
        (car (acon-with-selection ACON-STX ...))
        (cdr (acon-with-selection ACON-STX ...))
        (quote NUMBER)
        (graph-edges EDGE-STX ...)))]
    [(_ "CS" _ NUMBER:number "." (acon-with-selection ACON-STX ...))
     (syntax/loc stx
       (case
        (car (acon-with-selection ACON-STX ...))
        (cdr (acon-with-selection ACON-STX ...))
        (quote NUMBER)
        (list)))]
    [(_ "CS" _ NUMBER:number "." (acon-without-selection ACON-STX ...))
     (syntax/loc stx
       (case
        (acon-without-selection ACON-STX ...)
        (none)
        (quote NUMBER)
        (list)))]
    [(_ "CS" _ (acon-without-selection ACON-STX ...))
     (syntax/loc stx
       (case
        (acon-without-selection ACON-STX ...)
        (none)
        #f
        (list)))]))
(provide case-split-edges)

(define-syntax (cyclenode stx)
  (syntax-parse stx
    [(_ "CY" WS IDX:number)
     (syntax/loc stx (cycle (quote IDX)))]))
(provide cyclenode)

(define-syntax (subtrees stx)
  (syntax-parse stx
    [(_ AT-STX) (syntax/loc stx (list AT-STX))]
    [(_ AT-STX _ REST-STX ...+)
     (syntax/loc stx (cons AT-STX (subtrees REST-STX ...)))]))
(provide subtrees)