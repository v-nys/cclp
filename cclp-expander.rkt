; MIT License
;
; Copyright (c) 2016 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#lang br/quicklang
; for abstract variables, functions, atoms,...
(require (prefix-in ad: "abstract-multi-domain.rkt"))
; because output patterns can be obtained by applying a subsitution
(require (prefix-in as: "abstract-substitution.rkt"))
; for rules on how to fully evaluate
(require (prefix-in fai: "fullai-domain.rkt"))
(require (only-in "syntax-utils.rkt" odd-elems-as-list))
(require (prefix-in cd: "concrete-domain.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require (for-syntax syntax/parse))
(require (only-in "interaction.rkt" cclp-top cclp))
(require racket/contract)
(require "abstract-domain-ordering.rkt")
(require "preprior-graph.rkt")
(require (only-in sugar/coerce ->symbol))
(require (for-syntax (only-in racket-list-utils/utils odd-elems)))

; PUTTING THE THREE PARTS TOGETHER

; can I make this more modular?
(define-syntax (cclp-program stx)
  (syntax-parse stx
    ; no full ai rules, no concrete constants
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{QUERY}" _QUERY-SECTION)
     (syntax/loc stx (cclp _PROGRAM-SECTION (list) (list) _QUERY-SECTION))]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{QUERY}" _QUERY-SECTION)
     (syntax/loc stx (cclp _PROGRAM-SECTION _FULL-EVALUATION-SECTION (list) _QUERY-SECTION))]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     (syntax/loc stx (cclp _PROGRAM-SECTION (list) _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION))]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     (syntax/loc stx (cclp _PROGRAM-SECTION _FULL-EVALUATION-SECTION _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION))]))
(provide cclp-program)

; AND THE GLUE TO GO TO TOP-LEVEL INTERACTION
(define-syntax (cclp-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE ...)
     (syntax/loc stx (#%module-begin
                      (cclp-top current-contract-region _PARSE-TREE ...)))]))
(provide (rename-out [cclp-module-begin #%module-begin]) #%top-interaction)

; PART FOR THE LOGIC PROGRAM ITSELF
(define-for-syntax (inject-rule-id-stx rule-stx id)
  (syntax-parse rule-stx #:literals (rule)
    [(rule arg ...)
     (cons
      (quasisyntax/loc rule-stx
        (rule arg ... #,id))
      (add1 id))]))

(require (for-syntax (only-in racket-list-utils/utils map-accumulatel)))
(define-syntax (program-section stx)
  (with-syntax ([(RULE-STX ...)
                 (car
                  (map-accumulatel
                   inject-rule-id-stx
                   1
                   (odd-elems (cdr (syntax->list stx)))))])
    (syntax/loc stx
      (list RULE-STX ...))))
(provide program-section)

(define-syntax (atom stx)
  (syntax-parse stx
    [(_ symbol)
     (syntax/loc stx (cd:atom (string->symbol (quote symbol)) '()))]
    [(_ symbol "(" arg ... ")")
     (syntax/loc stx (cd:atom (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))]))
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

(define-syntax (rule stx)
  (syntax-parse stx
    [(_ atom id) (syntax/loc stx (ck:rule atom '() id))]
    [(_ atom ":-" conjunction id) (syntax/loc stx (ck:rule atom conjunction id))]))
(provide rule)

(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_ conjunct ...) (syntax/loc stx (odd-elems-as-list conjunct ...))]))
(provide conjunction)

(define-syntax (abstract-conjunction stx)
  (syntax-parse stx
    [(_ conjunct ...) (syntax/loc stx (odd-elems-as-list conjunct ...))]))
(provide abstract-conjunction)

; PART RELATED TO FULL EVALUATION

(define-syntax (fail stx)
  (syntax-parse stx
    [(_ "fail") (syntax/loc stx #f)]))
(provide fail)

(define-for-syntax (inject-full-ai-rule air-stx id)
  (syntax-parse
      air-stx
    [(type args ...)
     (cons #`(type args ... #,id) (add1 id))]))

(define-syntax (full-evaluation-section stx)
  (with-syntax
      ([(INJECTED-RULE ...)
        (car
         (map-accumulatel
          inject-full-ai-rule
          1
          (cdr (syntax->list stx))))])
    (syntax/loc stx (list INJECTED-RULE ...))))
(provide full-evaluation-section)

(define-syntax-rule (fullai-rule-with-body atom "->" subst "." idx)
  (fai:full-ai-rule atom subst idx))
(provide fullai-rule-with-body)

(define-syntax-rule (fullai-rule-without-body atom "." idx)
  (fai:full-ai-rule atom (list) idx))
(provide fullai-rule-without-body)

(define-syntax-rule (abstract-atom-with-args symbol "(" arg ... ")")
  (ad:abstract-atom (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide abstract-atom-without-args)

(define-syntax (abstract-atom stx)
  (syntax-parse stx [(_ args-or-nothing) (syntax/loc stx args-or-nothing)]))
(provide abstract-atom)

(define-syntax-rule (abstract-term specific-term) specific-term)
(provide abstract-term)

(define-syntax-rule (abstract-variable specific-var) specific-var)
(provide abstract-variable)

; M.O.: string is hier overbodig...
(define-syntax-rule (abstract-variable-a "α" index) (ad:a (quote index)))
(provide abstract-variable-a)

(define-syntax-rule (abstract-variable-g "γ" index) (ad:g (quote index)))
(provide abstract-variable-g)

(define-syntax (abstract-function-term stx)
  (syntax-parse stx
    [(_ symbol:str) (syntax/loc stx (ad:abstract-function (string->symbol (quote symbol)) '()))]
    [(_ num-term) (syntax/loc stx num-term)]
    [(_ symbol "(" arg ... ")")
     (syntax/loc stx (ad:abstract-function (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))]))
(provide abstract-function-term)

(define-syntax-rule (abstract-number NUMBER)
  (ad:abstract-function (->symbol (quote NUMBER)) '()))
(provide abstract-number)

(define-syntax-rule (abstract-number-term TERM) TERM)
(provide abstract-number-term)

(define-syntax-rule (number-term TERM)
  (cd:function (->symbol (quote TERM)) '()))
(provide number-term)

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

; empty substitutions make sense if we can just scratch the abstract atom
; e.g. lte(g1,g2) just disappears and does not need a substitution
(define-syntax (abstract-substitution stx)
  (syntax-parse stx
    [(_) (list)]
    [(_ lhs0 lhs1 ...)
     (syntax/loc stx (odd-elems-as-list lhs0 lhs1 ...))]))
(provide abstract-substitution)

(define-syntax-rule (abstract-substitution-pair lhs "/" rhs)
  (as:abstract-equality lhs rhs))
(provide abstract-substitution-pair)

(define-syntax (concrete-constant stx)
  (syntax-parse stx
    [(_ _CONSTANT-SYMBOL)
     (with-syntax
         ([CON-SYM (datum->syntax #'_CONSTANT-SYMBOL (string->symbol (syntax->datum #'_CONSTANT-SYMBOL)))])
       (syntax/loc stx (cd:function (quote CON-SYM) (list))))]))
(provide concrete-constant)

(define-syntax (concrete-constants-section stx)
  (syntax-parse stx
    [(_ _CONCRETE-CONSTANT ...) (syntax/loc stx (list _CONCRETE-CONSTANT ...))]))
(provide concrete-constants-section)

(module+ test
  (require rackunit)
  (check-equal? (number-term 4) (cd:function (->symbol 4) '()))
  (check-equal? (lplist "[" "]") (cd:function 'nil '()))

  (check-equal? (abstract-variable-a "α" 1) (ad:a 1))
  (check-equal? (abstract-variable-g "γ" 2) (ad:g 2))
  (check-equal? (abstract-number 3) (ad:abstract-function (->symbol 3) '()))
  (check-equal? (abstract-variable (abstract-variable-a "α" 1)) (ad:a 1))
  (check-equal? (abstract-variable (abstract-variable-g "γ" 2)) (ad:g 2))
  (check-equal? (abstract-number-term (abstract-number 3)) (ad:abstract-function (->symbol 3) '()))
  (check-equal? (abstract-function-term "my-func") (ad:abstract-function 'my-func '()))
  (check-equal? (abstract-atom-without-args "my-atom") (ad:abstract-atom 'my-atom '()))
  (check-equal? (abstract-atom (abstract-atom-without-args "my-atom"))
                (ad:abstract-atom 'my-atom '()))
  (check-equal? (abstract-lplist "[" "]") (ad:abstract-function 'nil '()))
  (check-equal? (abstract-lplist
                 "["
                 (abstract-variable (abstract-variable-g "γ" 2))
                 ","
                 (abstract-variable (abstract-variable-a "α" 1))
                 "]")
                (ad:abstract-function
                 'cons
                 (list (ad:g 2)
                       (ad:abstract-function
                        'cons
                        (list (ad:a 1)
                              (ad:abstract-function 'nil '()))))))
  (check-equal? (abstract-lplist
                 "["
                 (abstract-variable (abstract-variable-g "γ" 2))
                 ","
                 (abstract-variable (abstract-variable-a "α" 1))
                 "|"
                 (abstract-variable (abstract-variable-a "α" 2))
                 "]")
                (ad:abstract-function
                 'cons
                 (list (ad:g 2) (ad:abstract-function 'cons (list (ad:a 1) (ad:a 2))))))
  (check-equal? (conjunction
                 (abstract-atom (abstract-atom-without-args "my-atom1"))
                 "," (abstract-atom (abstract-atom-without-args "my-atom2"))
                 "," (abstract-atom (abstract-atom-without-args "my-atom3")))
                (list (ad:abstract-atom 'my-atom1 '())
                      (ad:abstract-atom 'my-atom2 '())
                      (ad:abstract-atom 'my-atom3 '())))
  (check-equal? (abstract-function-term
                 "my-func"
                 "("
                 (abstract-term (abstract-variable (abstract-variable-g "γ" 1)))
                 ","
                 (abstract-term (abstract-variable (abstract-variable-g "γ" 2))) ")")
                (ad:abstract-function 'my-func (list (ad:g 1) (ad:g 2))))
  (check-equal? (abstract-atom
                 (abstract-atom-with-args
                  "my-atom"
                  "("
                  (abstract-variable (abstract-variable-g "γ" 1))
                  ","
                  (abstract-variable (abstract-variable-g "γ" 2))
                  ")"))
                (ad:abstract-atom 'my-atom (list (ad:g 1) (ad:g 2))))

  ; concrete program section
  (check-equal?
   (variable "MyPrologVar")
   (cd:variable 'MyPrologVar))

  ; full eval section
  (check-equal?
   (fullai-rule-without-body
    (abstract-atom-with-args
     "myatom"
     "("
     (abstract-variable
      (abstract-variable-g "γ" 1))
     ","
     (abstract-variable
      (abstract-variable-g "γ" 2))
     ")")
    "."
    1)
   (fai:full-ai-rule
    (ad:abstract-atom 'myatom (list (ad:g 1) (ad:g 2)))
    (list)
    1))

  (check-equal?
   (fullai-rule-with-body
    (abstract-atom-with-args
     "del"
     "("
     (abstract-variable
      (abstract-variable-a "α" 1))
     ","
     (abstract-variable
      (abstract-variable-g "γ" 1))
     ","
     (abstract-variable
      (abstract-variable-a "α" 2))
     ")")
    "->"
    (abstract-substitution
     (abstract-substitution-pair
      (abstract-variable
       (abstract-variable-a "α" 1))
      "/"
      (abstract-variable
       (abstract-variable-g "γ" 2)))
     ","
     (abstract-substitution-pair
      (abstract-variable
       (abstract-variable-a "α" 2))
      "/"
      (abstract-variable
       (abstract-variable-g "γ" 3))))
    "."
    1)
   (fai:full-ai-rule
    (ad:abstract-atom 'del (list (ad:a 1) (ad:g 1) (ad:a 2)))
    (list (as:abstract-equality (ad:a 1) (ad:g 2))
          (as:abstract-equality (ad:a 2) (ad:g 3)))
    1)))