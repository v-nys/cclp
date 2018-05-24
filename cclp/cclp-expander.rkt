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

#lang racket
; for abstract variables, functions, atoms,...
(require (prefix-in ad: cclp-common-data/abstract-multi-domain))
; because output patterns can be obtained by applying a subsitution
(require (prefix-in as: cclp-common-data/abstract-substitution)
         (only-in cclp-common/abstract-analysis full-ai-rule->full-evaluation))
; for rules on how to fully evaluate
(require cclp-common-data/abstract-knowledge)
(require (only-in "syntax-utils.rkt" odd-elems-as-list))
(require (prefix-in cd: cclp-common-data/concrete-domain))
(require (prefix-in ck: cclp-common-data/concrete-knowledge))
(require (for-syntax syntax/parse))
(require cclp-analysis/interaction)
(require racket/contract)
(require cclp-common/abstract-domain-ordering)
(require cclp-common/preprior-graph (only-in graph add-directed-edge! add-vertex!))
(require (only-in sugar/coerce ->symbol))
(require (for-syntax (only-in list-utils odd-elems)))
(require racket/provide)

(require
  (rename-in
   cclp-syntax/common-data-expander
   [ag-atom cclp-atom]
   [ag-termlist cclp-termlist]
   [ag-term cclp-term]
   [ag-variable cclp-variable]
   [ag-function-term cclp-function-term]
   [ag-number-term cclp-number-term]
   [ag-lplist cclp-lplist]
   [ag-abstract-atom-with-args cclp-abstract-atom-with-args]
   [ag-abstract-termlist cclp-abstract-termlist]
   [ag-abstract-term cclp-abstract-term]
   [ag-abstract-variable cclp-abstract-variable]
   [ag-abstract-number cclp-abstract-number]
   [ag-abstract-function-term cclp-abstract-function-term]
   [ag-abstract-atom-without-args cclp-abstract-atom-without-args]
   [ag-abstract-lplist cclp-abstract-lplist]
   [ag-abstract-atom cclp-abstract-atom]
   [ag-fail cclp-fail]))

(define (extract-program-constants e)
  (match e
    [(? list?)
     (remove-duplicates
      (append-map extract-program-constants e))]
    [(ck:rule h t id)
     (append-map extract-program-constants (cons h t))]
    [(cd:function sym (list))
     (list e)]
    [(or (cd:atom sym args)
         (cd:function sym args))
     (append-map extract-program-constants args)]
    [_ empty]))

(define-syntax (cclp-program stx)
  (syntax-parse stx
    [(_ "{PROGRAM}" _PROGRAM-SECTION
        OPTIONAL-SECTION ...
        "{QUERY}" _QUERY-SECTION)
     (with-syntax ([(_FULL-EVALUATION-SECTION _PARTIAL-ORDER-SECTION _K-SECTION)
                    (syntax->list (optional-cclp-sections/full-evaluation #'(OPTIONAL-SECTION ...)))])
       (syntax/loc stx
         (cclp _PROGRAM-SECTION _FULL-EVALUATION-SECTION (extract-program-constants _PROGRAM-SECTION) _PARTIAL-ORDER-SECTION _QUERY-SECTION "dummy" _K-SECTION)))]))

(define-for-syntax (optional-cclp-sections/full-evaluation stx)
  (syntax-parse stx
    [("{FULL EVALUATION}" _FULL-EVALUATION-SECTION REST ...)
     (with-syntax ([(_PARTIAL-ORDER-SECTION _K-SECTION)
                    (optional-cclp-sections/partial-order #'(REST ...))])
       (syntax/loc stx (_FULL-EVALUATION-SECTION _PARTIAL-ORDER-SECTION _K-SECTION)))]
    [(REST ...)
     (with-syntax ([(_PARTIAL-ORDER-SECTION _K-SECTION)
                    (optional-cclp-sections/partial-order #'(REST ...))])
       (syntax/loc stx ((list) _PARTIAL-ORDER-SECTION _K-SECTION)))]))

(define-for-syntax (optional-cclp-sections/partial-order stx)
  (syntax-parse stx
    [("{PARTIAL ORDER}" _PARTIAL-ORDER-SECTION REST ...)
     (with-syntax ([_K-SECTION (optional-cclp-sections/k #'(REST ...))])
       (syntax/loc stx (_PARTIAL-ORDER-SECTION _K-SECTION)))]
    [(REST ...)
     (with-syntax ([_K-SECTION (optional-cclp-sections/k #'(REST ...))])
       (syntax/loc stx ((mk-preprior-graph) _K-SECTION)))]))

(define-for-syntax (optional-cclp-sections/k stx)
  (syntax-parse stx
    [("{K}" K)
     (syntax/loc stx (quote K))]
    [()
     (syntax/loc stx #f)]))

; AND THE GLUE TO GO TO TOP-LEVEL INTERACTION
(define-syntax (cclp-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE ...)
     (syntax/loc stx
       (#%module-begin
        (define fn (current-contract-region)) ; inside submodule this is not a string but a list!
        (define initial-program-analysis
          (let ([initial _PARSE-TREE ...])
            (cclp->initial-analysis
             (struct-copy
              cclp
              initial
              [filename fn]
              [full-ai-rules
               (map
                full-ai-rule->full-evaluation
                (cclp-full-ai-rules initial))]))))
        (provide initial-program-analysis)
        (module+ main
          (require (only-in racket/set set->list)
                   repeated-application
                   cclp-analysis/synthesis)
          (let* ([complete-analysis (apply↑* proceed initial-program-analysis)]
                 [segments ((compose sort-segments set->list synthesizable-segments analysis-tree) complete-analysis)]
                 [resultants (map (compose pretty-print-rule (λ (b) (branch->clause b))) segments)])
            (for-each
             displayln
             resultants)))))]))

; PART FOR THE LOGIC PROGRAM ITSELF
(define-for-syntax (inject-rule-id-stx rule-stx id)
  (syntax-parse rule-stx #:literals (cclp-rule)
    [(cclp-rule arg ...)
     (cons
      (quasisyntax/loc rule-stx
        (cclp-rule arg ... #,id))
      (add1 id))]))

(require (for-syntax (only-in list-utils map-accumulatel)))
(define-syntax (cclp-program-section stx)
  (with-syntax ([(RULE-STX ...)
                 (car
                  (map-accumulatel
                   inject-rule-id-stx
                   1
                   (odd-elems (cdr (syntax->list stx)))))])
    (syntax/loc stx
      (list RULE-STX ...))))

(define-syntax (cclp-rule stx)
  (syntax-parse stx
    [(_ atom id) (syntax/loc stx (ck:rule atom '() id))]
    [(_ atom ":-" conjunction id) (syntax/loc stx (ck:rule atom conjunction id))]))

(define-syntax (cclp-conjunction stx)
  (syntax-parse stx
    [(_ conjunct ...) (syntax/loc stx (odd-elems-as-list conjunct ...))]))

; PART RELATED TO FULL EVALUATION

(define-for-syntax (inject-full-ai-rule air-stx id)
  (syntax-parse
      air-stx
    [(type args ...)
     (cons #`(type args ... #,id) (add1 id))]))

(define-syntax (cclp-full-evaluation-section stx)
  (with-syntax
      ([(INJECTED-RULE ...)
        (car
         (map-accumulatel
          inject-full-ai-rule
          1
          (cdr (syntax->list stx))))])
    (syntax/loc stx (list INJECTED-RULE ...))))

(define-syntax-rule (cclp-fullai-rule-with-body atom "->" subst "." idx)
  (full-ai-rule atom subst idx))

(define-syntax-rule (cclp-fullai-rule-without-body atom "." idx)
  (full-ai-rule atom (list) idx))

(define-syntax (cclp-abstract-substitution stx)
  (syntax-parse stx
    [(_ subst-pair) (syntax/loc stx (list subst-pair))]
    [(_ subst-pair "," rest-arg ...) (syntax/loc stx (cons subst-pair (cclp-abstract-substitution rest-arg ...)))]))

(define-syntax-rule (cclp-abstract-substitution-pair lhs "/" rhs)
  (as:abstract-equality lhs rhs))

(define-syntax (cclp-partial-order-section stx)
  (syntax-parse stx
    [(_ PAIR ...)
     (syntax/loc stx
       (let ([g (mk-preprior-graph)])
         (for ([p (list PAIR ...)])
           (add-directed-edge! g (car p) (cdr p)))
         g))]))

(define-syntax (cclp-partial-ordering-pair stx)
  (syntax-parse stx
    [(_ A _ B)
     (syntax/loc stx
       (cons A B))]))

(provide
 ; note: matching-identifiers-out does not play nice with rename-in used above
 cclp-abstract-atom
 cclp-abstract-atom-with-args
 cclp-abstract-atom-without-args
 cclp-abstract-lplist
 cclp-abstract-substitution
 cclp-abstract-substitution-pair
 cclp-abstract-term
 cclp-abstract-termlist
 cclp-abstract-variable
 cclp-abstract-function-term
 cclp-atom
 cclp-conjunction
 cclp-full-evaluation-section
 cclp-fullai-rule-with-body
 cclp-fullai-rule-without-body
 cclp-lplist
 cclp-partial-order-section
 cclp-partial-ordering-pair
 cclp-program
 cclp-program-section
 cclp-rule
 cclp-term
 cclp-termlist
 cclp-variable
 cclp-function-term
 cclp-number-term
 cclp-abstract-number
 (rename-out
  [cclp-module-begin #%module-begin]))