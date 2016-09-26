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

#lang br
; for abstract variables, functions, atoms,...
(require (prefix-in ad: "abstract-multi-domain.rkt"))
; because output patterns can be obtained by applying a subsitution
(require (prefix-in as: "abstract-substitution.rkt"))
; for rules on how to fully evaluate
(require (prefix-in fai: "fullai-domain.rkt"))
(require "syntax-utils.rkt") ; to filter out odd elements
(require (prefix-in cd: "concrete-domain.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require (prefix-in ex: "execution.rkt"))
(require (for-syntax syntax/parse))
(require "interaction.rkt")
(require racket/contract)
(require (only-in "data-utils.rkt" 4-tuple))

(require "abstract-domain-ordering.rkt")

; for actual usage
(require (for-syntax "abstract-multi-domain-sexp-conversion.rkt"))
; only for test
(require "abstract-multi-domain-sexp-conversion.rkt")
(require parenlog)

; PUTTING THE THREE PARTS TOGETHER

(define-syntax (cclp-program stx)
  (syntax-parse stx
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{QUERY}" _QUERY-SECTION)
     #'(4-tuple _PROGRAM-SECTION (list) (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{QUERY}" _QUERY-SECTION)
     #'(4-tuple _PROGRAM-SECTION _FULL-EVALUATION-SECTION (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{PREPRIOR}" _PREPRIOR-SECTION "{QUERY}" _QUERY-SECTION)
     #'(4-tuple _PROGRAM-SECTION (list) _PREPRIOR-SECTION _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{PREPRIOR}" _PREPRIOR-SECTION "{QUERY}" _QUERY-SECTION)
     #'(4-tuple _PROGRAM-SECTION _FULL-EVALUATION-SECTION _PREPRIOR-SECTION _QUERY-SECTION)]))
(provide cclp-program)

; PART FOR THE LOGIC PROGRAM ITSELF

(define-syntax (program-section stx)
  (syntax-parse stx
    [(_) #'(list)]
    [(_ _KNOWLEDGE _PERIOD _MOREKNOWLEDGE ...)
     #'(cons _KNOWLEDGE (program-section _MOREKNOWLEDGE ...))]))
(provide program-section)

(define-syntax (atom stx)
  (syntax-parse stx
    [(_ symbol)
     #'(cd:atom (string->symbol symbol) '())]
    [(_ symbol "(" arg ... ")")
     #'(cd:atom (string->symbol symbol) (odd-elems-as-list arg ...))]))
(provide atom)

(define-syntax (term stx)
  (syntax-parse stx
    [(_ VAR-OR-LIST-OR-MISC-FUNCTION)
     #'VAR-OR-LIST-OR-MISC-FUNCTION]))
(provide term)

(define-syntax-rule (variable VARIABLE-NAME) (cd:variable (quote VARIABLE-NAME)))
(provide variable)

(define-syntax (function-term stx)
  (syntax-parse stx
    [(_ symbol:str)
     #'(cd:function (string->symbol symbol) '())]
    [(_ num-term) #'num-term] ; these are just plain numbers
    [(_ symbol "(" arg ... ")")
     #'(cd:function (string->symbol symbol) (odd-elems-as-list arg ...))]))
(provide function-term)

(define-syntax-rule (number NUMBER) (cd:function (number->string (quote NUMBER)) '()))
(provide number)

(define-syntax (lplist stx)
  (syntax-parse stx
    [(_ open-paren close-paren)
     #'(cd:function 'nil '())]
    [(_ open-paren term0 close-paren)
     #'(cd:function 'cons (list term0 (cd:function 'nil '())))]
    [(_ open-paren term0 "," rest ... close-paren)
     #'(cd:function 'cons (list term0 (lplist open-paren rest ... close-paren)))]
    [(_ open-paren term0 "|" rest ... close-paren)
     #'(cd:function 'cons (list term0 rest ...))]))
(provide lplist)

(define-syntax (rule stx)
  (syntax-parse stx
    [(_ atom) #'(ck:rule atom '())]
    [(_ atom ":-" conjunction) #'(ck:rule atom conjunction)]))
(provide rule)

(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_ conjunct ...) #'(odd-elems-as-list conjunct ...)]))
(provide conjunction)

(define-syntax (abstract-conjunction stx)
  (syntax-parse stx
    [(_ conjunct ...) #'(odd-elems-as-list conjunct ...)]))
(provide abstract-conjunction)

; PART RELATED TO FULL EVALUATION

(define-syntax-rule (full-evaluation-section rule ...) (list rule ...))
(provide full-evaluation-section)

(define-syntax-rule (fullai-rule-with-body atom "->" subst ".")
  (fai:full-ai-rule atom subst))
(provide fullai-rule-with-body)

(define-syntax-rule (fullai-rule-without-body atom ".")
  (fai:full-ai-rule atom (list)))
(provide fullai-rule-without-body)

(define-syntax-rule (abstract-atom-with-args symbol "(" arg ... ")")
  (ad:abstract-atom (string->symbol symbol) (odd-elems-as-list arg ...)))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol symbol) (list)))
(provide abstract-atom-without-args)

(define-syntax-rule (abstract-atom with-or-without-args)
  with-or-without-args)
(provide abstract-atom)

(define-syntax-rule (abstract-term specific-term) specific-term)
(provide abstract-term)

(define-syntax-rule (abstract-variable specific-var) specific-var)
(provide abstract-variable)

(define-syntax-rule (abstract-variable-a "α" index) (ad:a (quote index)))
(provide abstract-variable-a)

(define-syntax-rule (abstract-variable-g "γ" index) (ad:g (quote index)))
(provide abstract-variable-g)

(define-syntax (abstract-function-term stx)
  (syntax-parse stx
    [(_ symbol:str) #'(ad:abstract-function (string->symbol symbol) '())]
    [(_ num-term) #'num-term]
    [(_ symbol "(" arg ... ")")
     #'(ad:abstract-function (string->symbol symbol) (odd-elems-as-list arg ...))]))
(provide abstract-function-term)

(define-syntax-rule (number-term NUMBER)
  (ad:abstract-function (number->string (quote NUMBER)) '()))
(provide number-term)

(define-syntax (abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     #'(ad:abstract-function 'nil '())]
    [(_ "[" term0 "]")
     #'(ad:abstract-function 'cons (list term0 (ad:abstract-function 'nil '())))]
    [(_ "[" term0 "," rest ... "]")
     #'(ad:abstract-function 'cons (list term0 (abstract-lplist "[" rest ... "]")))]
    [(_ "[" term0 "|" rest ... "]")
     #'(ad:abstract-function 'cons (list term0 rest ...))]))
(provide abstract-lplist)

; empty substitutions make sense if we can just scratch the abstract atom
; e.g. lte(g1,g2) just disappears and does not need a substitution
(define-syntax (abstract-substitution stx)
  (syntax-parse stx
    [(_) (list)]
    [(_ lhs0 lhs1 ...)
     #'(odd-elems-as-list lhs0 lhs1 ...)]))
(provide abstract-substitution)

(define-syntax-rule (abstract-substitution-pair lhs "/" rhs)
  (as:abstract-equality lhs rhs))
(provide abstract-substitution-pair)

; PART RELATED TO PREPRIOR

(define-syntax (preprior-section stx)
  (syntax-case stx ()
    [(_ pair ...)
     (with-syntax
         ([(partially-expanded-pair ...) (map partially-expand-pair (syntax->list #'(pair ...)))])
       #`((λ () (define-model prior
                  partially-expanded-pair ...
                  (not_a_member X ())
                  (:- (not_a_member X (cons A B))
                      (,(compose not equal?) X A)
                      (not_a_member X B))
                  (:- (reaches_without_encountering X Y Path)
                      (before X Y)
                      (not_a_member Y Path))
                  (:- (reaches_without_encountering X Z Path)
                      (before X Y)
                      (not_a_member Y Path)
                      (reaches_without_encountering Y Z (cons Y Path)))
                  (:- (reaches_loopfree X Y)
                      (reaches_without_encountering X Y (cons X ())))
                  (:- (violates_partial_order)
                      (reaches_loopfree X Y)
                      (reaches_loopfree Y X)
                      (,(compose not
                                 (λ (sexp1 sexp2) (renames? (sexp->abstract-atom sexp1)
                                                            (sexp->abstract-atom sexp2)))) X Y))
                  (reaches_all_under_consistency X ())
                  (:- (reaches_all_under_consistency X (cons Destination Ds))
                      (sexp_gt_extension Destination X)
                      (reaches_all_under_consistency X Ds))
                  (:- (reaches_all_under_consistency X (cons Destination Ds))
                      (reaches_under_consistency X Destination)
                      (reaches_all_under_consistency X Ds))
                  (:- (reaches_under_consistency X Y)
                      (reaches_loopfree X1 Y1)
                      (sexp_gt_extension X1 X)
                      (sexp_gt_extension Y Y1))
                  (:- (sexp_gt_extension X Y)
                      (,(λ (e1 e2) (>=-extension (sexp->abstract-atom X)
                                                 (sexp->abstract-atom Y))) X Y)))
            prior)))]))
(provide preprior-section)

(define-for-syntax (partially-expand-pair stx)
  (syntax-case stx ()
    ; FIXME is this producing something like "(before a b)" when it should produce (before (a) (b))?
    ; (testable-partially-expand-pair #`(bleh (abp:parse-abstract-atom "a") "," (abp:parse-abstract-atom "b"))) does precisely that
    [(_ atom1 "," atom2)
     #`(before #,(abstract-domain-elem->sexp (eval-syntax #'atom1))
               #,(abstract-domain-elem->sexp (eval-syntax #'atom2)))]))

(define (testable-partially-expand-pair stx)
  (syntax-case stx ()
    [(_ atom1 "," atom2)
     #`(before #,(abstract-domain-elem->sexp (eval-syntax #'atom1))
               #,(abstract-domain-elem->sexp (eval-syntax #'atom2)))]))

; AND THE GLUE TO GO TO TOP-LEVEL INTERACTION
; can we get the filename of the program being run? would be useful for serialization
(define #'(cclp-module-begin _PARSE-TREE ...)
  #'(#%module-begin (cclp-run current-contract-region _PARSE-TREE ...)))
(provide (rename-out [cclp-module-begin #%module-begin]) #%top-interaction)