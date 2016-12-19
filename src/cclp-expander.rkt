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
(require (only-in "syntax-utils.rkt" odd-elems-as-list))
(require (prefix-in cd: "concrete-domain.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require (for-syntax syntax/parse))
(require (only-in "interaction.rkt" cclp-top cclp))
(require racket/contract)
(require (for-syntax (only-in racket-list-utils/utils odd-elems)))
(require (for-syntax (only-in "data-utils.rkt" positive-integer->symbol)))
(require (for-syntax (only-in racket remove-duplicates match second third)))

(require "abstract-domain-ordering.rkt")

(require (for-syntax "abstract-multi-domain-sexp-conversion.rkt"))
(require "abstract-multi-domain-sexp-conversion.rkt")
(require parenlog)

; a "generic" model for a partial order
; relevant atoms (i.e. those explicitly in 'before' pairs are specified
; and the before relations themselves
(define blank-model
  (i/model
   `((member X (cons X Y))
     (:- (member X (cons Y Z))
         (member X Z))
     (not_a_member X ())
     (:- (not_a_member X (cons A B))
         (,(compose not equal?) X A)
         (not_a_member X B))
     (:- (reaches_without_encountering X Y Path)
         (before X1 Y1)
         (not_a_member Y1 Path)
         (sexp_gt_extension X1 X)
         (sexp_gt_extension Y Y1))
     (:- (reaches_without_encountering X Z Path)
         (before X1 Y)
         (not_a_member Y Path)
         (sexp_gt_extension X1 X)
         (relevant_atoms R)
         (member Z1 R)
         (sexp_gt_extension Z Z1)
         (reaches_without_encountering Y Z1 (cons Y Path)))
     (:- (reaches_loopfree X Y)
         (reaches_without_encountering X Y (cons X ())))
     (:- (violates_partial_order)
         (relevant_atoms R)
         (member X R)
         (member Y R)
         (reaches_loopfree X Y)
         (reaches_loopfree Y X)
         (,(compose not
                    (λ (sexp1 sexp2) (renames? (sexp->abstract-atom sexp1)
                                               (sexp->abstract-atom sexp2)))) X Y))
     (:- (member_reaches_or_includes_all_under_consistency X Atoms)
         (member X Atoms)
         (reaches_or_includes_all_under_consistency X Atoms))
     (reaches_or_includes_all_under_consistency X ())
     (:- (reaches_or_includes_all_under_consistency X (cons Destination Ds))
         (sexp_gt_extension Destination X)
         (reaches_or_includes_all_under_consistency X Ds))
     (:- (reaches_or_includes_all_under_consistency X (cons Destination Ds))
         (reaches_loopfree X Destination)
         (reaches_or_includes_all_under_consistency X Ds))
     (:- (sexp_gt_extension X Y)
         (,(λ (e1 e2) (>=-extension (sexp->abstract-atom e1)
                                    (sexp->abstract-atom e2))) X Y)))))

; PUTTING THE THREE PARTS TOGETHER

; can I make this more modular?
(define-syntax (cclp-program stx)
  (syntax-parse stx
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{QUERY}" _QUERY-SECTION)
     #'(cclp _PROGRAM-SECTION (list) blank-model (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{QUERY}" _QUERY-SECTION)
     #'(cclp _PROGRAM-SECTION _FULL-EVALUATION-SECTION blank-model (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     #'(cclp _PROGRAM-SECTION (list) blank-model _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     #'(cclp _PROGRAM-SECTION _FULL-EVALUATION-SECTION blank-model _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION)]))
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
     #'(cd:atom (string->symbol (quote symbol)) '())]
    [(_ symbol "(" arg ... ")")
     #'(cd:atom (string->symbol (quote symbol)) (odd-elems-as-list arg ...))]))
(provide atom)

(define-syntax (term stx)
  (syntax-parse stx
    [(_ VAR-OR-LIST-OR-MISC-FUNCTION)
     #'VAR-OR-LIST-OR-MISC-FUNCTION]))
(provide term)

(define-syntax-rule (variable VARIABLE-NAME) (cd:variable (string->symbol (quote VARIABLE-NAME))))
(provide variable)

(define-syntax (function-term stx)
  (syntax-parse stx
    [(_ symbol:str)
     #'(cd:function (string->symbol (quote symbol)) '())]
    [(_ num-term) #'num-term] ; these are just plain numbers
    [(_ symbol "(" arg ... ")")
     #'(cd:function (string->symbol (quote symbol)) (odd-elems-as-list arg ...))]))
(provide function-term)

(define-syntax (lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     #'(cd:function 'nil '())]
    [(_ "[" term0 "]")
     #'(cd:function 'cons (list term0 (cd:function 'nil '())))]
    [(_ "[" term0 "," rest ... "]")
     #'(cd:function 'cons (list term0 (lplist "[" rest ... "]")))]
    [(_ "[" term0 "|" rest ... "]")
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
  (ad:abstract-atom (string->symbol (quote symbol)) (odd-elems-as-list arg ...)))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide abstract-atom-without-args)

(define-syntax (abstract-atom stx)
  (syntax-parse stx [(_ args-or-nothing) #'args-or-nothing]))
(provide abstract-atom)

(define-syntax-rule (abstract-term specific-term) specific-term)
(provide abstract-term)

(define-syntax-rule (abstract-variable specific-var) specific-var)
(provide abstract-variable)

; M.O.: string is hier eigenlijk vrij overbodig...
(define-syntax-rule (abstract-variable-a "α" index) (ad:a (quote index)))
(provide abstract-variable-a)

(define-syntax-rule (abstract-variable-g "γ" index) (ad:g (quote index)))
(provide abstract-variable-g)

(define-syntax (abstract-function-term stx)
  (syntax-parse stx
    [(_ symbol:str) #'(ad:abstract-function (string->symbol (quote symbol)) '())]
    [(_ num-term) #'num-term]
    [(_ symbol "(" arg ... ")")
     #'(ad:abstract-function (string->symbol (quote symbol)) (odd-elems-as-list arg ...))]))
(provide abstract-function-term)

(define-syntax-rule (abstract-number NUMBER)
  (ad:abstract-function (quote NUMBER) '()))
(provide abstract-number)

(define-syntax-rule (abstract-number-term TERM) TERM)
(provide abstract-number-term)

(define-syntax-rule (number-term TERM)
  (cd:function (quote TERM) '()))
(provide number-term)

(define-syntax (abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     #'(ad:abstract-function 'nil '())]
    [(_ "[" term0 "]")
     #'(ad:abstract-function 'cons (list term0 (ad:abstract-function 'nil '())))]
    [(_ "[" term0 "," rest ... "]")
     #'(ad:abstract-function 'cons (list term0 (abstract-lplist "[" rest ... "]")))]
    [(_ "[" term0 "|" rest "]")
     #'(ad:abstract-function 'cons (list term0 rest))]))
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

(define-syntax (concrete-constant stx)
  (syntax-parse stx
    [(_ _CONSTANT-SYMBOL)
     (with-syntax
         ([CON-SYM (datum->syntax #'_CONSTANT-SYMBOL (string->symbol (syntax->datum #'_CONSTANT-SYMBOL)))])
       #'(cd:function (quote CON-SYM) (list)))]))
(provide concrete-constant)

(define-syntax (concrete-constants-section stx)
  (syntax-parse stx
    [(_ _CONCRETE-CONSTANT ...) #'(list _CONCRETE-CONSTANT ...)]))
(provide concrete-constants-section)

; AND THE GLUE TO GO TO TOP-LEVEL INTERACTION
(define-syntax (cclp-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE ...) #'(#%module-begin (cclp-top current-contract-region _PARSE-TREE ...))]))
(provide (rename-out [cclp-module-begin #%module-begin]) #%top-interaction)

(module+ test
  (require rackunit)
  (check-equal? (number-term 4) (cd:function 4 '()))
  (check-equal? (lplist "[" "]") (cd:function 'nil '()))

  (check-equal? (abstract-variable-a "α" 1) (ad:a 1))
  (check-equal? (abstract-variable-g "γ" 2) (ad:g 2))
  (check-equal? (abstract-number 3) (ad:abstract-function 3 '()))
  (check-equal? (abstract-variable (abstract-variable-a "α" 1)) (ad:a 1))
  (check-equal? (abstract-variable (abstract-variable-g "γ" 2)) (ad:g 2))
  (check-equal? (abstract-number-term (abstract-number 3)) (ad:abstract-function 3 '()))
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
    ".")
   (fai:full-ai-rule
    (ad:abstract-atom 'myatom (list (ad:g 1) (ad:g 2)))
    (list)))

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
    ".")
   (fai:full-ai-rule
    (ad:abstract-atom 'del (list (ad:a 1) (ad:g 1) (ad:a 2)))
    (list (as:abstract-equality (ad:a 1) (ad:g 2))
          (as:abstract-equality (ad:a 2) (ad:g 3))))))