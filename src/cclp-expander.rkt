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
(require (only-in "interaction.rkt" cclp-run))
(require racket/contract)
(require (only-in "data-utils.rkt" 4-tuple))

(require "abstract-domain-ordering.rkt")

(require (for-syntax "abstract-multi-domain-sexp-conversion.rkt"))
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
     #'(cd:atom (quote (string->symbol symbol)) '())]
    [(_ symbol "(" arg ... ")")
     #'(cd:atom (quote (string->symbol symbol)) (odd-elems-as-list arg ...))]))
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
  (ad:abstract-atom (quote (string->symbol symbol)) (odd-elems-as-list arg ...)))
(provide abstract-atom-with-args)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (quote (string->symbol symbol)) (list)))
(provide abstract-atom-without-args)

(define-syntax (abstract-atom stx)
  (syntax-parse stx [(_ args-or-nothing) #'args-or-nothing]))
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
  (begin (println (syntax->datum stx))
         (syntax-parse stx
           [(_ pair ...)
            #`((λ () (define-model prior
                       ; problem with permutation sort is here
                       ; just leaving out the pair causes interactive analysis to start
                       ; interestingly, just writing (before a b) works, so expansion (if any?!) is not something like that
                       ; would be very helpful if there was a way to see full program expansion
                       ; also, still getting the info about "perm"
                       ; oddly, expanding pair to void also doesn't allow the program to run
                       ; so my understanding of how these macros work is flawed
                       ; how do I figure it out?
                       ; test: first define syntax for a pair using datum->syntax, then check its expansion? 
                       pair ...
                       (member X (cons X Y))
                       (:- (member X (cons Y Z))
                           (member X Z))
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
                       (:- (member_reaches_all_under_consistency X Atoms)
                           (member X Atoms)
                           (reaches_all_under_consistency X Atoms))
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
                           (,(λ (e1 e2) (>=-extension (sexp->abstract-atom e1)
                                                      (sexp->abstract-atom e2))) X Y)))
                 prior))])))
(provide preprior-section)

; need some phase 2 stuff here...
(require (for-syntax (prefix-in sad: "abstract-multi-domain.rkt")))
(require (for-syntax (for-syntax syntax/parse) (for-syntax racket/base)))
(require (for-syntax (only-in "syntax-utils.rkt" odd-elems-as-list)))
(begin-for-syntax
  (define-syntax (abstract-atom stx)
    (syntax-parse stx [(_ args-or-nothing) #'args-or-nothing]))
  (define-syntax-rule (abstract-atom-with-args symbol "(" arg ... ")")
    (sad:abstract-atom (string->symbol symbol) (odd-elems-as-list arg ...)))
  (define-syntax-rule (abstract-atom-without-args symbol)
    (sad:abstract-atom (string->symbol symbol) (list)))
  (define-syntax-rule (abstract-term specific-term) specific-term)
  (define-syntax-rule (abstract-variable specific-var) specific-var)
  (define-syntax-rule (abstract-variable-a "α" index) (sad:a (quote index)))
  (define-syntax-rule (abstract-variable-g "γ" index) (sad:g (quote index)))
  (define-syntax (abstract-function-term stx)
    (syntax-parse stx
      [(_ symbol:str) #'(sad:abstract-function (string->symbol symbol) '())]
      [(_ num-term) #'num-term]
      [(_ symbol "(" arg ... ")")
       #'(sad:abstract-function (string->symbol symbol) (odd-elems-as-list arg ...))]))
  (define-syntax-rule (number-term NUMBER)
    (sad:abstract-function (number->string (quote NUMBER)) '()))
  (define-syntax (abstract-lplist stx)
    (syntax-parse stx
      [(_ "[" "]")
       #'(sad:abstract-function 'nil '())]
      [(_ "[" term0 "]")
       #'(sad:abstract-function 'cons (list term0 (sad:abstract-function 'nil '())))]
      [(_ "[" term0 "," rest ... "]")
       #'(sad:abstract-function 'cons (list term0 (abstract-lplist "[" rest ... "]")))]
      [(_ "[" term0 "|" rest ... "]")
       #'(sad:abstract-function 'cons (list term0 rest ...))])))


; TODO zie 'Macro Testing' in Racket Reference
(define-syntax (preprior-pair stx)
  (syntax-parse stx
    [(_ atom1 "," atom2)
     #`'(before #,(abstract-domain-elem->sexp (eval-syntax #'atom1))
                #,(abstract-domain-elem->sexp (eval-syntax #'atom2)))]))
(provide preprior-pair)

; moeilijkheid: abstract-atom is niet zichtbaar voor de eval-syntax
; hoofdstuk 15 Racket Guide geeft aanwijzing, maar voor fase 0
; idealiter zou er een manier zijn om geneste syntax eerst uit te breiden zonder eval
; lijkt plausibel, maar hier zijn syntax objecten precies dat...
; kan voorbeeld 'eigen structs' uit Fear of Macros inspiratie bieden?
; daar is syntax-unsyntax-syntax aanwezig...
(preprior-pair
 (abstract-atom
  (abstract-atom-with-args "perm" "(" (abstract-term (abstract-variable (abstract-variable-g "γ" 1))) "," (abstract-term (abstract-variable (abstract-variable-a "α" 1))) ")"))
 ","
 (abstract-atom
  (abstract-atom-with-args "ord" "(" (abstract-term (abstract-variable (abstract-variable-a "α" 1))) ")")))

; AND THE GLUE TO GO TO TOP-LEVEL INTERACTION
; can we get the filename of the program being run? would be useful for serialization
(define #'(cclp-module-begin _PARSE-TREE ...)
  #'(#%module-begin (cclp-run current-contract-region _PARSE-TREE ...)))
(provide (rename-out [cclp-module-begin #%module-begin]) #%top-interaction)