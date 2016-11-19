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
(require (only-in "interaction.rkt" cclp-top))
(require racket/contract)
(require (only-in "data-utils.rkt" 5-tuple))
(require (for-syntax (only-in racket-list-utils/utils odd-elems)))
(require (for-syntax (only-in "data-utils.rkt" positive-integer->symbol)))
(require (for-syntax (only-in racket remove-duplicates match second third)))

(require "abstract-domain-ordering.rkt")

(require (for-syntax "abstract-multi-domain-sexp-conversion.rkt"))
(require "abstract-multi-domain-sexp-conversion.rkt")
(require parenlog)

; PUTTING THE THREE PARTS TOGETHER

; can I make this more modular?
(define-syntax (cclp-program stx)
  (syntax-parse stx
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION (list) (list) (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION _FULL-EVALUATION-SECTION (list) (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{PREPRIOR}" _PREPRIOR-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION (list) _PREPRIOR-SECTION (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{PREPRIOR}" _PREPRIOR-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION _FULL-EVALUATION-SECTION _PREPRIOR-SECTION (list) _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION (list) (list) _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION _FULL-EVALUATION-SECTION (list) _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{PREPRIOR}" _PREPRIOR-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION (list) _PREPRIOR-SECTION _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION)]
    [(_ "{PROGRAM}" _PROGRAM-SECTION "{FULL EVALUATION}"
        _FULL-EVALUATION-SECTION "{PREPRIOR}" _PREPRIOR-SECTION "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION "{QUERY}" _QUERY-SECTION)
     #'(5-tuple _PROGRAM-SECTION _FULL-EVALUATION-SECTION _PREPRIOR-SECTION _CONCRETE-CONSTANTS-SECTION _QUERY-SECTION)]))
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

; PART RELATED TO PREPRIOR

; careful! this is syntax such that the atoms can be *converted* to S-expressions
; the parsed forms are not what we want!
; use expand-syntax-while-bound instead of syntax->datum!
(define-for-syntax (extract-relevant-atom-stx pairs-syntax)
  (define pair-syntaxes (syntax->list pairs-syntax))
  (define (syntaxes->atom-datums lst acc)
    (match lst
      [(list) (list)]
      [(list-rest h t)
       (let ([d1 (syntax->datum (second (syntax->list h)))]
             [d2 (syntax->datum (third (syntax->list h)))])
         (syntaxes->atom-datums t (cons d1 (cons d2 acc))))]))
  (define as-datums (remove-duplicates (syntaxes->atom-datums pair-syntaxes '())))
  (define as-single-datum ('relevant_atoms (foldr (λ (elem acc) ('cons elem acc)) '() as-datums)))
  (datum->syntax pairs-syntax as-single-datum))

(define-syntax (preprior-section stx)
  (begin
    (syntax-parse stx
      [(_ pair ...)
       (with-syntax
           ([(expanded-pair ...)
             (datum->syntax
              #'(pair ...)
              (map expand-syntax-while-bound (syntax->list #'(pair ...))))]
            [relevant-atoms-stx (extract-relevant-atom-stx #'(pair ...))])
            ; placeholder to check if the approach works
            ;[relevant-atoms-stx #'(relevant_atoms (cons (collect (γ sym1) (α sym1)) (cons (eq (α sym1) (α sym2)) ())))])
         #`((λ () (define-model prior
                    expanded-pair ...
                    relevant-atoms-stx
                    (member X (cons X Y))
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
                                                   (sexp->abstract-atom e2))) X Y)))
              prior)))])))
(provide preprior-section)

; REVIEW: is dit nog nodig? rechtstreekse expansie gebeurt nooit.
(define-syntax (preprior-pair stx)
  (syntax-parse stx
    [(_ atom1 "," atom2)
     (with-syntax ([before (datum->syntax #'() 'before)])
       #`(before atom1 atom2))]))
(provide preprior-pair)

; REVIEW: is dit nog nodig? rechtstreekse expansie gebeurt nooit.
(define-syntax (sexp-abstract-atom stx)
  (syntax-parse stx
    [(_ actual-atom) #'actual-atom]))
(provide sexp-abstract-atom)

; REVIEW: is dit nog nodig? rechtstreekse expansie gebeurt nooit.
(define-syntax (sexp-abstract-atom-without-args stx)
  (syntax-parse stx
    [(_ sym:str)
     (with-syntax ([sym-sym (datum->syntax #'() (string->symbol (syntax->datum #'sym)))])
       #'(sym-sym))]))
(provide sexp-abstract-atom-without-args)

; REVIEW: is dit nog nodig? rechtstreekse expansie gebeurt nooit.
(define-syntax (sexp-abstract-variable stx)
  (syntax-parse stx
    [(_ real-var) #'real-var]))
(provide sexp-abstract-variable)

; REVIEW: is dit nog nodig? rechtstreekse expansie gebeurt nooit.
(define-syntax (sexp-abstract-variable-a stx)
  (syntax-parse stx
    [(_ "α" index)
     (with-syntax
         ([sym-stx (datum->syntax
                    #'index
                    (string->symbol (string-append "sym" (number->string (syntax->datum #'index)))))])
       #''(α sym-stx))]))
(provide sexp-abstract-variable-a)

(define-syntax (sexp-abstract-variable-g stx)
  (syntax-parse stx
    [(_ "γ" index)
     (with-syntax
         ([sym-stx (datum->syntax
                    #'index
                    (string->symbol (string-append "sym" (number->string (syntax->datum #'index)))))])
       #''(γ sym-stx))]))
(provide sexp-abstract-variable-g)

(define-syntax (sexp-abstract-number stx)
  (syntax-parse stx
    [(_ num:nat) #'num]))
(provide sexp-abstract-number)

(define-syntax (sexp-abstract-number-term stx)
  (syntax-parse stx
    [(_ num) #'num]))
(provide sexp-abstract-number-term)

(define-syntax (sexp-abstract-term stx)
  (syntax-parse stx
    [(_ contents) #'contents]))
(provide sexp-abstract-term)

(define-syntax (sexp-abstract-atom-with-args stx)
  (syntax-parse stx
    [(_ sym:str) (with-syntax ([sym-sym (string->symbol (syntax->datum #'sym))]) #''(sym-sym))]
    [(_ symbol:str "(" arg ... ")")
     (with-syntax ([sym-sym (string->symbol (syntax->datum #'symbol))])
       #'(cons 'sym-sym (odd-elems-as-list arg ...)))]))
(provide sexp-abstract-atom-with-args)

(define-syntax (sexp-abstract-function-term stx)
  (syntax-parse stx
    [(_ sym:str) (with-syntax ([sym-sym (string->symbol (syntax->datum #'sym))]) #''(sym-sym))]
    [(_ symbol:str "(" arg ... ")")
     (with-syntax ([sym-sym (string->symbol (syntax->datum #'symbol))])
       #'(cons 'sym-sym (odd-elems-as-list arg ...)))]
    [(_ num-term) #'num-term]))
(provide sexp-abstract-function-term)

(define-syntax (sexp-abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     #'(nil)]
    [(_ "[" term0 "]")
     #'(list 'cons term0 (nil))]
    [(_ "[" term0 "," rest ... "]")
     #'(list 'cons term0 (sexp-abstract-lplist "[" rest ... "]"))]
    [(_ "[" term0 "|" rest "]")
     #'(list 'cons term0 rest)]))
(provide sexp-abstract-lplist)



; expand-syntax expands preprior-pairs too far
; it tries to expand the resulting unbound identifiers (e.g. the identifier before)
; therefore, we need an expansion mechanism which stops just in time
(define-for-syntax (expand-syntax-while-bound stx)
  (syntax-case stx (preprior-pair
                    sexp-abstract-atom
                    sexp-abstract-atom-without-args
                    sexp-abstract-atom-with-args
                    sexp-abstract-term
                    sexp-abstract-number
                    sexp-abstract-lplist
                    sexp-abstract-number-term
                    sexp-abstract-number
                    sexp-abstract-function-term
                    sexp-abstract-lplist)
    [(preprior-pair atom1 "," atom2)
     (with-syntax ([before (datum->syntax #'() 'before)]
                   [exp-atom1 (expand-syntax-while-bound #'atom1)]
                   [exp-atom2 (expand-syntax-while-bound #'atom2)])
       #'(before exp-atom1 exp-atom2))]
    [(sexp-abstract-atom actual-atom)
     (expand-syntax-while-bound #'actual-atom)]
    [(sexp-abstract-atom-without-args atom-sym)
     (with-syntax ([sym (datum->syntax #'() (string->symbol (syntax->datum #'atom-sym)))])
       #'(sym))]
    [(sexp-abstract-lplist "[" "]")
     #'(nil)]
    [(sexp-abstract-lplist "[" term0 "]")
     (with-syntax ([expanded-term0 (expand-syntax-while-bound #'term0)])
       #'(cons expanded-term0 (nil)))]
    [(sexp-abstract-lplist "[" term0 "," rest ... "]")
     (with-syntax ([expanded-term0 (expand-syntax-while-bound #'term0)]
                   [expanded-rest (expand-syntax-while-bound #'(sexp-abstract-lplist "[" rest ... "]"))])
       #'(cons expanded-term0 expanded-rest))]
    [(sexp-abstract-lplist "[" term0 "|" rest "]")
     (with-syntax ([expanded-term0 (expand-syntax-while-bound #'term0)]
                   [expanded-rest (expand-syntax-while-bound #'(sexp-abstract-term rest))])
       #'(cons expanded-term0 expanded-rest))]
    [(sexp-abstract-function-term (sexp-abstract-number-term num))
     #'(sexp-abstract-number-term num)]
    [(sexp-abstract-function-term func-sym)
     (with-syntax ([sym (datum->syntax #'() (string->symbol (syntax->datum #'func-sym)))])
       #`(sym))]
    [(sexp-abstract-function-term func-sym "(" arg ... ")")
     (let ([splicable-list (map expand-syntax-while-bound (odd-elems (syntax->list #'(arg ...))))])
       (begin
         (with-syntax ([sym (datum->syntax #'() (string->symbol (syntax->datum #'func-sym)))])
           #`(sym #,@splicable-list))))]
    [(sexp-abstract-number-term actual-number)
     #'actual-number]
    [(sexp-abstract-number num)
     (with-syntax ([num-sym (datum->syntax #'num (positive-integer->symbol (syntax->datum #'num)))])
       #'(num-sym))]
    [(sexp-abstract-term actual-term)
     (expand-syntax-while-bound #'actual-term)]
    [(sexp-abstract-atom-with-args atom-sym "(" arg ... ")")
     (let ([splicable-list (map expand-syntax-while-bound (odd-elems (syntax->list #'(arg ...))))])
       (begin
         (with-syntax ([sym (datum->syntax #'() (string->symbol (syntax->datum #'atom-sym)))])
           #`(sym #,@splicable-list))))]
    [(sexp-abstract-variable actual-variable)
     (expand-syntax-while-bound #'actual-variable)]
    [(sexp-abstract-variable-a "α" index)
     (with-syntax ([alpha-symbol (datum->syntax #'() 'α)]
                   [index-symbol (datum->syntax #'index (positive-integer->symbol (syntax->datum #'index)))])
       #'(alpha-symbol index-symbol))]
    [(sexp-abstract-variable-g "γ" index)
     (with-syntax ([gamma-symbol (datum->syntax #'() 'γ)]
                   [index-symbol (datum->syntax #'index (positive-integer->symbol (syntax->datum #'index)))])
       #'(gamma-symbol index-symbol))]))

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
; can we get the filename of the program being run? would be useful for serialization
(define #'(cclp-module-begin _PARSE-TREE ...)
  #'(#%module-begin (cclp-top current-contract-region _PARSE-TREE ...)))
(provide (rename-out [cclp-module-begin #%module-begin]) #%top-interaction)