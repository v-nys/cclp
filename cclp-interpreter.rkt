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

#lang at-exp racket

(require syntax/parse)

(require (only-in "cclp-parser.rkt" make-rule-parser))
(require (only-in "cclp-reader.rkt" all-tokens))
(require (prefix-in ad: "abstract-multi-domain.rkt"))
(require (prefix-in faid: "fullai-domain.rkt"))
(require (prefix-in cd: "concrete-domain.rkt")
         (prefix-in ck: "concrete-knowledge.rkt"))
(require (only-in list-utils odd-elems))
(require "abstract-substitution.rkt")
(require (only-in sugar/coerce ->symbol))

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(module+ test
  (require rackunit))

(define (interpret-concrete-rule str)
  (define (interpret-concrete-rule-syntax rule-stx)
    (syntax-parse rule-stx
      [((~literal rule) ATOM) (ck:rule (interpret-concrete-atom-syntax #'ATOM) (list))]
      [((~literal rule) ATOM ":-" CONJUNCTION)
       (ck:rule
        (interpret-concrete-atom-syntax #'ATOM)
        (interpret-concrete-conjunction-syntax #'CONJUNCTION))]))
  (define (interpret-concrete-conjunction-syntax con-stx)
    (syntax-parse con-stx
      [((~literal conjunction) ATOM0 ATOM-OR-COMMA ...)
       (map interpret-concrete-atom-syntax (odd-elems (syntax->list #'(ATOM0 ATOM-OR-COMMA ...))))]))
  (define (interpret-concrete-atom-syntax stx)
    (syntax-parse stx
      [((~literal atom) SYMBOL)
       (cd:atom (string->symbol (syntax->datum #'SYMBOL)) (list))]
      [((~literal atom) SYMBOL "(" ARG-OR-SEP ... ")")
       (cd:atom
        (string->symbol (syntax->datum #'SYMBOL))
        (map interpret-concrete-term-syntax (odd-elems (syntax->list #'(ARG-OR-SEP ...)))))]))
  (define (interpret-concrete-term-syntax term-stx)
    (syntax-parse term-stx
      [((~literal term) ((~literal variable) NESTED-VAR))
       (interpret-concrete-variable-syntax #'NESTED-VAR)]
      [((~literal term) ((~literal function-term) ((~literal number-term) NUM)))
       (cd:function (->symbol (syntax->datum #'NUM)) '())]
      [((~literal term) ((~literal function-term) FUNCTOR))
       (cd:function (string->symbol (syntax->datum #'FUNCTOR)) '())]
      [((~literal term) ((~literal function-term) FUNCTOR "(" ARG-OR-SEP ... ")"))
       (cd:function
        (string->symbol (syntax->datum #'FUNCTOR))
        (map interpret-concrete-term-syntax (odd-elems (syntax->list #'(ARG-OR-SEP ...)))))]
      [((~literal term) ((~literal lplist) "[" "]"))
       (cd:function 'nil '())]
      [((~literal term) ((~literal lplist) "[" TERM "]"))
       (cd:function
        'cons
        (list (interpret-concrete-term-syntax #'TERM) (cd:function 'nil '())))]
      [((~literal term) ((~literal lplist) "[" TERM0 "," REST ... "]"))
       (cd:function
        'cons
        (list
         (interpret-concrete-term-syntax #'TERM0)
         (interpret-concrete-term-syntax #'(term (lplist "[" REST ... "]")))))]
      [((~literal term) ((~literal lplist) "[" TERM0 "|" REST "]"))
       (cd:function
        'cons
        (list (interpret-concrete-term-syntax #'TERM0)
              (interpret-concrete-term-syntax #'(term REST))))]))
  (define (interpret-concrete-variable-syntax stx)
    (syntax-parse stx
      [VAR-ID (cd:variable (string->symbol (syntax->datum #'VAR-ID)))]))
  (define rule-parse (make-rule-parser rule))
  (define parsed (rule-parse (all-tokens str)))
  (interpret-concrete-rule-syntax parsed))
(module+ test
  (check-equal?
   (interpret-concrete-rule "perm([X|Y],[U|V]) :- del(U,[X|Y],W), perm(W,V)")
   (ck:rule
    (cd:atom
     'perm
     (list
      (cd:function 'cons (list (cd:variable 'X) (cd:variable 'Y)))
      (cd:function 'cons (list (cd:variable 'U) (cd:variable 'V)))))
    (list
     (cd:atom
      'del
      (list
       (cd:variable 'U)
       (cd:function 'cons (list (cd:variable 'X) (cd:variable 'Y)))
       (cd:variable 'W)))
     (cd:atom 'perm (list (cd:variable 'W) (cd:variable 'V)))))))
(provide
 (proc-doc/names
  interpret-concrete-rule
  (-> string? ck:rule?)
  (str)
  @{Interprets a string @racket[str] as a @racket[ck:rule?],
 i.e. a rule in the concrete domain.}))

(define (interpret-abstract-variable-syntax var-stx)
  (syntax-parse var-stx
    [((~literal abstract-variable-a) A-SYMBOL A-INDEX)
     (ad:a (syntax->datum #'A-INDEX))]
    [((~literal abstract-variable-g) G-SYMBOL G-INDEX)
     (ad:g (syntax->datum #'G-INDEX))]))
(define (interpret-abstract-term str)
  (define term-parse (make-rule-parser abstract-term))
  (interpret-abstract-term-syntax (term-parse (all-tokens str))))
(define (interpret-abstract-term-syntax term-stx)
  (syntax-parse term-stx
    [((~literal abstract-term) ((~literal abstract-variable) NESTED-VAR))
     (interpret-abstract-variable-syntax #'NESTED-VAR)]
    [((~literal abstract-term) ((~literal abstract-function-term)
                                ((~literal abstract-number-term)
                                 ((~literal abstract-number) NUM))))
     (ad:abstract-function (syntax->datum #'NUM) '())]
    [((~literal abstract-term) ((~literal abstract-function-term) FUNCTOR))
     (ad:abstract-function (string->symbol (syntax->datum #'FUNCTOR)) '())]
    [((~literal abstract-term) ((~literal abstract-function-term) FUNCTOR "(" ARG-OR-SEP ... ")"))
     (ad:abstract-function
      (string->symbol (syntax->datum #'FUNCTOR))
      (map interpret-abstract-term-syntax (odd-elems (syntax->list #'(ARG-OR-SEP ...)))))]
    [((~literal abstract-term) ((~literal abstract-lplist) "[" "]"))
     (ad:abstract-function 'nil '())]
    [((~literal abstract-term) ((~literal abstract-lplist) "[" TERM "]"))
     (ad:abstract-function
      'cons
      (list (interpret-abstract-term-syntax #'TERM) (ad:abstract-function 'nil '())))]

    [((~literal abstract-term) ((~literal abstract-lplist) "[" TERM0 "," REST ... "]"))
     (ad:abstract-function
      'cons
      (list
       (interpret-abstract-term-syntax #'TERM0)
       (interpret-abstract-term-syntax #'(abstract-term (abstract-lplist "[" REST ... "]")))))]
    [((~literal abstract-term) ((~literal abstract-lplist) "[" TERM0 "|" REST "]"))
     (ad:abstract-function
      'cons
      (list (interpret-abstract-term-syntax #'TERM0)
            (interpret-abstract-term-syntax #'(abstract-term REST))))]))
(provide
 (proc-doc/names
  interpret-abstract-term
  (-> string? ad:abstract-term?)
  (str)
  @{Interpret an abstract term from a string @racket[str] (in Prolog-style notation, but with abstract variables) to a data structure for the control compiler.}))
(module+ test
  (check-equal?
   (interpret-abstract-term "foo(g1)")
   (ad:abstract-function 'foo (list (ad:g 1))))
  (check-equal?
   (interpret-abstract-term "[]")
   (ad:abstract-function 'nil (list))))

(define (interpret-abstract-atom str)
  (define atom-parse (make-rule-parser abstract-atom))
  (define parsed (atom-parse (all-tokens str)))
  (interpret-abstract-atom-syntax parsed))
(define (interpret-abstract-atom-syntax atom-stx)
  (syntax-parse atom-stx
    [((~literal abstract-atom) ((~literal abstract-atom-without-args) SYMBOL))
     (ad:abstract-atom (string->symbol (syntax->datum #'SYMBOL)) (list))]
    [((~literal abstract-atom) ((~literal abstract-atom-with-args) SYMBOL "(" ARG-OR-SEP ... ")"))
     (ad:abstract-atom
      (string->symbol (syntax->datum #'SYMBOL))
      (map interpret-abstract-term-syntax (odd-elems (syntax->list #'(ARG-OR-SEP ...)))))]))
(module+ test
  (check-equal?
   (interpret-abstract-atom "safe")
   (ad:abstract-atom 'safe (list)))
  (check-equal?
   (interpret-abstract-atom "safe([g1])")
   (ad:abstract-atom
    'safe
    (list (ad:abstract-function 'cons (list (ad:g 1) (ad:abstract-function 'nil (list)))))))
  (check-equal?
   (interpret-abstract-atom "safe([g1,g2])")
   (ad:abstract-atom
    'safe
    (list
     (ad:abstract-function
      'cons
      (list (ad:g 1) (ad:abstract-function 'cons (list (ad:g 2) (ad:abstract-function 'nil (list)))))))))
  (check-equal?
   (interpret-abstract-atom "safe([g1,g2|a1])")
   (ad:abstract-atom
    'safe
    (list (ad:abstract-function 'cons (list (ad:g 1) (ad:abstract-function 'cons (list (ad:g 2) (ad:a 1))))))))
  (check-equal?
   (interpret-abstract-atom "safe([g1,g2|a1],g3)")
   (ad:abstract-atom
    'safe
    (list
     (ad:abstract-function
      'cons
      (list (ad:g 1) (ad:abstract-function 'cons (list (ad:g 2) (ad:a 1)))))
     (ad:g 3)))))
(provide
 (proc-doc/names
  interpret-abstract-atom
  (-> string? ad:abstract-atom?)
  (str)
  @{Interpret an abstract atom from a string @racket[str] (in Prolog-style notation, but with abstract variables) to a data structure for the control compiler.}))

(define (interpret-abstract-conjunction str)
  (define (interpret-abstract-conjunction-syntax con-stx)
    (syntax-parse con-stx
      [((~literal abstract-conjunction) ATOM0 ATOM-OR-COMMA ...)
       (map interpret-abstract-atom-syntax (odd-elems (syntax->list #'(ATOM0 ATOM-OR-COMMA ...))))]))
  (define conjunction-parse (make-rule-parser abstract-conjunction))
  (define parsed (conjunction-parse (all-tokens str)))
  (interpret-abstract-conjunction-syntax parsed))
(module+ test
  (check-equal?
   (interpret-abstract-conjunction "safe([g1,g2|a1]),perm(g1,a1)")
   (list
    (interpret-abstract-atom "safe([g1,g2|a1])")
    (interpret-abstract-atom "perm(g1,a1)"))))
(provide interpret-abstract-conjunction)
  
(define (interpret-abstract-substitution-syntax sub-stx)
  (syntax-parse sub-stx
    [((~literal abstract-substitution) SUBST-PAIR)
     (list (interpret-abstract-substitution-pair-syntax #'SUBST-PAIR))]
    [((~literal abstract-substitution) SUBST-PAIR "," COMMA-OR-SUBST-PAIR ...)
     (cons (interpret-abstract-substitution-pair-syntax #'SUBST-PAIR)
           (interpret-abstract-substitution-syntax
            #'(abstract-substitution COMMA-OR-SUBST-PAIR ...)))]))
(define (interpret-abstract-substitution-pair-syntax pair-stx)
  (syntax-parse pair-stx
    [((~literal abstract-substitution-pair)
      ((~literal abstract-variable) AVAR-STX) "/" ATERM-STX)
     (abstract-equality
      (interpret-abstract-variable-syntax #'AVAR-STX)
      (interpret-abstract-term-syntax #'ATERM-STX))]))
(define (interpret-abstract-substitution str)
  (define subst-parse (make-rule-parser abstract-substitution))
  (define parsed (subst-parse (all-tokens str)))
  (interpret-abstract-substitution-syntax parsed))
(module+ test
  (check-equal?
   (interpret-abstract-substitution "a1/g3,a2/g4")
   (list
    (abstract-equality (ad:a 1) (ad:g 3))
    (abstract-equality (ad:a 2) (ad:g 4)))))

(define (interpret-full-eval-section str)
  (define (interpret-full-eval-section-syntax sec-stx)
    (syntax-parse sec-stx
      [((~literal full-evaluation-section) FULL-EVAL-RULE ...+)
       (map interpret-full-eval-stx (syntax->list #'(FULL-EVAL-RULE ...)))]))
  (define (interpret-full-eval-stx rule-stx)
    (syntax-parse rule-stx
      [((~literal fullai-rule-with-body) ATOM-STX "->" SUB-STX ".")
       (faid:full-ai-rule
        (interpret-abstract-atom-syntax #'(abstract-atom ATOM-STX))
        (interpret-abstract-substitution-syntax #'SUB-STX))]
      [((~literal fullai-rule-without-body) ATOM-STX".")
       (faid:full-ai-rule
        (interpret-abstract-atom-syntax #'(abstract-atom ATOM-STX))
        (list))]))
  (define section-parse (make-rule-parser full-evaluation-section))
  (define parsed (section-parse (all-tokens str)))
  (interpret-full-eval-section-syntax parsed))
(module+ test
  (check-equal?
   (interpret-full-eval-section
    #<<HERE
lte(g1,g2).
del(a1,[g1|g2],a2) -> a1/g3,a2/g4.
HERE
    )
   (list
    (faid:full-ai-rule (interpret-abstract-atom "lte(g1,g2)") (list))
    (faid:full-ai-rule
     (interpret-abstract-atom "del(a1,[g1|g2],a2)")
     (interpret-abstract-substitution "a1/g3,a2/g4")))))
(provide
 (proc-doc/names
  interpret-full-eval-section
  (-> string? (listof faid:full-ai-rule?))
  (str)
  @{Interprets a full evaluation section, without section delimiter,
 as a list of @racket[faid:full-ai-rule?] structs.}))