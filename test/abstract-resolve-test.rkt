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
(require rackunit
         (prefix-in abp: "abstract-domain-boilerplate.rkt")
         "../src/abstract-resolve.rkt"
         "printed-test-results.rkt"
         "../src/abstract-knowledge.rkt"
         "../src/cclp-interpreter.rkt")
(require (for-syntax syntax/parse))
(require "../src/abstract-substitution.rkt")
(require "../src/abstract-multi-domain.rkt")

(define-syntax (aeq stx)
  (syntax-parse stx
    [((~literal aeq) (TERM1 TERM2))
     #'(abstract-equality TERM1 TERM2)]))

(define-syntax (asubst stx)
  (syntax-parse stx
    [(_ SUBST-PAIR ...)
     #'(list (aeq SUBST-PAIR) ...)]))

(check-equal?
 (abstract-resolve (interpret-abstract-conjunction "perm(γ1,α1),ord(α1)")
                   0
                   (list (interpret-concrete-rule "perm([],[])")
                         (interpret-concrete-rule "perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V)"))
                   (list)
                   (list))
 (list (resolvent (interpret-abstract-conjunction "del(α8,[γ8|γ9],α10),perm(α10,α9),ord([α8|α9])")
                  (asubst
                   ((a 6) (g 8))
                   ((a 7) (g 9))
                   ((g 1) (abstract-function 'cons (list (g 8) (g 9))))
                   ((a 1) (abstract-function 'cons (list (a 8) (a 9)))))
                  (interpret-concrete-rule "perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V)"))
       (resolvent (interpret-abstract-conjunction "ord(γ2)")
                  (asubst
                   ((g 1) (g 2))
                   ((a 1) (g 2)))
                  (interpret-concrete-rule "perm([],[])"))))

(let ([full-eval
       (full-evaluation (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                        (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)"))])
  (check-equal?
   (abstract-resolve
    (interpret-abstract-conjunction "del(α12,[γ18|γ19],α14),perm(α14,α13),ord([γ3,α12|α13])")
    0
    '()
    (list full-eval)
    (list))
   (list
    (resolvent
     (interpret-abstract-conjunction "perm(γ23,α13),ord([γ3,γ22|α13])")
     (asubst
      ((a 12) (g 22))
      ((g 18) (g 20))
      ((g 19) (g 21))
      ((a 14) (g 23)))
     full-eval))))

; TODO test whether single-step unfolding does not take place when full eval is applied