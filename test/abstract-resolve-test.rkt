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
         (prefix-in cbp: "concrete-domain-boilerplate.rkt")
         "../src/abstract-resolve.rkt"
         "printed-test-results.rkt"
         "../src/abstract-knowledge.rkt"
         "../src/cclp-interpreter.rkt")

(check-equal?
 (abstract-resolve (interpret-abstract-conjunction "perm(γ1,α1),ord(α1)")
                   0
                   (list (cbp:parse-rule "perm([],[])")
                         (cbp:parse-rule "perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V)"))
                   (list)
                   (list))
 (list (resolvent (interpret-abstract-conjunction "del(α8,[γ8|γ9],α10),perm(α10,α9),ord([α8|α9])")
                  (abp:parse-abstract-substitution "α6/γ8,α7/γ9,γ1/[γ8|γ9],α1/[α8|α9]")
                  (cbp:parse-rule "perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V)"))
       (resolvent (interpret-abstract-conjunction "ord(γ2)")
                  (abp:parse-abstract-substitution "γ1/γ2,α1/γ2")
                  (cbp:parse-rule "perm([],[])"))))

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
     (abp:parse-abstract-substitution "α12/γ22,γ18/γ20,γ19/γ21,α14/γ23")
     full-eval))))

; TODO test whether single-step unfolding does not take place when full eval is applied