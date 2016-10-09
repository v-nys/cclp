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
         "../src/abstract-knowledge.rkt")

(readable-check-equal?
 (abstract-resolve (abp:parse-abstract-conjunction "perm(γ1,α1),ord(α1)")
                   (abp:parse-prior-relation "perm(γ1,α1),ord(α1)")
                   (list (cbp:parse-rule "perm([],[])")
                         (cbp:parse-rule "perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V)"))
                   (list))
 (cons
  0
  (list (resolvent (abp:parse-abstract-conjunction "del(α8,[γ8|γ9],α10),perm(α10,α9),ord([α8|α9])")
                   (abp:parse-abstract-substitution "α6/γ8,α7/γ9,γ1/[γ8|γ9],α1/[α8|α9]")
                   (cbp:parse-rule "perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V)"))
        (resolvent (abp:parse-abstract-conjunction "ord(γ2)")
                   (abp:parse-abstract-substitution "γ1/γ2,α1/γ2")
                   (cbp:parse-rule "perm([],[])")))))

; bug trigger test for problem with permutation sort
(let ([full-eval
       (full-evaluation (abp:parse-abstract-atom "del(α1,[γ1|γ2],α2)")
                        (abp:parse-abstract-atom "del(γ3,[γ1|γ2],γ4)"))])
  (readable-check-equal?
   (abstract-resolve
    (abp:parse-abstract-conjunction "del(α12,[γ18|γ19],α14),perm(α14,α13),ord([γ3,α12|α13])")
    (abp:parse-prior-relation "perm(γ1,α1),ord(α1)")
    '()
    (list full-eval))
   (cons
    0
    (list (resolvent (abp:parse-abstract-conjunction "perm(γ4,α13),ord([γ3,γ6|α13])") (list) full-eval)))))