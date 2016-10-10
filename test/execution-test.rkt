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
(require rackunit)
(require "../src/execution.rkt")
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/fullai-domain.rkt")
(require "../src/abstract-knowledge.rkt")
(require parenlog)

(define looping-graph (abp:parse-prior-relation "a,b a,c b,a b,d c,b c,e d,e"))
(define non-looping-graph (abp:parse-prior-relation "a,b a,c b,d c,b c,e d,e"))
(define
  permsort-graph
  (abp:parse-prior-relation
   "perm(γ1,α1),ord(α1) perm(γ1,α1),ord([γ1|α1]) ord([γ1,γ2|α1]),perm(γ1,α1)"))
(define hypothetical-graph-for-consistency (abp:parse-prior-relation "foo(γ1,α1),bar(γ1,α1)"))

(check-true (is-valid? non-looping-graph))
(check-false (is-valid? looping-graph))
(check-true (is-valid? permsort-graph))
(check-true (is-valid? hypothetical-graph-for-consistency))

(check-equal?
 (selected-index
  (abp:parse-abstract-conjunction "c,b,d,a,e")
  non-looping-graph
  (list))
 3)

(check-equal?
 (selected-index
  (abp:parse-abstract-conjunction "bar(γ1,α1),foo(γ1,γ2)")
  hypothetical-graph-for-consistency
  (list))
 1)

(check-equal?
 (selected-index
  (abp:parse-abstract-conjunction "bar(α1,α2),foo(γ1,α2)")
  hypothetical-graph-for-consistency
  (list))
 1)

(check-equal?
 (selected-index
  (abp:parse-abstract-conjunction "bar(α1,α2),foo(γ1,α2),quux(γ3,γ4)")
  hypothetical-graph-for-consistency
  (list
   (full-evaluation
    (abp:parse-abstract-atom "quux(γ1,γ2)")
    (abp:parse-abstract-atom "quux(γ1,γ2)"))))
 2)

(test-case
 "bug trigger test for permutation sort"
 (check-equal?
  (query-model permsort-graph
               (member
                X
                (cons
                 (perm (γ sym41) (α sym13))
                 (cons
                  (ord (cons
                        (γ sym12)
                        (cons
                         (γ sym40)
                         (α sym13))))
                  ()))))
  (list (hasheq 'X '(perm (γ sym41) (α sym13)))
        (hasheq 'X '(ord (cons (γ sym12) (cons (γ sym40) (α sym13)))))))
 (check-equal?
  (query-model permsort-graph
               (before X Y))
               (list
                (hasheq 'X '(perm (γ sym1) (α sym1)) 'Y '(ord (α sym1)))
                (hasheq 'X '(perm (γ sym1) (α sym1)) 'Y '(ord (cons (γ sym1) (α sym1))))
                (hasheq 'X '(ord (cons (γ sym1) (cons (γ sym2) (α sym1)))) 'Y '(perm (γ sym1) (α sym1)))))
 (check-equal? (query-model
                permsort-graph
                (reaches_under_consistency
                 (ord (cons
                       (γ sym12)
                       (cons
                        (γ sym40)
                        (α sym13))))
                 (perm (γ sym41) (α sym13))))
               (list (hasheq)))
 (check-equal? (query-model
                permsort-graph
                (sexp_gt_extension
                 (ord (cons
                       (γ sym12)
                       (cons
                        (γ sym40)
                        (α sym13))))
                 (ord (cons
                       (γ sym12)
                       (cons
                        (γ sym40)
                        (α sym13))))))
               (list (hasheq)))
 (check-equal?
  (query-model permsort-graph
               (member_reaches_or_includes_all_under_consistency
                X
                (cons
                 (perm (γ sym41) (α sym13))
                 (cons
                  (ord (cons
                        (γ sym12)
                        (cons
                         (γ sym40)
                         (α sym13))))
                  ()))))
  (list (hasheq 'X '(ord (cons (γ sym12) (cons (γ sym40) (α sym13)))))))
 (check-equal?
  (selected-index
   (abp:parse-abstract-conjunction "perm(γ41,α13),ord([γ12,γ40|α13])")
   permsort-graph
   (list))
  1))