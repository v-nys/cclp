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

(define looping-graph (abp:parse-prior-relation "a,b b,c c,a"))
(define non-looping-graph (abp:parse-prior-relation "a,b a,c b,d c,b c,e d,e"))
(define
  permsort-graph
  (abp:parse-prior-relation
   "perm(γ1,α1),ord(α1) perm(γ1,α1),ord([γ1|α1]) ord([γ1,γ2|α1]),perm(γ1,α1)"))
(define hypothetical-graph-for-consistency (abp:parse-prior-relation "foo(γ1,α1),bar(γ1,α1)"))
(define
  simplified-primes-graph
  (abp:parse-prior-relation
   "integers(γ1,α1),filter(γ1,α1,α2) integers(γ1,α1),sift(α1,α2) integers(γ1,α1),length(α1,γ2) sift([γ1|α1],α2),integers(γ1,α1) filter(γ1,[γ2|α1],α2),integers(γ1,α1) length([γ1|α1],γ2),integers(γ1,α1)"))
(define
  full-primes-graph
  (abp:parse-prior-relation
   "integers(γ1,α1),filter(γ1,α1,α2) integers(γ1,α1),sift(α1,α2) integers(γ1,α1),length(α1,γ2) sift([γ1|α1],α2),integers(γ1,α1) sift(γ1,α1),length(α1,γ1) filter(γ1,[γ2|α1],α2),integers(γ1,α1) length([γ1|α1],γ2),integers(γ1,γ2) length(γ1,γ2),integers(γ1,[γ2|γ3])"))

; this passes at the time of writing, but it is too expensive
;(check-true (is-valid? non-looping-graph))
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
                (reaches_loopfree
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

(check-equal?
 (selected-index
  (abp:parse-abstract-conjunction
   "integers(γ22,α31),filter(γ4,α31,α32),filter(γ12,[γ20|α32],α24),sift(α24,α26),length(α26,γ18)")
  simplified-primes-graph
  (list))
 2)

(check-equal?
 (selected-index
  (abp:parse-abstract-conjunction
   "integers(γ6,α10),filter(γ4,α10,α12),sift(α12,α11),length([γ4|α11],γ1)")
  full-primes-graph
  (list))
 3)

(check-equal?
 (query-model
  full-primes-graph
  (member_reaches_or_includes_all_under_consistency
   X
   (cons (integers (γ sym6) (α sym10)) ())))
 (list (hasheq 'X '(integers (γ sym6) (α sym10)))))

(check-equal?
 (query-model
  full-primes-graph
  (member_reaches_or_includes_all_under_consistency
   X
   (cons (integers (γ sym6) (α sym10))
         (cons (filter (γ sym4) (α sym10) (α sym12)) ()))))
 (list (hasheq 'X '(integers (γ sym6) (α sym10))))
 "integers explicitly precedes filter")

(check-equal?
 (query-model
  full-primes-graph
  (member_reaches_or_includes_all_under_consistency
   X
   (cons (integers (γ sym6) (α sym10))
         (cons (filter (γ sym4) (α sym10) (α sym12))
               (cons (sift (α sym12) (α sym11)) ())))))
 (list (hasheq 'X '(integers (γ sym6) (α sym10)))))

(check-equal?
 (query-model
  full-primes-graph
  (member_reaches_or_includes_all_under_consistency
   X
   (cons (integers (γ sym6) (α sym10))
        (cons (filter (γ sym4) (α sym10) (α sym12))
              (cons (sift (α sym12) (α sym11))
                    (cons (length (cons (γ sym4) (α sym11)) (γ sym1))
                          ()))))))
 (list (hasheq 'X '(length (cons (γ sym4) (α sym11)) (γ sym1))))
 "length, by transitivity, precedes all of these atoms")

(check-equal?
 (query-model
  full-primes-graph
  (member_reaches_or_includes_all_under_consistency
   X
   (cons (integers (γ sym6) (α sym10))
         (cons (length (cons (γ sym4) (α sym11)) (γ sym1)) ()))))
 (list (hasheq 'X '(length (cons (γ sym4) (α sym11)) (γ sym1))))
 "length explicitly precedes more general instance of integers")

(check-equal?
 (query-model
  full-primes-graph
  (member_reaches_or_includes_all_under_consistency
   X
   (cons (filter (γ sym4) (α sym10) (α sym12))
         (cons (length (cons (γ sym4) (α sym11)) (γ sym1)) ()))))
 (list (hasheq 'X '(length (cons (γ sym4) (α sym11)) (γ sym1))))
 "length explicitly precedes integers, which explicitly precedes filter")

(check-equal?
 (query-model
  full-primes-graph
  (member_reaches_or_includes_all_under_consistency
   X
   (cons (sift (α sym12) (α sym11))
         (cons (length (cons (γ sym4) (α sym11)) (γ sym1)) ()))))
 (list (hasheq 'X '(length (cons (γ sym4) (α sym11)) (γ sym1))))
 "length explicitly precedes integers, which precedes sift")

(check-equal?
 (query-model
  full-primes-graph
  (reaches_loopfree
   (length (cons (γ sym4) (α sym11)) (γ sym1))
   (sift (α sym12) (α sym11))))
 (list (hasheq))
 "length does not specify sift, but reaches it")

(check-equal?
 (query-model
  full-primes-graph
  (reaches_loopfree
   (length (cons (γ sym1) (α sym1)) (γ sym2))
   (sift (α sym1) (α sym2))))
 (list (hasheq))
 "normalized representation")