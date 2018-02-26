; MIT License
;
; Copyright (c) 2017 Vincent Nys
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
(require
  scribble/srcdoc
  cclp-analysis/concrete-domain
  cclp-analysis/concrete-substitution)
(require (for-doc scribble/manual))

(define (concrete-unify concrete-subst)
  (define (occurs var term)
    (match term
      [(or (function symbol args) (atom symbol args))
       (ormap (λ (elem) (occurs var elem)) args)]
      [else (equal? var term)]))
  (match concrete-subst
    [(list) (list)]
    [(list-rest (concrete-equality h1a h1a) t1) (concrete-unify t1)]
    [(or (list-rest (concrete-equality (atom sym args1) (atom sym args2)) t)
         (list-rest (concrete-equality (function sym args1) (function sym args2)) t))
     #:when (equal? (length args1) (length args2))
     (concrete-unify
      (append
       (map
        (λ (a1 a2) (concrete-equality a1 a2))
        args1
        args2)
       t))]
    [(list-rest (concrete-equality (and (? list?) list1) (and (? list?) list2)) t)
     #:when (equal? (length list1) (length list2))
     (concrete-unify
      (append
       (map
        (λ (e1 e2) (concrete-equality e1 e2))
        list1
        list2)
       t))]
    [(list-rest (concrete-equality (and (? function?) f) (and (? variable?) v)) t)
     (concrete-unify (cons (concrete-equality v f) t))]
    [(list-rest (concrete-equality (and (? variable?) v) (and (? term?) f)) t)
     #:when (not (occurs v f))
     (let ([rec-unification
            (concrete-unify
             (substitute v f t))])
       (and rec-unification
            (cons (concrete-equality v f) rec-unification)))]
    [else #f]))
(module+ test
  (require rackunit)
  (check-equal?
   (concrete-unify empty)
   empty)
  (check-equal?
   (concrete-unify
    (list
     (concrete-equality
      (function 't empty)
      (function 't empty))))
   empty)
  (check-equal?
   (concrete-unify
    (list
     (concrete-equality
      (atom 'ancestor (list (variable 'A) (function 'a empty)))
      (atom 'ancestor (list (function 'b empty) (variable 'B))))))
   (list
    (concrete-equality (variable 'A) (function 'b empty))
    (concrete-equality (variable 'B) (function 'a empty))))
  (check-equal?
   (concrete-unify
    (list
     (concrete-equality
      (list (variable 'A) (function 'b empty))
      (list (function 'b empty) (function 'b empty)))))
   (list (concrete-equality (variable 'A) (function 'b empty))))
  (check-equal?
   (concrete-unify
    (list
     (concrete-equality
      (list (variable 'A) (function 'c empty))
      (list (function 'b empty) (function 'b empty)))))
   #f)
  (check-equal?
   (concrete-unify
    (list
     (concrete-equality
      (list (variable 'A) (function 'b empty))
      (list (function 'b empty) (variable 'A)))))
   (list (concrete-equality (variable 'A) (function 'b empty))))
  (check-equal?
   (concrete-unify
    (list
     (concrete-equality
      (atom
       'pred
       (list
        (variable 'A)
        (function 'b empty)
        (variable 'C)
        (variable 'D)
        (variable 'E)
        (variable 'G)))
      (atom
       'pred
       (list
        (function 'b empty)
        (variable 'A)
        (function 'f empty)
        (function 'f empty)
        (variable 'G)
        (function 'h empty))))))
   (list
    (concrete-equality (variable 'A) (function 'b empty))
    (concrete-equality (variable 'C) (function 'f empty))
    (concrete-equality (variable 'D) (function 'f empty))
    (concrete-equality (variable 'E) (variable 'G))
    (concrete-equality (variable 'G) (function 'h empty))))
  (check-equal?
   (concrete-unify
    (list
     (concrete-equality
      (variable 'A)
      (function
       'f
       (list (variable 'A))))))
   #f))
(provide
 (proc-doc/names
  concrete-unify
  (->
   (listof concrete-equality?)
   (or/c #f (listof concrete-equality?)))
  (concrete-subst)
  @{Applies the Martelli-Montanari algorithm for concrete unification.}))