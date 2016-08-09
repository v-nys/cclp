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

#lang typed/racket
(require "abstract-substitution.rkt")
(require "data-utils.rkt")
(require "abstract-multi-domain.rkt")





(: abstract-unify (-> AbstractSubstitution (Opt AbstractSubstitution)))
(define (abstract-unify subst)
  (match subst
    [(list) (some (list))]
    [(list-rest (abstract-equality t1 t2) tail) #:when (equal? t1 t2) (abstract-unify tail)]
    [(list-rest (abstract-equality v t) tail) #:when (and (AbstractVariable? v) (occurs v t)) (none)]
    [(list-rest (abstract-equality (a i) t) tail) #:when (AbstractTerm? t) ; we can't unify a conjunct with a term
     (let ([substituted (substitute-in-substitution t (a i) tail)]
           [recursion (abstract-unify tail)])
       (cond [(none? recursion) recursion]
             [else (some (cons (abstract-equality (a i) t) (some-v recursion)))]))]
    [(list-rest (abstract-equality (abstract-atom sym1 args1) (abstract-atom sym2 args2)) tail)
     (if (and (equal? sym1 sym2) (equal? (length args1) (length args2)))
         (abstract-unify (append (for/list ([arg1 : AbstractTerm args1] [arg2 : AbstractTerm args2]) (abstract-equality arg1 arg2)) tail))
         (none))]
    [(list-rest (abstract-equality (abstract-function sym1 args1) (abstract-function sym2 args2)) tail)
     (if (and (equal? sym1 sym2) (equal? (length args1) (length args2)))
         (abstract-unify (append (for/list ([arg1 : AbstractTerm args1] [arg2 : AbstractTerm args2]) (abstract-equality arg1 arg2)) tail))
         (none))]
    [(list-rest (abstract-equality t var) tail) #:when (AbstractVariable? var) (abstract-unify (cons (abstract-equality var t) tail))]
    
    [else (none)]))
(provide abstract-unify)

(: occurs (-> AbstractVariable AbstractDomainElem Boolean))
(define (occurs avar aterm)
  (match aterm
    [(abstract-function symbol args) (ormap (λ ([elem : AbstractTerm]) (occurs avar elem)) args)]
    [(abstract-atom symbol args) (ormap (λ ([elem : AbstractTerm]) (occurs avar elem)) args)]
    [else (equal? avar aterm)]))
(provide occurs)