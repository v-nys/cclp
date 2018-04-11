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
  cclp-common-data/concrete-domain
  cclp-common-data/concrete-knowledge
  cclp-analysis/concrete-substitution
  cclp-analysis/concrete-unify)
(require (for-doc scribble/manual))

(struct resolvent (conjunction substitution)
  #:methods
  gen:equal+hash
  [(define (equal-proc r1 r2 equal?-recur)
     (and (equal?-recur (resolvent-conjunction r1) (resolvent-conjunction r2))
          (equal?-recur (resolvent-substitution r1) (resolvent-substitution r2))))
   (define (hash-proc my-r hash-recur)
     (+ (hash-recur (resolvent-conjunction my-r))
        (hash-recur (resolvent-substitution my-r))))
   (define (hash2-proc my-r hash2-recur)
     (+ (hash2-recur (resolvent-conjunction my-r))
        (hash2-recur (resolvent-substitution my-r))))])
(provide
 (struct*-doc
  resolvent
  ([conjunction (listof conjunct?)]
   [substitution (listof concrete-equality?)])
  @{Summarizes the result of a resolution step.}))

(define (rename c [gensym gensym])
  (define (extract-variables el)
    (match el
      [(variable v)
       (list (variable v))]
      [(or
        (atom _ meaningful-content)
        (function _ meaningful-content)
        (and (? list?) meaningful-content))
       (append-map extract-variables meaningful-content)]
      [(concrete-multi lst)
       (extract-variables lst)]
      [(rule h b _)
       (extract-variables (cons h b))]))
  (let* ([vars (remove-duplicates (extract-variables c))]
         [subst (map (λ (v) (concrete-equality v (variable (gensym 'Var)))) vars)])
    (apply-variable-substitution subst c)))
(provide rename)

(define (resolve conjunction idx clause [gensym gensym])
  (let* ([conjunct (list-ref conjunction idx)]
         [renamed-clause (rename clause gensym)]
         [unifier (concrete-unify (list (concrete-equality conjunct (rule-head renamed-clause))))])
    (and
     unifier
     (resolvent
      (apply-variable-substitution
       unifier
       (append
        (take conjunction idx)
        (rule-body renamed-clause)
        (drop conjunction (add1 idx))))
      unifier))))
(provide
 (proc-doc/names
  resolve
  (->*
   ((listof conjunct?) exact-nonnegative-integer? rule?)
   (procedure?)
   (or/c #f resolvent?))
  ((conjunction idx clause) ((gensym gensym)))
  @{Resolves the selected conjunct in @racket[conjunction] at index position @racket[idx] using @racket[clause].
 If concrete resolution is not possible, this returns @racket[#f].
 Note that @racket[clause] does not need to be renamed before this function is applied as this is done automatically using @racket[gensym]. An alternative symbol generation function can optionally be supplied, but this is only for the purpose of using mocks in tests.}))