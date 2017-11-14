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
  cclp/concrete-domain
  cclp/concrete-knowledge
  cclp/concrete-substitution
  cclp/concrete-unify)
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
  ([conjunction (listof atom?)]
   [substitution (listof concrete-equality?)])
  @{Summarizes the result of a resolution step.}))

(define (apply-variable-substitution subst ctxt)
  (define rec (curry apply-variable-substitution subst))
  (match subst
    [(list) ctxt]
    [(list-rest sh st)
     (match ctxt
       [(variable vn)
        #:when (eq? vn (variable-name (concrete-equality-term1 sh)))
        (concrete-equality-term2 sh)]
       [(variable vn) (variable vn)]
       [(atom sym args)
        (atom sym (rec args))]
       [(function sym args)
        (function sym (rec args))]
       [(? list?)
        (map rec ctxt)]
       [(rule h t idx)
        (rule (rec h) (rec t) idx)]
       [(concrete-equality t1 t2)
        (concrete-equality (rec t1) (rec t2))]
       [else (error "Don't know how to substitute in this context.")])]))

(define (rename-clause c)
  (define (extract-variables el)
    (match el
      [(variable v)
       (list (variable v))]
      [(or
        (atom _ args)
        (function _ args)
        (and (? list?) args))
       (append-map extract-variables args)]))
  (let* ([vars (remove-duplicates (extract-variables (cons (rule-head c) (rule-body c))))]
         [subst (map (λ (v) (concrete-equality v (variable (gensym 'Var)))) vars)])
    (apply-variable-substitution subst c)))

(define (resolve conjunction idx clause)
  (let* ([conjunct (list-ref conjunction idx)]
         [renamed-clause (rename-clause clause)]
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
  (-> (listof atom?) exact-nonnegative-integer? rule? (or/c #f resolvent?))
  (conjunction idx clause)
  @{Resolves the selected conjunct in @racket[conjunction] at index position @racket[idx] using @racket[clause].
 If concrete resolution is not possible, this returns @racket[#f].
 Note that @racket[clause] does not need to be renamed before this function is applied as this is done automatically.}))