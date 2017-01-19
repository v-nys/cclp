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
(require "abstract-knowledge.rkt")
(require "abstract-substitution.rkt")
(require "abstract-unify.rkt")
(require "data-utils.rkt")
(require "abstract-multi-domain.rkt")
(require "abstract-domain-ordering.rkt")
(require "execution.rkt")
(require "concrete-knowledge.rkt")
(require "domain-switching.rkt")
(require "abstract-renaming.rkt")
(require "abstraction-inspection-utils.rkt")
(require (only-in parenlog model?))
(require racket/logging)
(require (only-in "concrete-domain.rkt" function?))

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (write-resolvent obj port mode)
  (if (boolean? mode)
      (fprintf port
               "#(struct:resolvent ~s ~s ~s)"
               (resolvent-conjunction obj)
               (resolvent-substitution obj)
               (resolvent-knowledge obj))
      (fprintf port
               "resolvent ~v of rule ~v after substitution ~v"
               (resolvent-conjunction obj)
               (resolvent-knowledge obj)
               (resolvent-substitution obj))))
(struct resolvent (conjunction substitution knowledge)
  #:methods
  gen:custom-write
  [(define write-proc write-resolvent)]
  #:methods
  gen:equal+hash
  [(define (equal-proc r1 r2 equal?-recur)
     (and (equal?-recur (resolvent-conjunction r1) (resolvent-conjunction r2))
          (equal?-recur (resolvent-substitution r1) (resolvent-substitution r2))
          (equal?-recur (resolvent-knowledge r1) (resolvent-knowledge r2))))
   (define (hash-proc my-r hash-recur)
     (+ (hash-recur (resolvent-conjunction my-r))
        (hash-recur (resolvent-substitution my-r))
        (hash-recur (resolvent-knowledge my-r))))
   (define (hash2-proc my-r hash2-recur)
     (+ (hash2-recur (resolvent-conjunction my-r))
        (hash2-recur (resolvent-substitution my-r))
        (hash2-recur (resolvent-knowledge my-r))))])
(provide
 (struct*-doc
  resolvent
  ([conjunction (listof abstract-atom?)]
   [substitution (listof abstract-equality?)]
   [knowledge (or/c rule? full-evaluation?)])
  @{Summarizes the result of a resolution step.}))

(define (abstract-resolve conjunction idx concrete-clauses full-evaluations concrete-constants)
  (define (fold-over-knowledge i kb)
    (foldl
     (λ (k acc)
       (let ([step-outcome (abstract-step i conjunction k concrete-constants)])
         (if step-outcome
             (cons step-outcome acc)
             acc)))
     (list)
     kb))
  (let* ([conjunct (list-ref conjunction idx)]
         [outcomes-full-eval ((curry fold-over-knowledge idx) full-evaluations)])
    (if (null? outcomes-full-eval)
        ((curry fold-over-knowledge idx) concrete-clauses)
        outcomes-full-eval)))
(provide
 (proc-doc/names
  abstract-resolve
  (-> (listof abstract-atom?)
      exact-nonnegative-integer?
      (listof rule?)
      (listof full-evaluation?)
      (listof function?)
      (listof resolvent?))
  (conjunction idx concrete-clauses full-evaluations concrete-constants)
  @{Resolves the abstract atom in position @racket[idx] in @racket[conjunction]
 with every applicable rule in both
 @racket[concrete-clauses] and @racket[full-evaluations].
 The rules in @racket[concrete-clauses] themselves are concrete,
 but they are abstracted to apply resolution.
 Concrete constants in @racket[concrete-constants] are mapped to abstract constants,
 rather than abstract variables.
 The result is a @racket[list] of outcomes for every possible resolution step.}))

(define (abstract-step conjunct-index conjunction knowledge concrete-constants)
  (define conjunct (list-ref conjunction conjunct-index))
  (define abstract-knowledge (if (rule? knowledge) (pre-abstract-rule knowledge concrete-constants) knowledge))
  (define renamed-abstract-knowledge (rename-apart abstract-knowledge conjunction))
  (define g-offset
    (let ([candidate (maximum-var-index conjunction g?)])
      (if (some? candidate) (some-v candidate) 0)))
  (if (abstract-rule? renamed-abstract-knowledge)
      (let* ([in-subst (abstract-equality
                        conjunct
                        (abstract-rule-head renamed-abstract-knowledge))]
             [out-subst (abstract-unify (list in-subst) g-offset)])
        (let*-values ([(before from) (split-at conjunction conjunct-index)]
                      [(stitched)
                       (append before
                               (abstract-rule-body renamed-abstract-knowledge)
                               (cdr from))])
          (if (some? out-subst)
              (begin
                (log-debug (format "Successfully resolved with ~a" renamed-abstract-knowledge))
                (resolvent
                 (apply-substitution (some-v out-subst) stitched)
                 (some-v out-subst)
                 knowledge))
              #f)))
      (if (>=-extension (full-evaluation-input-pattern renamed-abstract-knowledge) conjunct)
          (let* ([in-subst
                  (abstract-equality
                   conjunct
                   (full-evaluation-output-pattern renamed-abstract-knowledge))]
                 [out-subst (abstract-unify (list in-subst) g-offset)]
                 [unspliced
                  (let-values ([(before from) (split-at conjunction conjunct-index)])
                    (append before (cdr from)))])
            (if (some? out-subst)
                (resolvent (apply-substitution (some-v out-subst) unspliced)
                           (some-v out-subst)
                           knowledge)
                (error "output pattern could not be applied - full evaluation is wrong?")))
          #f)))

(module+ test
  (require rackunit)
  (require (for-syntax syntax/parse))
  (require "cclp-interpreter.rkt")
  (require (for-syntax (only-in "abstract-substitution.rkt" asubst)))

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
                     ((g 1) (cons [(g 8) (g 9)]))
                     ((a 1) (cons [(a 8) (a 9)])))
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
       full-eval)))))

; TODO test whether single-step unfolding does not take place when full eval is applied
