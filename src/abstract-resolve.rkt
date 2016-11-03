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

(define (abstract-resolve conjunction prior concrete-clauses full-evaluations concrete-constants)
  (define (fold-over-knowledge i kb)
    (foldl
     (λ (k acc)
       (let ([step-outcome (abstract-step i conjunction k concrete-constants)])
         (if step-outcome
             (cons step-outcome acc)
             acc)))
     (list)
     kb))
  (let* ([conjunct-index (selected-index conjunction prior full-evaluations)]
         [conjunct (list-ref conjunction conjunct-index)]
         [outcomes-full-eval ((curry fold-over-knowledge conjunct-index) full-evaluations)])
    (cons conjunct-index
          (if (null? outcomes-full-eval)
              ((curry fold-over-knowledge conjunct-index) concrete-clauses)
              outcomes-full-eval))))

(provide
 (proc-doc/names
  abstract-resolve
  (-> (listof abstract-atom?)
      model?
      (listof rule?)
      (listof full-evaluation?)
      (listof function?)
      (cons/c exact-nonnegative-integer? (listof resolvent?)))
  (conjunction prior concrete-clauses full-evaluations concrete-constants)
  @{Resolves the next abstract atom selected from @racket[conjunction]
 by partial order @racket[prior] with every applicable rule in both
 @racket[concrete-clauses] and @racket[full-evaluations].
 The rules in @racket[concrete-clauses] themselves are concrete,
 but they are abstracted to apply resolution.
 Concrete constants in @racket[concrete-constants] are mapped to abstract constants,
 rather than abstract variables.
 The result is a @racket[pair] consisting of the index of the selected
 abstract atom and a list of outcomes for every possible resolution step.}))

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