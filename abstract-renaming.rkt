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
(require "abstract-multi-domain.rkt"
         "data-utils.rkt"
         "abstract-substitution.rkt"
         "abstraction-inspection-utils.rkt"
         "abstract-knowledge.rkt")
(require racket-list-utils/utils)
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(module+ test (require rackunit))

;(: opt-max (-> (Opt Integer) (Opt Integer) Integer Integer))
(define (opt-max opt1 opt2 default)
  (cond [(and (some? opt1) (some? opt2)) (max (some-v opt1) (some-v opt2))]
        [(some? opt1) (some-v opt1)]
        [(some? opt2) (some-v opt2)]
        [else default]))

; TODO might want some tests here, buddy
(define (offset-vars renamee a-offset g-offset)
  (let* ([a-indices (assemble-var-indices a? renamee)]
         [g-indices (assemble-var-indices g? renamee)]
         [subst
          (append
           (map
            (λ (index) (abstract-equality (a index) (a (+ a-offset index))))
            a-indices)
           (map
            (λ (index) (abstract-equality (g index) (g (+ g-offset index))))
            g-indices))])
    (apply-substitution subst renamee)))
(provide
 (proc-doc/names
  offset-vars
  (->
   (or/c abstract-domain-elem? abstract-knowledge?)
   exact-integer?
   exact-integer?
   (or/c abstract-domain-elem? abstract-knowledge?))
  (renamee a-offset g-offset)
  @{Offset the "any" variables in an abstraction by @racket[a-offset]
 and offset the "ground" variables by @racket[g-offset].}))

(define (rename-apart renamee non-renamee)
  (let* ([g-max-non-renamee (maximum-var-index non-renamee g?)]
         [a-max-non-renamee (maximum-var-index non-renamee a?)]
         [g-max-renamee (maximum-var-index renamee g?)]
         [a-max-renamee (maximum-var-index renamee a?)]
         [g-offset (opt-max g-max-non-renamee g-max-renamee 0)]
         [a-offset (opt-max a-max-non-renamee a-max-renamee 0)])
    (offset-vars renamee a-offset g-offset)))
(provide
 (contract-out
  [rename-apart
   (-> (or/c abstract-domain-elem? abstract-knowledge?)
       abstract-domain-elem?
       (or/c abstract-domain-elem? abstract-knowledge?))]))
(module+ test
  (require "cclp-interpreter.rkt" "domain-switching.rkt" (prefix-in ak: "abstract-knowledge.rkt"))
  (test-case
   "the right abstract rule should be obtained"
   (let* ([rule (interpret-concrete-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)")]
          [abstract-conjunction (list (interpret-abstract-atom "collect(γ1,α1)"))]
          [abstract-rule (pre-abstract-rule rule (list))]
          [renamed-abstract-rule (rename-apart abstract-rule abstract-conjunction)]
          [expected
           (ak:abstract-rule (interpret-abstract-atom "collect(tree(α6,α7),α8)")
                             (list (interpret-abstract-atom "collect(α6,α9)")
                                   (interpret-abstract-atom "collect(α7,α10)")
                                   (interpret-abstract-atom "append(α9,α10,α8)")))])
     (check-equal?
      abstract-rule
      (ak:abstract-rule (interpret-abstract-atom "collect(tree(α1,α2),α3)")
                        (list (interpret-abstract-atom "collect(α1,α4)")
                              (interpret-abstract-atom "collect(α2,α5)")
                              (interpret-abstract-atom "append(α4,α5,α3)"))))
     (check-equal? renamed-abstract-rule expected)))
  (test-case
   "a properly renamed full evaluation should be obtained"
   (let* ([full-eval (ak:full-evaluation (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                                         (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)"))]
          [abstract-conjunction
           (interpret-abstract-conjunction "del(α12,[γ18|γ19],α14),perm(α14,α13),ord([γ3,α12|α13])")]
          [renamed-abstract-rule (rename-apart full-eval abstract-conjunction)]
          [expected
           (ak:full-evaluation (interpret-abstract-atom "del(α15,[γ20|γ21],α16)")
                               (interpret-abstract-atom "del(γ22,[γ20|γ21],γ23)"))])
     (check-equal?
      renamed-abstract-rule
      expected))))

(define (normalize-abstract-atom aa)
  (let* ([mmax-a (maximum-var-index aa a?)]
         [mmax-g (maximum-var-index aa g?)]
         [g-offset (if (some? mmax-g) (some-v mmax-g) 0)]
         [a-offset (if (some? mmax-a) (some-v mmax-a) 0)]
         [renamed-aa (offset-vars aa a-offset g-offset)]
         [subst
          (append
           (car (map-accumulatel
                 (λ (el acc) (cons (abstract-equality (a el) (a acc)) (add1 acc)))
                 1
                 (assemble-var-indices a? renamed-aa)))
           (car (map-accumulatel
                 (λ (el acc) (cons (abstract-equality (g el) (g acc)) (add1 acc)))
                 1
                 (assemble-var-indices g? renamed-aa))))])
    (apply-substitution-to-conjunct subst renamed-aa)))
(provide
 (proc-doc/names
  normalize-abstract-atom
  (-> abstract-atom? abstract-atom?)
  (abstract-atom)
  @{Returns a normalized representation of @racket[abstract-atom].}))

(module+ test
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (a 5))))
   (abstract-atom 'foo (list (a 1))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (g 5))))
   (abstract-atom 'foo (list (g 1))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (a 5) (a 6))))
   (abstract-atom 'foo (list (a 1) (a 2))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (g 5) (g 6))))
   (abstract-atom 'foo (list (g 1) (g 2))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (a 6) (a 5))))
   (abstract-atom 'foo (list (a 1) (a 2))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (g 6) (g 5))))
   (abstract-atom 'foo (list (g 1) (g 2)))
   "avoid naive substitution")
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (a 2) (a 1))))
   (abstract-atom 'foo (list (a 1) (a 2))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (g 2) (g 1))))
   (abstract-atom 'foo (list (g 1) (g 2))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (a 6) (a 5) (a 6))))
   (abstract-atom 'foo (list (a 1) (a 2) (a 1))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (g 6) (g 5) (g 6))))
   (abstract-atom 'foo (list (g 1) (g 2) (g 1))))
  (check-equal?
   (normalize-abstract-atom
    (abstract-atom 'foo (list (g 6) (a 5) (g 6))))
   (abstract-atom 'foo (list (g 1) (a 1) (g 1)))))