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
(require "abstract-multi-domain.rkt"
         "data-utils.rkt"
         "abstract-substitution.rkt"
         "abstraction-inspection-utils.rkt"
         "abstract-knowledge.rkt")
;(: opt-max (-> (Opt Integer) (Opt Integer) Integer Integer))
(define (opt-max opt1 opt2 default)
  (cond [(and (some? opt1) (some? opt2)) (max (some-v opt1) (some-v opt2))]
        [(some? opt1) (some-v opt1)]
        [(some? opt2) (some-v opt2)]
        [else default]))

(define (rename-apart renamee non-renamee)
  (let* ([g-max-non-renamee (maximum-var-index non-renamee g?)]
         [a-max-non-renamee (maximum-var-index non-renamee a?)]
         [g-max-renamee (maximum-var-index renamee g?)]
         [a-max-renamee (maximum-var-index renamee a?)]
         [g-offset (opt-max g-max-non-renamee g-max-renamee 0)]
         [a-offset (opt-max a-max-non-renamee a-max-renamee 0)]
         [a-indices (assemble-var-indices a? renamee)]
         [g-indices (assemble-var-indices g? renamee)]
         [subst
          (append
           (map
            (λ (index) (abstract-equality (a index) (a (+ a-offset index))))
            (set->list a-indices))
           (map
            (λ (index) (abstract-equality (g index) (g (+ g-offset index))))
            (set->list g-indices)))])
    (apply-substitution subst renamee)))
(provide
 (contract-out
  [rename-apart
   (-> (or/c abstract-domain-elem? abstract-knowledge?)
       abstract-domain-elem?
       (or/c abstract-domain-elem? abstract-knowledge?))]))

(module+ test
  (require rackunit "cclp-interpreter.rkt" "domain-switching.rkt" (prefix-in ak: "abstract-knowledge.rkt"))
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