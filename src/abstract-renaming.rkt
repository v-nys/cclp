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

;(: rename-apart (-> ak:AbstractKnowledge AbstractConjunction ak:AbstractKnowledge))
; modified so that it works for all abstract domain elements
(define (rename-apart renamee non-renamee)
  (let* ([g-max-non-renamee (maximum-var-index non-renamee g?)]
         [a-max-non-renamee (maximum-var-index non-renamee a?)]
         [g-max-renamee (maximum-var-index renamee g?)]
         [a-max-renamee (maximum-var-index renamee a?)]
         [g-offset (opt-max g-max-non-renamee g-max-renamee 0)]
         [a-offset (opt-max a-max-non-renamee a-max-renamee 0)]
         [a-indices (assemble-var-indices a? renamee)]
         [g-indices (assemble-var-indices g? renamee)]
         [subst (append (map (λ (index) (abstract-equality (a index) (a (+ a-offset index)))) (set->list a-indices))
                        (map (λ (index) (abstract-equality (g index) (g (+ g-offset index)))) (set->list g-indices)))])
    (apply-substitution subst renamee)))
(provide
 (contract-out
  [rename-apart
   (-> (or/c abstract-domain-elem? abstract-knowledge?)
       abstract-domain-elem?
       (or/c abstract-domain-elem? abstract-knowledge?))]))