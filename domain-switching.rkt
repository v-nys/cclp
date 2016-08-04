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

; functionality for switching between the concrete and abstract domain
; essentially the abstraction function α and concretization function γ

#lang typed/racket
(require "concrete-domain.rkt")
(require "abstract-multi-domain.rkt")
(require typed-racket-list-utils/utils)

(struct none () #:transparent)
(provide (struct-out none))
(struct (a) some ([v : a]) #:transparent)
(provide (struct-out some))
(define-type (Opt a) (U none (some a)))

(: get-maximum-abstract-var (-> (-> AbstractVariable Boolean) (-> AbstractVariable Integer) (Listof AbstractVariable) (Opt Integer)))
; type-test is to distinguish a from g
; index-selector gets the index from a or g, respectively
(define (get-maximum-abstract-var type-test? index-selector vals)
  (foldl (λ ([val : AbstractVariable] [acc : (Opt Integer)])
           (cond [(not (type-test? val)) acc]
                 [(none? acc) (some (index-selector val))]
                 [(some? acc) (some (max (some-v acc) (index-selector val)))]
                 [else none])) (none) vals)) ; last clause is just there to get the type right
(provide get-maximum-abstract-var)

(: pre-abstract-aux-variable (-> variable (HashTable Term AbstractVariable) (Pair AbstractVariable (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-variable var existing-mapping)
  (if (hash-has-key? existing-mapping var)
      (cons (hash-ref existing-mapping var) existing-mapping)
      (let ([max-a (get-maximum-abstract-var a? avar-index (hash-values existing-mapping))])
        (match max-a [none (cons (a 1) (hash-set existing-mapping var (a 1)))]
          [(some val) (cons (a (+ val 1)) (hash-set existing-mapping var (a (+ val 1))))]))))

(: pre-abstract-aux-constant (-> function (HashTable Term AbstractVariable) (Pair AbstractVariable (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-constant constant existing-mapping)
  (if (hash-has-key? existing-mapping constant)
      (cons (hash-ref existing-mapping constant) existing-mapping)
      (let ([max-g (get-maximum-abstract-var g? avar-index (hash-values existing-mapping))])
        (match max-g [none (cons (g 1) (hash-set existing-mapping constant (g 1)))]
          [(some val) (cons (g (+ val 1)) (hash-set existing-mapping constant (g (+ val 1))))]))))
(provide pre-abstract-aux-constant)

(: pre-abstract-aux-term (-> Term (HashTable Term AbstractVariable) (Pair AbstractTerm (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-term concrete-term existing-mapping)
  (cond [(variable? concrete-term) (pre-abstract-aux-variable concrete-term existing-mapping)]
        [(function? concrete-term) (if (null? (function-args concrete-term))
                                       (pre-abstract-aux-constant concrete-term existing-mapping)
                                       (let* ([applied-to-args (mapAccum pre-abstract-aux-term existing-mapping (function-args concrete-term))]
                                              [just-mapped-args (car applied-to-args)]
                                              [just-acc (cdr applied-to-args)])
                                         (cons (abstract-function (function-functor concrete-term) just-mapped-args) just-acc)))]))

; note: there is some duplication here, solely due to Conjunctions...
(: pre-abstract-aux-atom (-> atom (HashTable Term AbstractVariable) (Pair abstract-atom (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-atom concrete-atom existing-mapping)
  (if (null? (atom-args concrete-atom))
      (cons (abstract-atom (atom-symbol concrete-atom) '()) existing-mapping)
      (let* ([applied-to-args (mapAccum pre-abstract-aux-term existing-mapping (atom-args concrete-atom))]
             [just-mapped-args (car applied-to-args)]
             [just-acc (cdr applied-to-args)])
        (cons (abstract-atom (atom-symbol concrete-atom) just-mapped-args) just-acc))))

(: pre-abstract (-> (U atom Conjunction Term) (U abstract-atom AbstractConjunction AbstractTerm)))
(define (pre-abstract concrete-domain-elem)
  (cond [(atom? concrete-domain-elem) (abstract-atom (atom-symbol concrete-domain-elem) (car (mapAccum pre-abstract-aux-term (ann (hash) (HashTable Term AbstractVariable)) (atom-args concrete-domain-elem))))]
        [(Conjunction? concrete-domain-elem) (car (mapAccum pre-abstract-aux-atom (ann (hash) (HashTable Term AbstractVariable)) concrete-domain-elem))]
        [(Term? concrete-domain-elem) (car (pre-abstract-aux-term concrete-domain-elem (ann (hash) (HashTable Term AbstractVariable))))]))
(provide pre-abstract)