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


(: pre-abstract-aux (-> (U atom Term) hash (U abstract-atom AbstractTerm)))
; TODO implement
  
; TODO test!
(: pre-abstract (-> (U atom Conjunction Term) (U abstract-atom AbstractConjunction AbstractTerm)))
(define (pre-abstract concrete-domain-elem)
  (cond [(atom? concrete-domain-elem) (abstract-atom (atom-symbol concrete-domain-elem) (mapAccum pre-abstract-aux (hash-set) (atom-args concrete-domain-elem)))]
        [(Conjunction? concrete-domain-elem) (mapAccum pre-abstract-aux (hash-set) concrete-domain-elem)]
        [(function? concrete-domain-elem) (if (null? (function-args concrete-domain-elem))
                                              (car (pre-abstract-aux concrete-domain-elem (hash-set)))
                                              (abstract-function (function-functor concrete-domain-elem)
                                                                 (mapAccum pre-abstract-aux (hash-set) (function-args concrete-domain-elem))))]))
  