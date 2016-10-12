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

#lang at-exp racket
(require "concrete-domain.rkt")
(require "abstract-multi-domain.rkt")
(require "data-utils.rkt")
(require racket-list-utils/utils)
(require (prefix-in ck: "concrete-knowledge.rkt") (prefix-in ak: "abstract-knowledge.rkt"))
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (get-maximum-abstract-var type-test? index-selector vals)
  (foldl
   (λ (val acc)
     (cond [(not (type-test? val)) acc]
           [(none? acc) (some (index-selector val))]
           [(some? acc) (some (max (some-v acc) (index-selector val)))]))
   (none)
   vals))
(provide
 (proc-doc/names
  get-maximum-abstract-var
  (->
   (-> abstract-variable? boolean?)
   (-> abstract-variable? exact-positive-integer?)
   (listof abstract-variable?)
   (maybe exact-positive-integer?))
  (type-test? index-selector vals)
  @{Finds the greatest index of an abstract variable in @racket[vals]
 which passes @racket[type-test?] by selecting it with @racket[index-selector].}))

;(: pre-abstract-aux-variable (-> variable (HashTable Term AbstractVariable) (Pair AbstractVariable (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-variable var existing-mapping)
  (if (hash-has-key? existing-mapping var)
      (cons (hash-ref existing-mapping var) existing-mapping)
      (let ([max-a (get-maximum-abstract-var a? avar-index (hash-values existing-mapping))])
        (match max-a [(none) (cons (a 1) (hash-set existing-mapping var (a 1)))]
          [(some val) (cons (a (+ val 1)) (hash-set existing-mapping var (a (+ val 1))))]))))

;(: pre-abstract-aux-constant (-> function (HashTable Term AbstractVariable) (Pair AbstractVariable (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-constant constant existing-mapping)
  (if (hash-has-key? existing-mapping constant)
      (cons (hash-ref existing-mapping constant) existing-mapping)
      (let ([max-g (get-maximum-abstract-var g? avar-index (hash-values existing-mapping))])
        (match max-g [(none) (cons (g 1) (hash-set existing-mapping constant (g 1)))]
          [(some index) (cons (g (+ index 1)) (hash-set existing-mapping constant (g (+ index 1))))]))))
(provide pre-abstract-aux-constant)

;(: pre-abstract-aux-term (-> Term (HashTable Term AbstractVariable) (Pair AbstractTerm (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-term concrete-term existing-mapping)
  (cond [(variable? concrete-term)
         (pre-abstract-aux-variable concrete-term existing-mapping)]
        [(function? concrete-term)
         (if (null? (function-args concrete-term))
             (pre-abstract-aux-constant concrete-term existing-mapping)
             (let* ([applied-to-args
                     (map-accumulatel
                      pre-abstract-aux-term
                      existing-mapping
                      (function-args concrete-term))]
                    [just-mapped-args (car applied-to-args)]
                    [just-acc (cdr applied-to-args)])
               (cons (abstract-function (function-functor concrete-term) just-mapped-args) just-acc)))]
        [(number? concrete-term) (error "TODO: implement abstraction of numbers")]
        [else (error "Missed a case")]))
(provide pre-abstract-aux-term)

; note: there is some duplication here, solely due to Conjunctions...
;(: pre-abstract-aux-atom (-> atom (HashTable Term AbstractVariable) (Pair abstract-atom (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-atom concrete-atom existing-mapping)
  (if (null? (atom-args concrete-atom))
      (cons (abstract-atom (atom-symbol concrete-atom) '()) existing-mapping)
      (let* ([applied-to-args (map-accumulatel pre-abstract-aux-term existing-mapping (atom-args concrete-atom))]
             [just-mapped-args (car applied-to-args)]
             [just-acc (cdr applied-to-args)])
        (cons (abstract-atom (atom-symbol concrete-atom) just-mapped-args) just-acc))))

;(: pre-abstract-rule (-> ck:rule ak:rule))
(define (pre-abstract-rule concrete-rule)
  (let* ([rule-as-conjunction (cons (ck:rule-head concrete-rule) (ck:rule-body concrete-rule))]
         [abstracted-conjunction (pre-abstract-conjunction rule-as-conjunction)])
    (ak:abstract-rule (car abstracted-conjunction) (cdr abstracted-conjunction))))
(provide pre-abstract-rule)

;(: pre-abstract-conjunction (-> Conjunction AbstractConjunction))
(define (pre-abstract-conjunction conjunction)
  (car (map-accumulatel pre-abstract-aux-atom (hash) conjunction)))

(define (pre-abstract concrete-domain-elem)
  (cond [(atom? concrete-domain-elem)
         (abstract-atom
          (atom-symbol concrete-domain-elem)
          (car (map-accumulatel pre-abstract-aux-term (hash) (atom-args concrete-domain-elem))))]
        [(list? concrete-domain-elem)
         (pre-abstract-conjunction concrete-domain-elem)]
        [(term? concrete-domain-elem)
         (car (pre-abstract-aux-term concrete-domain-elem (hash)))]))
(provide
 (proc-doc/names
  pre-abstract
  (->
   (or/c atom? (listof atom?) term?)
   (or/c abstract-atom? (listof abstract-atom?) abstract-term?))
  (concrete-domain-elem)
  @{Returns the most specific abstraction of @racket[concrete-domain-elem].
 If @racket[concrete-domain-elem] is a concrete atom, the result is an abstract atom.
 If @racket[concrete-domain-elem] is a concrete conjunction, the result is a concrete conjunction.
 If @racket[concrete-domain-elem] is a concrete term, the result is an abstract term.}))
