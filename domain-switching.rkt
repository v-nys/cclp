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
(define (pre-abstract-aux-constant constant existing-mapping concrete-constants)
  (if (hash-has-key? existing-mapping constant)
      (cons (hash-ref existing-mapping constant) existing-mapping)
      (if (member constant concrete-constants)
          (cons (abstract-function (function-functor constant) '()) existing-mapping)
          (let ([max-g (get-maximum-abstract-var g? avar-index (hash-values existing-mapping))])
            (match max-g [(none) (cons (g 1) (hash-set existing-mapping constant (g 1)))]
              [(some index) (cons (g (+ index 1)) (hash-set existing-mapping constant (g (+ index 1))))])))))
(provide pre-abstract-aux-constant)

;(: pre-abstract-aux-term (-> Term (HashTable Term AbstractVariable) (Pair AbstractTerm (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-term concrete-constants concrete-term existing-mapping)
  (cond [(variable? concrete-term)
         (pre-abstract-aux-variable concrete-term existing-mapping)]
        [(function? concrete-term)
         (if (null? (function-args concrete-term))
             (pre-abstract-aux-constant concrete-term existing-mapping concrete-constants)
             (let* ([applied-to-args
                     (map-accumulatel
                      (curry pre-abstract-aux-term concrete-constants)
                      existing-mapping
                      (function-args concrete-term))]
                    [just-mapped-args (car applied-to-args)]
                    [just-acc (cdr applied-to-args)])
               (cons (abstract-function (function-functor concrete-term) just-mapped-args) just-acc)))]
        
        [(number? concrete-term)
         (pre-abstract-aux-constant concrete-term existing-mapping concrete-constants)]
        [else (error "Missed a case")]))
(provide pre-abstract-aux-term)

; note: there is some duplication here, solely due to Conjunctions...
;(: pre-abstract-aux-atom (-> atom (HashTable Term AbstractVariable) (Pair abstract-atom (HashTable Term AbstractVariable))))
(define (pre-abstract-aux-atom concrete-constants concrete-atom existing-mapping)
  (if (null? (atom-args concrete-atom))
      (cons (abstract-atom (atom-symbol concrete-atom) '()) existing-mapping)
      (let* ([applied-to-args (map-accumulatel (curry pre-abstract-aux-term concrete-constants) existing-mapping (atom-args concrete-atom))]
             [just-mapped-args (car applied-to-args)]
             [just-acc (cdr applied-to-args)])
        (cons (abstract-atom (atom-symbol concrete-atom) just-mapped-args) just-acc))))

(define (pre-abstract-rule concrete-rule concrete-constants)
  (let* ([rule-as-conjunction (cons (ck:rule-head concrete-rule) (ck:rule-body concrete-rule))]
         [abstracted-conjunction (pre-abstract-conjunction rule-as-conjunction concrete-constants)])
    (ak:abstract-rule (car abstracted-conjunction) (cdr abstracted-conjunction))))
(provide
 (proc-doc/names
  pre-abstract-rule
  (-> ck:rule? (listof function?) ak:abstract-rule?)
  (concrete-rule concrete-constants)
  @{Takes a concrete rule @racket[concrete-rule] and abstracts it so that abstract resolution can be applied.
 Abstraction assumes that all ground terms become @racket[g?] values,
 but concrete constants listed in @racket[concrete-constants] are mapped to abstract constants with the same functor.}))

;(: pre-abstract-conjunction (-> Conjunction AbstractConjunction))
(define (pre-abstract-conjunction conjunction concrete-constants)
  (car (map-accumulatel (curry pre-abstract-aux-atom concrete-constants) (hash) conjunction)))

(define (pre-abstract concrete-domain-elem)
  (cond [(atom? concrete-domain-elem)
         (abstract-atom
          (atom-symbol concrete-domain-elem)
          ; we only care about retaining concrete constants in the specific case of clauses
          (car (map-accumulatel (curry pre-abstract-aux-term (list)) (hash) (atom-args concrete-domain-elem))))]
        [(list? concrete-domain-elem)
         (pre-abstract-conjunction concrete-domain-elem (list))]
        [(term? concrete-domain-elem)
         (car (pre-abstract-aux-term (list) concrete-domain-elem (hash)))]))
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

(module+ test
  (require rackunit)
  (require "cclp-interpreter.rkt")
  (check-equal?
   (get-maximum-abstract-var
    a?
    avar-index
    (list (g 1) (a 2) (g 5) (a 9) (a 6) (g 14)))
   (some 9)
   "Find the biggest a, where there is one")
  (check-equal? (get-maximum-abstract-var g? avar-index (list (g 1) (a 2) (g 5) (a 9) (a 6) (g 14))) (some 14) "Find the biggest g, where there is one")
  (check-equal? (get-maximum-abstract-var g? avar-index (list (a 1) (a 2) (a 5) (a 9) (a 6) (a 14))) (none) "Find the biggest g, where there is none")

  (check-equal? (pre-abstract (variable "A")) (a 1) "single new variable case")
  (check-equal? (pre-abstract (function "dummy" '())) (g 1) "single new constant case")

  (check-equal?
   (pre-abstract-aux-constant
    (function 'dummy '())
    (hash)
    (list))
   (cons (g 1) (hash (function 'dummy '()) (g 1)))
   "case of constant with no existing mapping")
  (check-equal?
   (pre-abstract-aux-constant
    (function 'dummy '())
    (hash)
    (list (function 'dummy '())))
   (cons (abstract-function 'dummy '()) (hash))
   "case of constant in the abstract domain with no existing mapping")
  (check-equal?
   (pre-abstract-aux-constant 3 (hash) (list))
   (cons (g 1) (hash 3 (g 1)))
   "case of constant with no existing mapping")
  (check-equal?
   (pre-abstract-aux-constant
    (function "dummy" '())
    (hash (function "dummy" '()) (g 1))
    (list))
   (cons (g 1) (hash (function "dummy" '()) (g 1)))
   "case of constant with an existing mapping")
  (check-equal?
   (pre-abstract-aux-constant
    (function "dummy2" '())
    (hash (function "dummy1" '()) (g 1))
    (list))
   (cons (g 2) (hash (function "dummy1" '()) (g 1) (function "dummy2" '()) (g 2)))
   "case of constant when there is a mapping for another constant")

  (let ([abstract-args (list (a 1) (a 1) (a 2) (g 1) (g 2) (g 1))]
        ; should be able to do this more concisely using #lang lp building blocks, roughly as (expand (parse 'function "dummy(A,A,dummy2,dummy3,dummy2)"))
        [concrete-args (list (variable "A") (variable "A") (variable "B") (function "dummy2" '()) (function "dummy3" '()) (function "dummy2" '()))])
    (check-equal? (pre-abstract (function "dummy" concrete-args)) (abstract-function "dummy" abstract-args) "abstracting a complex term"))

  (check-equal? (pre-abstract-rule (interpret-concrete-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)") (list))
                (ak:abstract-rule (interpret-abstract-atom "collect(tree(α1,α2),α3)") (list (interpret-abstract-atom "collect(α1,α4)") (interpret-abstract-atom "collect(α2,α5)") (interpret-abstract-atom "append(α4,α5,α3)"))))

  (check-equal? (pre-abstract-rule (interpret-concrete-rule "append([],L,L)") (list (function 'nil '())))
                (ak:abstract-rule (interpret-abstract-atom "append(nil,α1,α1)") (list))))
