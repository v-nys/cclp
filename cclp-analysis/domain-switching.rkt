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
; essentially the abstraction function a and concretization function gconc

#lang at-exp racket
(require
  (only-in racket/struct make-constructor-style-printer)
  (only-in racket/syntax format-symbol)
  "concrete-domain.rkt")
(require "abstract-multi-domain.rkt")
(require "data-utils.rkt")
(require list-utils)
(require (prefix-in ck: "concrete-knowledge.rkt") (prefix-in ak: "abstract-knowledge.rkt"))
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define cons-symbol (string->symbol "'[|]'"))
(provide cons-symbol)
(define concrete-nil (function (string->symbol "[]") (list)))
(provide concrete-nil)
(define (concrete-listify lst)
  (let* ([prefix (proper-prefix lst)]
         [proper? (equal? prefix lst)])
    (foldr
     (λ (e acc) (function cons-symbol (list e acc)))
     (if proper? concrete-nil (improper-tail lst))
     (if proper? lst prefix))))
(provide concrete-listify)

(define (improper-tail imlist)
  (match imlist
    [(cons h t)
     (improper-tail t)]
    [t t]))
(provide improper-tail)

(define (proper-prefix lst)
  (match lst
    [(list-rest h t)
     (cons h (proper-prefix t))]
    [_ empty]))
(provide proper-prefix)

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

  (check-equal? (pre-abstract (variable 'A)) (a 1) "single new variable case")
  (check-equal? (pre-abstract (function 'dummy '())) (g 1) "single new constant case")

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
    (function 'dummy '())
    (hash (function 'dummy '()) (g 1))
    (list))
   (cons (g 1) (hash (function 'dummy '()) (g 1)))
   "case of constant with an existing mapping")
  (check-equal?
   (pre-abstract-aux-constant
    (function 'dummy2 '())
    (hash (function 'dummy1 '()) (g 1))
    (list))
   (cons (g 2) (hash (function 'dummy1 '()) (g 1) (function 'dummy2 '()) (g 2)))
   "case of constant when there is a mapping for another constant")

  ;  (let ([abstract-args (list (a 1) (a 1) (a 2) (g 1) (g 2) (g 1))]
  ;        ; should be able to do this more concisely using #lang lp building blocks, roughly as (expand (parse 'function "dummy(A,A,dummy2,dummy3,dummy2)"))
  ;        [concrete-args (list (variable 'A) (variable 'A) (variable 'B) (function 'dummy2 '()) (function 'dummy3 '()) (function 'dummy2 '()))])
  ;    (check-equal? (pre-abstract (function 'dummy concrete-args)) (abstract-function 'dummy abstract-args) "abstracting a complex term"))
  ;  (check-equal?
  ;   (pre-abstract-rule
  ;    (interpret-concrete-rule "collect(tree(X,Y),Z) :- collect(X,Z1),collect(Y,Z2),append(Z1,Z2,Z)") (list))
  ;   (ak:abstract-rule (interpret-abstract-atom "collect(tree(a1,a2),a3)") (list (interpret-abstract-atom "collect(a1,a4)") (interpret-abstract-atom "collect(a2,a5)") (interpret-abstract-atom "append(a4,a5,a3)"))))
  ;
  ;  (check-equal?
  ;   (pre-abstract-rule
  ;    (interpret-concrete-rule "append([],L,L)")
  ;    (list (function 'nil '())))
  ;   (ak:abstract-rule (interpret-abstract-atom "append(nil,a1,a1)") (list)))
  )


;; note: this is concrete-synth-counterpart rather than concretize because concretization of multi is infinite set
;; this is close, but it is specifically for synthesis (which is also why constraints are not applied to concrete multi)

(define (atom->function e)
  (match e
    [(atom sym args)
     (function sym args)]))
(provide atom->function)

(define (concrete-synth-counterpart elem)
  (define (aux e tail-no)
    (match e
      [(a idx) (cons (variable (format-symbol "A~a" idx)) tail-no)]
      [(g idx) (cons (variable (format-symbol "G~a" idx)) tail-no)]
      [(a* idx1 'i idx2) (cons (variable (format-symbol "A~ai~a" idx1 idx2)) tail-no)]
      [(g* idx1 'i idx2) (cons (variable (format-symbol "G~ai~a" idx1 idx2)) tail-no)]
      [(or
        (abstract-function sym args)
        (abstract-function* sym args))
       (cons (function sym (map (λ (a) (car (aux a 'dummy))) args)) tail-no)]
      [(or
        (abstract-atom sym args)
        (abstract-atom* sym args))
       (cons (atom sym (map (λ (a) (car (aux a 'dummy))) args)) tail-no)]
      [(multi patt _ _ _ _ _)
       (cons
        (concrete-multi
         (function
          cons-symbol
          (list
           (function
            'building_block
            (list
             (concrete-listify (map (compose atom->function (λ (a) (car (aux a 'dummy)))) patt))))
           (variable (format-symbol "Tail~a" tail-no)))))
        (add1 tail-no))]
      [(? list?)
       #:when (andmap abstract-conjunct? e)
       (map-accumulatel aux tail-no e)]))
  (car (aux elem 1)))
(module+ test
  (check-equal?
   (concrete-synth-counterpart
    (interpret-abstract-conjunction
     "integers(g1,a6),filter(g2,a1,a7),filter(g3,a2,a8),filter(g4,a3,a9),sift(a4,a10),alt_length(a11,g5)"))
   (list
    (atom 'integers (list (variable 'G1) (variable 'A6)))
    (atom 'filter (list (variable 'G2) (variable 'A1) (variable 'A7)))
    (atom 'filter (list (variable 'G3) (variable 'A2) (variable 'A8)))
    (atom 'filter (list (variable 'G4) (variable 'A3) (variable 'A9)))
    (atom 'sift (list (variable 'A4) (variable 'A10)))
    (atom 'alt_length (list (variable 'A11) (variable 'G5)))))
  (check-equal?
   (concrete-synth-counterpart
    (list
     (abstract-atom 'integers (list (g 1) (a 1)))
     (multi
      (list
       (abstract-atom* 'filterA (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2)))
       (abstract-atom* 'filterB (list (g* 1 'i 2) (a* 1 'i 2) (a* 1 'i 3))))
      #t
      (list
       (cons (a* 1 'i 1) (a 1)))
      (list
       (cons (a* 1 'i+1 1) (a* 1 'i 3)))
      (list
       (cons (a* 1 'L 3) (a 2))))
     (multi
      (list
       (abstract-atom* 'filterA (list (g* 2 'i 1) (a* 2 'i 1) (a* 2 'i 2)))
       (abstract-atom* 'filterB (list (g* 2 'i 2) (a* 2 'i 2) (a* 2 'i 3))))
      #t
      (list
       (cons
        (a* 2 'i 1)
        (abstract-function 'cons (list (g 2) (a 2)))))
      (list
       (cons (a* 2 'i+1 1) (a* 2 'i 3)))
      (list
       (cons (a* 2 'L 3) (a 3))))
     (abstract-atom 'sift (list (a 3) (a 4)))))
   (list
    (atom 'integers (list (variable 'G1) (variable 'A1)))
    (concrete-multi
     (function
      cons-symbol
      (list
       (function
        'building_block
        (list
         (function
          cons-symbol
          (list
           (function 'filterA (list (variable 'G1i1) (variable 'A1i1) (variable 'A1i2)))
           (function
            cons-symbol
            (list
             (function 'filterB (list (variable 'G1i2) (variable 'A1i2) (variable 'A1i3)))
             concrete-nil))))))
       (variable 'Tail1))))
    (concrete-multi
     (function
      cons-symbol
      (list
       (function
        'building_block
        (list
         (function
          cons-symbol
          (list
           (function 'filterA (list (variable 'G2i1) (variable 'A2i1) (variable 'A2i2)))
           (function
            cons-symbol
            (list
             (function 'filterB (list (variable 'G2i2) (variable 'A2i2) (variable 'A2i3)))
             concrete-nil))))))
       (variable 'Tail2))))
    (atom 'sift (list (variable 'A3) (variable 'A4))))))
(provide
 (proc-doc/names
  concrete-synth-counterpart
  (->
   (or/c abstract-domain-elem*? abstract-atom*? abstract-function*? abstract-variable*?)
   (or/c
    concrete-domain-elem?
    concrete-multi?
    (listof
     (or/c
      concrete-domain-elem?
      concrete-multi?))))
  (e)
  @{Converts an abstract domain element to an element which is suitable for synthesis of concrete code.
 Typically, this is a concrete domain element, but for multi this is an auxiliary structure.}))

(define (racket-listify lst)
  (match lst
    [(variable sym) lst] ; can create improper lists!
    [(function (quote \[\]) (list)) empty]
    [(function (quote \'\[\|\]\') (list-rest c1 c2))
     (cons c1 (racket-listify (first c2)))]))
(provide racket-listify)
