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
(require "abstract-multi-domain.rkt")
(require "abstract-knowledge.rkt")
(require "data-utils.rkt")

(require scribble/srcdoc)
(require (for-doc scribble/manual))
(module+ test
  (require rackunit
           "cclp-interpreter.rkt"))

(define (assemble-var-indices right-variable-type? abstract-data)
  (define (assemble-aux right-variable-type? abstract-data)
    (match abstract-data
      [(? abstract-variable?)
       (if (right-variable-type? abstract-data)
           (list (avar-index abstract-data)) (list))]
      [(? abstract-atom?)
       (append* (map
                 (λ (arg) (assemble-var-indices right-variable-type? arg))
                 (abstract-atom-args abstract-data)))]
      [(? abstract-function?)
       (append*
        (map
         (λ (arg) (assemble-var-indices right-variable-type? arg))
         (abstract-function-args abstract-data)))]
      [(? list?)
       (append*
        (map (λ (arg) (assemble-var-indices right-variable-type? arg)) abstract-data))]
      [(? abstract-rule?)
       (append
        (assemble-var-indices right-variable-type? (abstract-rule-head abstract-data))
        (assemble-var-indices right-variable-type? (abstract-rule-body abstract-data)))]
      [(? full-evaluation?)
       (append
        (assemble-var-indices
         right-variable-type?
         (full-evaluation-input-pattern abstract-data))
        (assemble-var-indices
         right-variable-type?
         (full-evaluation-output-pattern abstract-data)))]
      [(multi _ _ (init ic) _ (final fc))
       (assemble-var-indices right-variable-type? (map cdr (append ic fc)))]))
  (remove-duplicates (assemble-aux right-variable-type? abstract-data)))
(provide
 (proc-doc/names
  assemble-var-indices
  (->
   (-> any/c boolean?)
   (or/c abstract-domain-elem*? abstract-rule? full-evaluation?)
   (listof exact-positive-integer?))
  (pred abstract-data)
  @{Assembles the indices of the variables for which @racket[pred] passes,
 at any level in @racket[abstract-data] and returns them in left-to-right order of occurrence,
 without any duplicates.}))
(module+ test
  (check-equal? (assemble-var-indices g? (interpret-abstract-term "α1")) (list))
  (check-equal? (assemble-var-indices a? (interpret-abstract-term "γ1")) (list))
  (check-equal? (assemble-var-indices g? (interpret-abstract-term "foo(bar)")) (list))
  (check-equal? (assemble-var-indices g? (abstract-function 'bar (list))) (list))
  (check-equal?
   (assemble-var-indices g? (abstract-function 'bar (list (abstract-function 'bar'()))))
   (list))
  (check-equal? (assemble-var-indices a? (interpret-abstract-term "foo(bar)")) (list))
  (check-equal? (assemble-var-indices g? (interpret-abstract-term "foo(bar(γ1,γ2,α3,α4))")) (list 1 2))
  (check-equal? (assemble-var-indices a? (interpret-abstract-term "foo(bar(γ1,γ2,α3,α4))")) (list 3 4))
  (check-equal? (assemble-var-indices g? (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")) (list 1 2))
  (check-equal? (assemble-var-indices a? (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")) (list 3 4))

  (check-equal?
   (assemble-var-indices
    g?
    (list
     (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")
     (interpret-abstract-atom "foo(bar(γ5,γ6,α7,α8))")))
   (list 1 2 5 6))
  (check-equal?
   (assemble-var-indices
    a?
    (list
     (interpret-abstract-atom "foo(bar(γ1,γ2,α3,α4))")
     (interpret-abstract-atom "foo(bar(γ5,γ6,α7,α8))")))
   (list 3 4 7 8))

  (check-equal?
   (assemble-var-indices
    a?
    (full-evaluation (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                     (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
   (list 1 2))
  (check-equal?
   (assemble-var-indices
    g?
    (full-evaluation (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                     (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
   (list 1 2 3 4)))

(define (maximum-var-index abstraction right-variable-type?)
  (define max-of-args-accumulator
    (λ (el acc)
      (let ([subterm-max (maximum-var-index el right-variable-type?)])
        (cond [(none? acc) subterm-max]
              [(none? subterm-max) acc]
              [else (some (max (some-v acc) (some-v subterm-max)))]))))
  (match abstraction
    [(? abstract-variable?)
     (if (right-variable-type? abstraction) (some (avar-index abstraction)) (none))]
    [(or (abstract-function sym args) (abstract-atom sym args))
     (foldl max-of-args-accumulator (none) args)]
    [(? list?)
     (foldl max-of-args-accumulator (none) abstraction)]
    [(? abstract-rule?)
     (maximum-var-index (cons (abstract-rule-head abstraction) (abstract-rule-body abstraction)) right-variable-type?)]
    [(? full-evaluation?)
     (maximum-var-index (list (full-evaluation-input-pattern abstraction) (full-evaluation-output-pattern abstraction)) right-variable-type?)]
    [(multi _ _ (init ic) _ (final fc))
     (maximum-var-index (map cdr (append ic fc)) right-variable-type?)]))
(provide (contract-out [maximum-var-index (-> (or/c abstract-domain-elem*? abstract-knowledge?) (-> any/c boolean?) (maybe exact-nonnegative-integer?))]))

(module+ test
  (check-equal? (maximum-var-index (interpret-abstract-term "γ1") g?) (some 1))
  (check-equal? (maximum-var-index (interpret-abstract-term "γ1") a?) (none))
  (check-equal? (maximum-var-index (interpret-abstract-term "α2") a?) (some 2))
  (check-equal? (maximum-var-index (interpret-abstract-term "α2") g?) (none))

  (check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,α2)") g?) (some 1))
  (check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,α2)") a?) (some 2))
  (check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,γ2)") a?) (none))
  (check-equal? (maximum-var-index (interpret-abstract-term "foo(α1,α2)") g?) (none))

  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,α2)") g?) (some 1))
  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,α2)") a?) (some 2))
  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,γ2)") a?) (none))
  (check-equal? (maximum-var-index (interpret-abstract-atom "foo(α1,α2)") g?) (none)))

(define (contains-subterm? abstraction subterm)
  (match abstraction
    [(list) #f]
    [(list-rest h t) (ormap (λ (elem) (contains-subterm? elem subterm)) (cons h t))]
    [(abstract-atom sym args) (ormap (λ (arg) (contains-subterm? arg subterm)) args)]
    [(abstract-function sym args)
     (or (equal? abstraction subterm) (ormap (λ (arg) (contains-subterm? arg subterm)) args))]
    [other (equal? other subterm)]))
(provide
 (proc-doc/names
  contains-subterm?
  (-> abstract-domain-elem? abstract-term? boolean?)
  (abstraction subterm)
  @{Checks whether @racket[subterm] occurs anywhere in @racket[abstraction].}))

(module+ test
  (check-equal?
   (contains-subterm? (interpret-abstract-conjunction "bar(α2),foo(q(γ7),α1)") (g 7)) #t)
  (check-equal?
   (contains-subterm? (interpret-abstract-conjunction "bar(α2),foo(q(γ7),α1)") (g 6)) #f))

;; used so generalization of level keeps multi ID's separate
(define (extract-subscripted-variables v)
  (define (aux v)
    (match v
      [(multi conjunction _ _ _ _)
       (apply append (map extract-subscripted-variables conjunction))]
      [(or (abstract-atom* _ args) (abstract-function* _ args))
       (apply append (map extract-subscripted-variables args))]
      [(or (? g*?) (? a*?)) (list v)]))
  (remove-duplicates (aux v)))
(module+ test
  (check-equal?
   (extract-subscripted-variables
    (multi (list
            (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
            (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
           #t
           (init (list))
           (consecutive (list))
           (final (list))))
   (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2) (a* 1 'i 3))))
(provide
 (proc-doc/names
  extract-subscripted-variables
  (-> (or/c multi? abstract-atom*? abstract-function*? a*? g*?) (listof (or/c a*? g*?)))
  (v)
  @{Extracts all @racket[abstract-variable*] values in @racket[v] and returns them as a list,
 in order of occurrence, without duplicates.}))

(define (extract-variables v)
  (define (aux v)
    (match v
      [(? list?) (append-map extract-variables v)]
      [(or (abstract-atom _ args) (abstract-function _ args))
       (append-map extract-variables args)]
      [(multi _ _ (init i) _ (final f))
       (append
        (extract-variables (map cdr i))
        (extract-variables (map cdr f)))]
      [(or (? g?) (? a?)) (list v)]))
  (remove-duplicates (aux v)))
(provide extract-variables)