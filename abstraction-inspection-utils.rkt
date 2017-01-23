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
(module+ test (require rackunit
                       "cclp-interpreter.rkt"
                       ))

(define (assemble-var-indices right-variable-type? abstract-data)
  (define (assemble-aux right-variable-type? abstract-data)
    (cond [(abstract-variable? abstract-data)
           (if (right-variable-type? abstract-data)
               (list (avar-index abstract-data)) (list))]
          [(abstract-atom? abstract-data)
           (append* (map
                     (λ (arg) (assemble-var-indices right-variable-type? arg))
                     (abstract-atom-args abstract-data)))]
          [(abstract-function? abstract-data)
           (append*
            (map
             (λ (arg) (assemble-var-indices right-variable-type? arg))
             (abstract-function-args abstract-data)))]
          [(list? abstract-data)
           (append*
            (map (λ (arg) (assemble-var-indices right-variable-type? arg)) abstract-data))]
          [(abstract-rule? abstract-data)
           (append
            (assemble-var-indices right-variable-type? (abstract-rule-head abstract-data))
            (assemble-var-indices right-variable-type? (abstract-rule-body abstract-data)))]
          [(full-evaluation? abstract-data)
           (append
            (assemble-var-indices
             right-variable-type?
             (full-evaluation-input-pattern abstract-data))
            (assemble-var-indices
             right-variable-type?
             (full-evaluation-output-pattern abstract-data)))]))
  (remove-duplicates (assemble-aux right-variable-type? abstract-data)))
(provide
 (proc-doc/names
  assemble-var-indices
  (->
   (-> any/c boolean?)
   (or/c abstract-domain-elem? abstract-rule? full-evaluation? (listof (or/c abstract-domain-elem? abstract-rule? full-evaluation?)))
   (listof exact-positive-integer?))
  (pred abstract-data)
  @{Assembles the indices of the variables for which @racket[pred] passes,
 at any level in @racket[abstract-data] and returns them in left-to-right order of occurrence,
 without any duplicates.}))
(module+ test
  (check-equal? (assemble-var-indices g? (interpret-abstract-term "α1")) (list))
  (check-equal? (assemble-var-indices a? (interpret-abstract-term "γ1")) (list))
  (check-equal? (assemble-var-indices g? (interpret-abstract-term "foo(bar)")) (list))
  (check-equal? (assemble-var-indices g? (abstract-function "bar" (list))) (list))
  (check-equal?
   (assemble-var-indices g? (abstract-function "foo" (list (abstract-function "bar" '()))))
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
  (cond [(abstract-variable? abstraction)
         (if (right-variable-type? abstraction) (some (avar-index abstraction)) (none))]
        [(abstract-function? abstraction)
         (foldl max-of-args-accumulator (none) (abstract-function-args abstraction))]
        [(abstract-atom? abstraction)
         (foldl max-of-args-accumulator (none) (abstract-atom-args abstraction))]
        [(list? abstraction)
         (foldl max-of-args-accumulator (none) abstraction)]
        [(abstract-rule? abstraction)
         (maximum-var-index (cons (abstract-rule-head abstraction) (abstract-rule-body abstraction)) right-variable-type?)]
        [(full-evaluation? abstraction)
         (maximum-var-index (list (full-evaluation-input-pattern abstraction) (full-evaluation-output-pattern abstraction)) right-variable-type?)]))
(provide (contract-out [maximum-var-index (-> (or/c abstract-domain-elem? abstract-knowledge?) (-> any/c boolean?) (maybe exact-nonnegative-integer?))]))

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