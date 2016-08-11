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

#lang typed/racket
(struct a ([index : Integer]) #:transparent) ; may generalize later by allowing for double indices
(provide (struct-out a))
(struct g ([index : Integer]) #:transparent)
(provide (struct-out g))

(require "data-utils.rkt") ; for optional set union

(define-type AbstractVariable (U a g))
(provide AbstractVariable)
(define-predicate AbstractVariable? AbstractVariable)
(provide AbstractVariable?)

(: avar-index (-> AbstractVariable Integer))
(define (avar-index avar)
  (cond [(a? avar) (a-index avar)]
        [(g? avar) (g-index avar)]))
(provide avar-index)

(struct abstract-function ([functor : String] [args : (Listof AbstractTerm)]) #:transparent)
(provide (struct-out abstract-function))
(define-type AbstractTerm (U AbstractVariable abstract-function))
(provide AbstractTerm)
(define-predicate AbstractTerm? AbstractTerm)
(provide AbstractTerm?)

(struct abstract-atom ([symbol : String] [args : (Listof AbstractTerm)]) #:transparent)
(provide (struct-out abstract-atom))
(define-type AbstractConjunct abstract-atom)
(provide AbstractConjunct)
(define-predicate AbstractConjunct? AbstractConjunct)
(provide AbstractConjunct?)

(define-type AbstractConjunction (Listof AbstractConjunct))
(provide AbstractConjunction)
(define-predicate AbstractConjunction? AbstractConjunction)
(provide AbstractConjunction?)

(define-type AbstractDomainElem (U AbstractTerm AbstractConjunct AbstractConjunction))
(provide AbstractDomainElem)

; may want to move this outside of core domain?
(: assemble-var-indices (-> (-> AbstractVariable Boolean) AbstractDomainElem (Setof Integer)))
(define (assemble-var-indices right-variable-type? domain-elem)
  (cond [(AbstractVariable? domain-elem) (if (right-variable-type? domain-elem) (set (avar-index domain-elem)) (set))]
        [(abstract-atom? domain-elem) (apply optional-set-union (map (λ ([arg : AbstractTerm]) (assemble-var-indices right-variable-type? arg)) (abstract-atom-args domain-elem)))]
        [(abstract-function? domain-elem) (apply optional-set-union (map (λ ([arg : AbstractTerm]) (assemble-var-indices right-variable-type? arg)) (abstract-function-args domain-elem)))]
        [(AbstractConjunction? domain-elem) (apply optional-set-union (map (λ ([arg : abstract-atom]) (assemble-var-indices right-variable-type? arg)) domain-elem))]))
(provide assemble-var-indices)