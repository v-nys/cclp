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
(require "abstract-multi-domain.rkt")
(require "abstract-multi-domain-sexp-conversion.rkt")
(require "abstract-domain-ordering.rkt")
(require "abstract-knowledge.rkt")
(require (only-in racket-list-utils/utils findf-index))
(require parenlog)
(require racket/set)

(define (is-valid? prior)
  (let ([counter-examples (query-model prior (violates_partial_order))])
    (< (length counter-examples) 1)))
(provide (contract-out [is-valid? (-> model? boolean?)]))

(define (unique-atoms conjunction)
  (reverse
   (foldl
    (λ (at acc)
      (let ([renaming (findf (λ (x) (renames? x at)) acc)])
        (if renaming acc (cons at acc))))
    (list)
    conjunction)))

(define (selected-index conjunction prior full-ai-rules)
  (log-debug "looking for selected index")
  (define full-eval-index
    (foldl
     (λ (r acc)
       (if acc
           acc
           (findf-index (λ (atom) (>=-extension (full-evaluation-input-pattern r) atom)) conjunction)))
     #f
     full-ai-rules))
  (if full-eval-index
      full-eval-index
      (let* ([sexp-conjunction (abstract-domain-elem->sexp (unique-atoms conjunction))]
             [query (list 'member_reaches_or_includes_all_under_consistency 'X sexp-conjunction)]
             [outcomes (query-model* prior (i/query query) #:limit 1)])
        (begin
          (log-debug "found topmost atom type: ~v" outcomes)
          (if (null? outcomes)
              (error "Partial order is underspecified.")
              (begin
                (let ([sexp-renaming-of-selection (hash-ref (car outcomes) 'X)])
                  (findf-index
                   (λ (atom) (renames? atom (sexp->abstract-atom sexp-renaming-of-selection)))
                   conjunction))))))))

; contract could be more specific (range is from 0 to length of the list...), but can wait
(provide (contract-out [selected-index (-> (listof abstract-atom?) model? (listof full-evaluation?) natural-number/c)]))