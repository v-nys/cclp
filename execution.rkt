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
(require racket/set
         scribble/srcdoc)

(require "abstract-multi-domain.rkt")
(require "abstract-domain-ordering.rkt")
(require "abstract-knowledge.rkt")
(require "preprior-graph.rkt")
(require "abstract-renaming.rkt")
(require (only-in racket-list-utils/utils findf-index))
(require (only-in "multi-unfolding.rkt" unfold-multi-bounded))
(require (only-in "abstraction-inspection-utils.rkt" assemble-var-indices))
(require graph)

(require (for-doc scribble/manual))
(module+ test (require rackunit "cclp-interpreter.rkt"))

(define (unique-atoms conjunction)
  (reverse
   (foldl
    (λ (at acc)
      (let ([renaming (findf (λ (x) (renames? x at)) acc)])
        (if renaming acc (cons at acc))))
    (list)
    conjunction)))
(module+ test
  (let* ([original (interpret-abstract-conjunction "foo(γ1,α1),bar(γ2,α2)")]
         [filtered (unique-atoms original)])
    (check-equal? filtered original))
  (let* ([st "foo(γ1,α1),bar(γ2,α2),foo(γ1,α1),bar(γ2,α2)"]
         [original (interpret-abstract-conjunction st)]
         [filtered (unique-atoms original)])
    (check-equal?
     filtered
     (interpret-abstract-conjunction "foo(γ1,α1),bar(γ2,α2)"))))
(provide
 (proc-doc/names
  unique-atoms
  (-> (listof abstract-atom?) (listof abstract-atom?))
  (conjunction)
  @{Returns a list of all atoms in @racket[conjunction] which are either the first or only representative of their equivalence class.}))

(define (selected-index conjunction preprior full-ai-rules)
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
      (let* ([multis (filter multi? conjunction)]
             [multi-conjuncts (apply append (map (λ (m) (let ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) m)))]) (car (unfold-multi-bounded 1 m offset offset)))) multis))]
             [unique (unique-atoms (map normalize-abstract-atom (append (filter abstract-atom? conjunction) multi-conjuncts)))]
             ; note: tc always has reachability of self, even when there are no self loops!
             [tc (transitive-closure preprior)]
             [first-choice
              (findf
               (λ (aa1)
                 (andmap
                  (λ (aa2)
                    (hash-ref tc (list aa1 aa2) #f))
                  unique))
               unique)])
        (begin
          (if (not first-choice)
              #f
              (findf-index (λ (c) (if (abstract-atom? c) (renames? c first-choice) (ormap (λ (mc) (renames? mc first-choice)) (let ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) c)))]) (car (unfold-multi-bounded 1 c offset offset)))))) conjunction))))))
(module+ test
  (let* ([preprior (mk-preprior-graph)]
         [conjunction (interpret-abstract-conjunction "foo(γ1,α1)")])
    (begin
      (add-vertex! preprior (first conjunction))
      (check-equal? (selected-index conjunction preprior '()) 0)))
  (let* ([preprior (mk-preprior-graph)]
         [conjunction (interpret-abstract-conjunction "foo(γ1,α1),bar(γ2,α2)")])
    (begin
      (add-vertex! preprior (first conjunction))
      (add-vertex! preprior (second conjunction))
      (check-equal? (selected-index conjunction preprior '()) #f)))
  (let* ([preprior (mk-preprior-graph)]
         [conjunction1 (interpret-abstract-conjunction "foo(γ1,α1),bar(γ2,α2)")]
         [conjunction2 (interpret-abstract-conjunction "foo(γ3,α3),bar(γ4,α4)")])
    (begin
      (add-vertex! preprior (first conjunction1))
      (add-vertex! preprior (second conjunction1))
      (add-directed-edge! preprior (first conjunction1) (second conjunction1))
      (check-equal? (selected-index conjunction2 preprior '()) 0)))
  (let* ([preprior (mk-preprior-graph)]
         [conjunction (interpret-abstract-conjunction "foo(γ1,α1),bar(γ2,α2)")])
    (begin
      (add-vertex! preprior (first conjunction))
      (add-vertex! preprior (second conjunction))
      (add-directed-edge! preprior (second conjunction) (first conjunction))
      (check-equal? (selected-index conjunction preprior '()) 1)))
  (let* ([preprior (mk-preprior-graph)]
         [conjunction (interpret-abstract-conjunction "foo(α1),foo(γ1),foo(nil)")])
    (begin
      (add-vertex! preprior (first conjunction))
      (add-vertex! preprior (second conjunction))
      (add-vertex! preprior (third conjunction))
      (check-equal? (selected-index conjunction preprior '()) 2)))
  (let ([conjunction (interpret-abstract-conjunction "foo(α1),bar(γ1)")]
        [full-ai
         (list
          (full-evaluation
           (interpret-abstract-atom "bar(γ1)")
           (interpret-abstract-atom "bar(nil)")))])
    (check-equal?
     (selected-index conjunction (mk-preprior-graph) full-ai)
     1)))
; contract could be more specific (range is from 0 to length of the list...), but can wait
(provide
 (proc-doc/names
  selected-index
  (-> (listof abstract-conjunct?) preprior-graph? (listof full-evaluation?) (or/c #f natural-number/c))
  (conjunction preprior full-ai-rules)
  @{Selects an abstract conjunct from @racket[conjunction] based on the ordering generated by @racket[preprior],
 or a fully evaluatable abstract atom based on @racket[full-ai-rules].
 If neither source provides sufficient information to make a choice, this function returns @racket[#f].}))