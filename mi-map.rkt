#lang racket
; MIT License
;
; Copyright (c) 2017 Vincent Nys
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

(require (only-in racket/syntax format-symbol)
         racket-tree-utils/src/tree
         "abstract-analysis.rkt"
         (prefix-in ak: "abstract-knowledge.rkt")
         (only-in "abstraction-inspection-utils.rkt"
                  extract-variables/duplicates
                  extract-subscripted-variables/duplicates)
         (prefix-in ck: "concrete-knowledge.rkt")
         "abstract-multi-domain.rkt"
         "cclp-interpreter.rkt")

(define (mi-map-visit-from idx n)
  (match n
    [(node (tree-label (list) _ _ (ck:rule _ _ rule-idx) #f _) _)
     (displayln (format "transition_from_to_via(~a,empty,rule~a)." idx rule-idx))]
    [(node (tree-label (list) _ _ (ak:full-evaluation _ _ rule-idx) #f _) _)
     (displayln (format "transition_from_to_via(~a,empty,fullai~a)." idx rule-idx))]
    [(node (tree-label _ _ _ (ck:rule _ _ rule-idx) idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,rule~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ (ak:full-evaluation _ _ rule-idx) idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,fullai~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ 'one idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,one)." idx idx2))]
    [(node (tree-label _ _ _ 'many idx2 _) _)
     (displayln (format "transition_from_to_via(~a,~a,many)." idx idx2))]
    [(node (generalization _ _ idx2 _ _) _)
     (displayln (format "generalization_from_to(~a,~a)" idx idx2))]
    [(node (cycle cycle-idx) _)
     (displayln (format "cycle_from_to(~a,~a)." idx cycle-idx))]
    [else
     (displayln (format "don't know how to be visited yet"))]))

(define (mi-map-visitor n)
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (for ([ch children])
       (mi-map-visit-from (label-index label) ch))]
    [(node (cycle idx) (list)) (void)]
    [_ (displayln (format "don't know how to visit ~a yet" n))]))

(define (display-mi-map tree)
  (visit mi-map-visitor tree)
  tree)
(provide display-mi-map)

(define (untangle init-ac building-blocks)
  (define (find-max-vars occ acc)
    (define symbolic-constructor (symbolize-avar-constructor occ))
    (define current-max (hash-ref acc symbolic-constructor #f))
    (define occ-local-index
      (if (abstract-variable? occ)
          (avar-index occ)
          (avar*-local-index occ)))
    (if (or (not current-max)
            (> occ-local-index current-max))
        (hash-set acc symbolic-constructor occ-local-index)
        acc))
  (define (maybe-rename-occurrence e acc)
    (list))
  (define (rename-first-occurrences ac occurrence-renamings)
    ac)
  (define var-occurrences
    (append
     (extract-subscripted-variables/duplicates init-ac)
     (extract-variables/duplicates init-ac)))
  (define max-var-indices
    (foldl find-max-vars (hash) var-occurrences))
  (define occurrence-renamings
    (foldl maybe-rename-occurrence `(() ,(hash) ,max-var-indices)))
  (rename-first-occurrences init-ac occurrence-renamings))

;; synthesis of generalization/2 head does not need info about generations or internal aliasing
(struct simplified-multi (conjunction init final))

(define (simplify-multis acon)
  (define (simplify m)
    (match m
      [(multi conj a? i cons f)
       (simplified-multi conj i f)]
      [_ m]))
  (map simplify acon))

(module+ test
  (require rackunit)
  (check-equal?
   (untangle
    (interpret-abstract-conjunction "integers(γ1,α1),filter(γ2,α1,α2),filter(γ3,α2,α3),filter(γ4,α3,α4),sift(α4,α5),length(α5,γ5)")
    (list
     (cons 1 1)
     (cons 2 2)))
   (list
    (interpret-abstract-conjunction "integers(γ1,α1),filter(γ2,α6,α2),filter(γ3,α7,α3),filter(γ4,α8,α4),sift(α9,α5),length(α10,γ5)")
    (list (cons (a 1) (a 6)) (cons (a 2) (a 7)) (cons (a 3) (a 8)) (cons (a 4) (a 9)) (cons (a 5) (a 10)))))
  (check-equal?
   (untangle
    (list
     (abstract-atom 'integers (list (g 1) (a 1)))
     (abstract-atom 'filter (list (g 2) (a 1) (a 2)))
     (multi
      (list
       (abstract-atom*
        'filter
        (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init (list (cons (a* 1 1 1) (a 2))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final (list (cons (a* 1 'L 2) (a 3)))))
     (abstract-atom 'filter (list (g 3) (a 3) (a 4)))
     (abstract-atom 'sift (list (a 4) (a 5)))
     (abstract-atom 'length (list (a 5) (g 4))))
    (list (cons 1 1) (cons 2 2)))
   (list
    (list
     (abstract-atom 'integers (list (g 1) (a 1)))
     (abstract-atom 'filter (list (g 2) (a 6) (a 2)))
     (multi
      (list
       (abstract-atom*
        'filter
        (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init (list (cons (a* 1 1 1) (a 7))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final (list (cons (a* 1 'L 2) (a 3)))))
     (abstract-atom 'filter (list (g 3) (a 8) (a 4)))
     (abstract-atom 'sift (list (a 9) (a 5)))
     (abstract-atom 'length (list (a 10) (g 4))))
    (list (cons (a 1) (a 6)) (cons (a 2) (a 7)) (cons (a 3) (a 8)) (cons (a 4) (a 9)) (cons (a 5) (a 10)))))
  (check-equal?
   (untangle
    (append
     (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),append(α1,α2,α3),collect(γ3,α4),append(α3,α4,α5)")
     (cons
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
       #f
       (init (list (cons (a* 1 1 2) (a 5))))
       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 3))))
       (final (list (cons (a* 1 'L 3) (a 6)))))
      (interpret-abstract-conjunction "collect(γ4,α7),eq(α6,α7)")))
    (list
     (cons 4 5) ; the second collect-append pair
     (cons 6 6))) ; the existing multi
   (list
    (append
     (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),append(α8,α9,α3),collect(γ3,α4),append(α10,α11,α5)")
     (cons
      ;; TODO: how to deal with aliasing inside abstracted pattern?
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 4) (a* 1 'i 3)))) ; !!
       #f
       (init (list (cons (a* 1 1 2) (a 12))))
       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 3))))
       (final (list (cons (a* 1 'L 3) (a 6)))))
      (interpret-abstract-conjunction "collect(γ4,α7),eq(α13,α7)")))
    (list (cons (a 1) (a 8)) (cons (a 2) (a 9)) (cons (a 3) (a 10)) (cons (a 4) (a 11)) (cons (a 5) (a 12)) (cons (a 6) (a 13))))))
;; TODO: needs more tests for the annoying scenarios, e.g. multiple multis, multi like in graph coloring,...

(define (generate-generalization-clause x y)
  (display "not implemented yet"))

;; auxiliary function for untangle
;; allows comparison of constructors
(define (symbolize-avar-constructor abstract-var)
  (match abstract-var
    [(a _) 'a]
    [(g _) 'g]
    [(a* id 'i j)
     (format-symbol "a-~a-i" id)]
    [(g* id 'i j)
     (format-symbol "g-~a-i" id)]))
(module+ test
  (check-equal?
   (symbolize-avar-constructor (a* 2 'i 3))
   'a-2-i))

(define (generalization-clause-visit-from conjunction1 n)
  (match n
    [(node (generalization conjunction2 _ _ _ _) _)
     (display
      (generate-generalization-clause
       conjunction1
       conjunction2))]
    [_ (void)]))

(define (generalization-clause-visitor n)
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (for ([ch children])
       (generalization-clause-visit-from (label-conjunction label) ch))]
    [(node (cycle idx) (list)) (void)]
    [_ (displayln (format "don't know how to visit ~a yet" n))]))

(define (display-generalization-clauses tree)
  (visit generalization-clause-visitor tree)
  tree)
(provide display-generalization-clauses)