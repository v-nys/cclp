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

(require racket/set
         (only-in racket/syntax format-symbol)
         racket-tree-utils/src/tree
         "abstract-analysis.rkt"
         (prefix-in ak: "abstract-knowledge.rkt")
         (only-in "abstraction-inspection-utils.rkt"
                  extract-all-variables/duplicates)
         (prefix-in ck: "concrete-knowledge.rkt")
         "abstract-multi-domain.rkt"
         "cclp-interpreter.rkt"
         (only-in "control-flow.rkt" aif it))

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

(define (rename-occurrence replacee replacer locus)
  (define (compound-constructor l)
    (match l
      [(abstract-atom _ _) abstract-atom]
      [(abstract-function _ _) abstract-function]
      [(abstract-atom* _ _) abstract-atom*]
      [(abstract-function* _ _) abstract-function*]))
  (define rename (curry rename-occurrence replacee replacer))
  (match locus
    [(or
      (? abstract-variable?)
      (? abstract-variable*?))
     (if
      (equal? locus replacee)
      `(,replacer . #t)
      `(,locus . #f))]
    [(or
      (abstract-atom sym args)
      (abstract-function sym args)
      (abstract-atom* sym args)
      (abstract-function* sym args))
     (match-let
         ([constructor (compound-constructor locus)]
          [(cons after success?)
           (rename args)])
       (cons (constructor sym after) success?))]
    [(multi patt asc? (init ic) consec (final fc))
     (match-let
         ([(cons after-1 success-1?)
           (rename patt)]
          [(cons after-2 success-2?)
           (rename (map cdr ic))]
          [(cons after-3 success-3?)
           (rename (map cdr fc))]
          [zip (λ (l1 l2) (map (λ (e1 e2) (cons e1 e2)) l1 l2))])
       (cond [success-1?
              (cons (multi after-1 asc? (init ic) consec (final fc)) success-1?)]
             [success-2?
              (cons (multi patt asc? (init (zip (map car ic) after-2)) consec (final fc)) success-2?)]
             [success-3?
              (cons (multi patt asc? (init ic) consec (final (zip (map car fc) after-3))) success-3?)]
             [else (cons locus #f)]))]
    [(list)
     (cons locus #f)]
    [(list-rest h t)
     (match-let
         ([(cons after success?)
           (rename h)])
       (if success?
           (cons (cons after t) success?)
           (match-let
               ([(cons renamed-t eventual-success?)
                 (rename t)])
             (cons
              (cons h renamed-t)
              eventual-success?))))]))
(module+ test
  (check-equal?
   (rename-occurrence
    (a 3)
    (a 101)
    (interpret-abstract-conjunction
     "foo(γ1,α1),bar(γ2,α2),baz(γ3,α3),quux(γ3,α3),poit(γ4,α4)"))
   (cons
    (interpret-abstract-conjunction
     "foo(γ1,α1),bar(γ2,α2),baz(γ3,α101),quux(γ3,α3),poit(γ4,α4)")
    #t)))
                      

(define (untangle init-ac building-blocks)
  (define (rename-occurrences locus aliases)
    (match aliases
      [(list) locus]
      [(list-rest (cons replacee replacer) tail)
       (rename-occurrences
        (car (rename-occurrence replacee replacer locus))
        tail)]))
  (define (extract-avar-constructor e)
    (match e
      [(a _) a]
      [(g _) g]
      [(a* i j _) (curry a* i j)]
      [(g* i j _) (curry g* i j)]))
  (define (local-index v)
    (if (abstract-variable? v)
        (avar-index v)
        (avar*-local-index v)))
  (define (find-max-vars occ acc)
    (define symbolic-constructor
      (symbolize-avar-constructor occ))
    (define current-max
      (aif (hash-ref acc symbolic-constructor #f)
           (local-index it)
           #f))
    (if (or (not current-max)
            (> (local-index occ) current-max))
        (hash-set acc symbolic-constructor occ)
        acc))
  (define (maybe-map-occurrence e acc)
    (match acc
      [(list aliases encountered maxima)
       (if (set-member? encountered e)
           (let* ([constructor (extract-avar-constructor e)]
                  [symbolized-constructor (symbolize-avar-constructor e)]
                  [current-max (local-index (hash-ref maxima symbolized-constructor))])
             (list
              (cons
               `(,e . ,(constructor (add1 current-max)))
               aliases)
              encountered
              (hash-set
               maxima
               symbolized-constructor
               (constructor (add1 current-max)))))
           (list aliases (set-add encountered e) maxima))]))
  (define (rename-first ac occurrence-renamings)
    ac)
  (define var-occurrences
    (extract-all-variables/duplicates init-ac))
  (define max-var-indices
    (foldl find-max-vars (hash) var-occurrences))
  (define occurrence-renamings
    (reverse
     (first
      (foldl maybe-map-occurrence `(() ,(set) ,max-var-indices) var-occurrences))))
  (list
   (rename-occurrences init-ac occurrence-renamings)
   occurrence-renamings))

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
    (interpret-abstract-conjunction "integers(γ1,α6),filter(γ2,α1,α7),filter(γ3,α2,α8),filter(γ4,α3,α9),sift(α4,α10),length(α5,γ5)")
    (list
     (cons (a 1) (a 6))
     (cons (a 2) (a 7))
     (cons (a 3) (a 8))
     (cons (a 4) (a 9))
     (cons (a 5) (a 10)))))
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
     (abstract-atom 'integers (list (g 1) (a 6)))
     (abstract-atom 'filter (list (g 2) (a 1) (a 7)))
     (multi
      (list
       (abstract-atom*
        'filter
        (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init (list (cons (a* 1 1 1) (a 2))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final (list (cons (a* 1 'L 2) (a 8)))))
     (abstract-atom 'filter (list (g 3) (a 3) (a 9)))
     (abstract-atom 'sift (list (a 4) (a 10)))
     (abstract-atom 'length (list (a 5) (g 4))))
    (list
     (cons (a 1) (a 6))
     (cons (a 2) (a 7))
     (cons (a 3) (a 8))
     (cons (a 4) (a 9))
     (cons (a 5) (a 10)))))
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
     (interpret-abstract-conjunction "collect(γ1,α8),collect(γ2,α9),append(α1,α2,α10),collect(γ3,α11),append(α3,α4,α12)")
     (cons
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 4)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3)))) ; !!
       #f
       (init (list (cons (a* 1 1 2) (a 5))))
       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 3))))
       (final (list (cons (a* 1 'L 3) (a 13)))))
      (interpret-abstract-conjunction "collect(γ4,α14),eq(α6,α7)")))
    (list
     (cons (a 1) (a 8))
     (cons (a 2) (a 9))
     (cons (a 3) (a 10))
     (cons (a 4) (a 11))
     (cons (a* 1 'i 1) (a* 1 'i 4))
     (cons (a 5) (a 12))
     (cons (a 6) (a 13))
     (cons (a 7) (a 14))))))
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