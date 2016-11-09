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

(require (only-in racket-list-utils/utils findf-index))
(require racket-tree-utils/src/tree)

(require "abstract-multi-domain.rkt")
(require "abstract-analysis.rkt")
(require "generational-tree.rkt")
(require (only-in "abstract-domain-ordering.rkt" renames?))
(require (only-in "data-utils.rkt" some-v))

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (sa1-renames-sa2? branch i1 i2)
  (let* ([label-i1 (findf (λ (n) (equal? i1 (label-index n))) branch)]
         [label-i2 (findf (λ (n) (equal? i2 (label-index n))) branch)])
    (renames?
     (list-ref (label-conjunction label-i1) (some-v (label-selection label-i1)))
     (list-ref (label-conjunction label-i2) (some-v (label-selection label-i2))))))

(define (shortest-branch-with-indices indices t)
  (match indices
    [(list) (list)]
    [(list-rest i1 is)
     (let* ([t-label (node-label t)]
            [t-index (cond [(tree-label? t-label) (tree-label-index t-label)]
                           [(widening? t-label) (widening-index t-label)]
                           [else #f])]
            [rest-indices (if (equal? i1 t-index) is indices)])
       (if (not t-index)
           #f ; children will not have indices either
           (let ([child-result
                  (foldl
                   (λ (el acc) (if acc acc (shortest-branch-with-indices rest-indices el)))
                   #f
                   (node-children t))])
             (if child-result (cons t-label child-result) #f))))]
    [_ #f]))
(provide
 (proc-doc/names
  shortest-branch-with-indices
  (-> (listof exact-nonnegative-integer?) node? (listof (or/c tree-label? widening?)))
  (indices tree)
  @{Find the shortest branch in @racket[tree]
 which has nodes with all indices in @racket[indices], in the correct order.}))

(define (dp-zero-subtree-depth-complement-at-level dp gen-tree lvl)
  (define gen-tree-lvl (horizontal-level gen-tree lvl))
  (list))

(define (find-dp-zero-subtrees-depths-complements dp gen-tree)
  (define gt-depth (node-depth gen-tree))
  (define levels (range 0 (+ gt-depth 1)))
  (apply append (map (curry dp-zero-subtree-depth-complement-at-level dp gen-tree) levels)))
(provide
 (proc-doc/names
  find-dp-zero-subtrees-depths-complements
  (-> abstract-atom? node? list?)
  (dp generational-tree)
  @{Find subtrees of the generational tree @racket[generational-tree] with an exact occurrence
 of abstract atom @racket[dp] of generation 0 at their root, along with the depth of this root and
 with subtrees at the same level in @racket[generational-tree].
 Note that the latter may theoretically contain a subtree with an exact occurrence, as well.}))

; subtrees: dp subtree with generation 0 at given depth
(define (checks-involving-generations ls1 ls2 gs1 gs2 subtree-depth-complement)
  ; note that complement means all trees at the same depth as that for dp!
  (match subtree-depth-complement
    [(list subtree depth complement)
     (begin
       (define subset-s1-with-gen (horizontal-level subtree (- ls1 depth)))
       (define subset-s2-with-gen (horizontal-level subtree (- ls2 depth)))
       (and
         (three-generation-correspondence gs1 gs2 subset-s1-with-gen subset-s2-with-gen)
         #f))]))

(define (three-generation-correspondence gs1 gs2 subset-s1-with-gen subset-s2-with-gen)
  (define
    three-generation-conjunction-1
    (filter
     (λ (a-g) (<= (- gs1 1) (atom-with-generation-generation a-g) (+ gs1 1)))
     subset-s1-with-gen))
  (define
    three-generation-conjunction-2
    (filter
     (λ (a-g) (<= (- gs2 1) (atom-with-generation-generation a-g) (+ gs2 1)))
     subset-s2-with-gen))
  (and
   (renames?
    (map atom-with-generation-atom three-generation-conjunction-1)
    (map atom-with-generation-atom three-generation-conjunction-2))
   (equal?
    (map (compose (curry - gs1) atom-with-generation-generation) three-generation-conjunction-1)
    (map (compose (curry - gs2) atom-with-generation-generation) three-generation-conjunction-2))))

(define (s-similar? node-index-1 node-index-2 tree)
  (define branch (shortest-branch-with-indices (list node-index-1 node-index-2) tree))
  (define skeleton (car (generational-tree-skeleton branch))) ; top-level is an atom anyway
  (define candidate-targets (candidate-target-atoms skeleton (- (length branch) 1)))
  (define all-generational-trees (generational-trees branch))
  (define ls1 (findf-index (λ (l) (equal? (label-index l) node-index-1)) branch))
  (define ls2 (findf-index (λ (l) (equal? (label-index l) node-index-2)) branch))
  (and
   (sa1-renames-sa2? branch node-index-1 node-index-2)
   (ormap ; s-similarity for any candidate target atom is enough
    (λ (dp gt)
      (let* ([annotated-s1 (horizontal-level gt ls1)]
             [annotated-s2 (horizontal-level gt ls2)]
             [dp-zero-subtrees-depths-complements (find-dp-zero-subtrees-depths-complements dp gt)]
             [gs1
              (atom-with-generation-generation
               (list-ref annotated-s1 (some-v (label-selection (list-ref branch ls1)))))]
             [gs2
              (atom-with-generation-generation
               (list-ref annotated-s2 (some-v (label-selection (list-ref branch ls2)))))])
        (ormap
          (curry checks-involving-generations ls1 ls2 gs1 gs2)
          dp-zero-subtrees-depths-complements)))
    candidate-targets
    all-generational-trees)))
(provide
 (proc-doc/names
  s-similar?
  (->
   exact-positive-integer?
   exact-positive-integer?
   node?
   boolean?)
  (node-index-1 node-index-2 analysis-tree)
  @{Determines whether the node with index @racket[node-index-2] is s-similar
 to the node with index @racket[node-index-1] in @racket[analysis-tree].
 The caller is responsible for checking that tree has a branch with the supplied indices.}))