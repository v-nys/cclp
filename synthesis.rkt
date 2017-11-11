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
#lang at-exp racket
(require
  scribble/srcdoc
  (only-in cclp/interaction analysis-tree proceed)
  cclp/abstract-analysis
  cclp/concrete-domain
  racket-tree-utils/src/tree)
(require (for-doc scribble/manual))

(define (A-nodes tree)
  (define (A-nodes* tree)
    (match tree
      [(node (cycle i) (list))
       (set i)]
      [(node lbl ch)
       (apply set-union (set) (map A-nodes* ch))]))
  (match tree
    [(node lbl (list-rest c cs))
     (apply
      set-union
      (set (label-index lbl))
      (map A-nodes* (cons c cs)))]
    [(node lbl (list))
     (set)]))
(module+ test
  (require
    rackunit
    repeated-application
    (prefix-in permsort: cclp-programs/permutation-sort))
  (define ps-tree
    (analysis-tree
     (apply↑* proceed permsort:initial-program-analysis)))
  (check-equal?
   (A-nodes ps-tree)
   (set 1 5)))
(provide
 (proc-doc/names
  A-nodes
  (-> node? set?)
  (tree)
  @{Collects the indices of all nodes in @racket[tree] which can correspond to a synthesized state.}))

(define (synthesizable-branches tree)
  (define (branches-from from endpoint-indices)
    (let* ([from-node
            (first
             (subtree-filter
              tree
              (λ (st)
                (and
                 (label-with-conjunction? (node-label st))
                 (equal?
                  from
                  (label-index
                   (node-label st)))))))]
           [from-ch (node-children from-node)])
      (for/fold ([flat-set (set)])
                ([ch from-ch])
        (cond
          [(and (label-with-conjunction? (node-label ch))
                (null? (label-conjunction (node-label ch))))
           (set-add flat-set (list (node-label from-node) (node-label ch)))]
          [(and (label-with-conjunction? (node-label ch))
                (set-member? endpoint-indices (label-index (node-label ch))))
           (set-add flat-set (list (node-label from-node) (node-label ch)))]
          [(label-with-conjunction? (node-label ch))
           (set-union
            flat-set
            (list->set
             (set-map
              (branches-from (label-index (node-label ch)) endpoint-indices)
              (λ (b) (cons (node-label from-node) b)))))]
          [(cycle? (node-label ch))
           (set-add flat-set (list (node-label from-node) #f))]
          [else flat-set]))))
  (let ([A (A-nodes tree)])
    (for/fold ([flat-set (set)])
              ([node-set 
                (list->set
                 (set-map
                  A
                  (λ (n)
                    (branches-from n A))))])
      (set-union flat-set node-set))))
(module+ test
  (check-equal?
   (list->set (set-map (synthesizable-branches ps-tree) (λ (b) (map (λ (e) (and e (label-index e))) b))))
   (set (list 1 2 3 #f) (list 1 2 4 5) (list 5 6 #f) (list 5 7 8 9 10 #f))))
(provide
 (proc-doc/names
  synthesizable-branches
  (-> node? set?)
  (tree)
  @{Collects the branch segments of the abstract tree to be transformed into concrete clauses.}))