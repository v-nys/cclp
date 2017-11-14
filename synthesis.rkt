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
  (only-in racket/syntax format-symbol)
  racket/mock
  scribble/srcdoc
  (only-in cclp/interaction analysis-tree proceed)
  cclp/abstract-analysis
  (only-in cclp/abstract-knowledge full-evaluation?)
  cclp/concrete-domain
  cclp/concrete-knowledge
  cclp/concrete-resolve
  (only-in cclp/concrete-substitution apply-variable-substitution)
  (only-in cclp/data-utils some-v)
  (only-in cclp/domain-switching concrete-synth-counterpart)
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

(define (synthesizable-segments tree)
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
  (define sort-segments identity) ;; TODO: sort by initial node, tie-break by final numbered node
  (define ps-segments (sort-segments (set-map (synthesizable-segments ps-tree) identity)))
  (check-equal?
   (list->set (map (λ (b) (map (λ (e) (and e (label-index e))) b)) ps-segments))
   (set (list 1 2 3 #f) (list 1 2 4 5) (list 5 6 #f) (list 5 7 8 9 10 #f))))
(provide
 (proc-doc/names
  synthesizable-segments
  (-> node? set?)
  (tree)
  @{Collects the branch segments of the abstract tree to be transformed into concrete clauses.}))

; b is a list of node labels
(define (branch->clause b)
  (define (remove-at-index lst idx)
    (append
     (take lst idx)
     (drop lst (add1 idx))))
  (define (synth resolvents full-evals last-node)
    (define (atom->function a)
      (match a
        [(atom sym args)
         (function sym args)]))
    (define collected-bindings
      (append-map resolvent-substitution resolvents))
    (rule
     (apply-variable-substitution
      collected-bindings
      (atom
       (format-symbol "q~a" (label-index (first b)))
       (map atom->function (resolvent-conjunction (first resolvents)))))
     (apply-variable-substitution
      collected-bindings
      (append
       full-evals
       (cond
         [(cycle? last-node)
          (list
           (atom
            (format-symbol "q~a" (cycle-index last-node))
            (map atom->function (resolvent-conjunction (last resolvents)))))]
         [(null? (label-conjunction last-node))
          empty]
         [else
          (list
           (atom
            (format-symbol "q~a" (label-index last-node))
            (map atom->function (resolvent-conjunction (last resolvents)))))])))
     #f))
  ;; note: full evals are just atoms
  (define (extend-resolvents n acc)
    (match acc
      [(list resolvents evals selection)
       (cond
         [(rule? (tree-label-rule n))
          (let ([next-resolvent
                 (resolve
                  (resolvent-conjunction (last resolvents))
                  (some-v selection)
                  (tree-label-rule n))])
            (and next-resolvent
                 (list
                  (append resolvents (list next-resolvent))
                  evals
                  (label-selection n))))]
         [(full-evaluation? (tree-label-rule n))
          (let ([next-resolvent
                 (resolvent
                  (remove-at-index
                   (resolvent-conjunction (last resolvents))
                   (some-v selection))
                  empty)]) ;; don't need to track this substitution
            (list
             (append resolvents (list next-resolvent))
             (append
              evals
              (list
               (list-ref
                (resolvent-conjunction (last resolvents))
                (some-v selection))))
             (label-selection n)))]
         [else (error "Can't deal with this type of rule yet.")])]))
  (let* ([initial-resolvent
          (resolvent (concrete-synth-counterpart (label-conjunction (first b))) empty)]
         [numbered-nodes
          (filter (λ (n) (and (label-with-conjunction? n) (label-index n))) b)]
         [resolvents/full-evals
          (foldl
           extend-resolvents
           (list
            (list initial-resolvent)
            empty
            (label-selection (first b)))
           (cdr numbered-nodes))])
    (synth
     (first resolvents/full-evals)
     (second resolvents/full-evals)
     (last b))))
(module+ test
  (check-equal?
   (branch->clause (set-first ps-segments))
   (rule (atom 'foo empty) (list (atom 'bar empty)) #f)))