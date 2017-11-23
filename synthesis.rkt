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
  mock
  racket/generator
  scribble/srcdoc
  (only-in cclp/interaction analysis-tree proceed)
  cclp/abstract-analysis
  (only-in cclp/abstract-knowledge full-evaluation?)
  cclp/concrete-domain
  cclp/concrete-knowledge
  cclp/concrete-resolve
  (only-in cclp/mi-map synth-str)
  (only-in cclp/concrete-substitution apply-variable-substitution)
  (only-in cclp/data-utils some-v)
  cclp/domain-switching
  cclp/gen-graph-structs
  racket-tree-utils/src/tree)
(require (for-doc scribble/manual))

;; FIXME: bad name, also does concrete multi
(define (atom->function a)
  (match a
    [(atom sym args)
     (function sym args)]))

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
    (prefix-in permsort: cclp-programs/permutation-sort)
    (prefix-in primes: cclp-programs/primes))
  (define ps-tree
    (analysis-tree
     (apply↑* proceed permsort:initial-program-analysis)))
  (define primes-tree
    (analysis-tree
     (apply↑* proceed primes:initial-program-analysis)))
  (check-equal?
   (A-nodes ps-tree)
   (set 1 5))
  (check-equal?
   (A-nodes primes-tree)
   (set 1 3 9 11 20 27 22 36 43 47 38 59 57 45 66 73 79 55 84 72 82 91 95)))
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
           (set-add flat-set (list (node-label from-node) (node-label ch)))]
          [else flat-set]))))
  (let ([A (A-nodes tree)])
    (for/fold ([flat-set (set)]) ; the outcome: a set of synthesizable segments
              ([node-set 
                (list->set
                 (set-map
                  A
                  (λ (n)
                    (branches-from n A))))])
      (set-union flat-set node-set))))
(module+ test
  (define ps-segments (sort-segments (set-map (synthesizable-segments ps-tree) identity)))
  (check-equal?
   (list->set (map (λ (b) (map (λ (e) (and e (if (label-with-conjunction? e) (label-index e) e))) b)) ps-segments))
   (set (list 1 2 3 #f) (list 1 2 4 5) (list 5 6 #f) (list 5 7 8 9 10 (cycle 5))))
  (define primes-segments (sort-segments (set-map (synthesizable-segments primes-tree) identity))))
(provide
 (proc-doc/names
  synthesizable-segments
  (-> node? set?)
  (tree)
  @{Collects the branch segments of the abstract tree to be transformed into concrete clauses.}))

(define (sort-segments segments)
  (define (segment-lt s1 s2)
    (or
     ;; this never applies to full segments, can only occur in recursive case
     (and (or (not (label-index (first s1))) (not (label-index (first s2))))
          (< (rule-idx (first s1)) (rule-idx (first s2))))
     ;; segments with different roots
     (< (label-index (first s1)) (label-index (first s2)))
     ;; segments with identical, non-false roots
     (and
      (label-index (first s1))
      (equal? (label-index (first s1)) (label-index (first s2)))
      (segment-lt (cdr s1) (cdr s2)))))
  (sort segments segment-lt))
(provide sort-segments)

(define (remove-at-index lst idx)
    (append
     (take lst idx)
     (drop lst (add1 idx))))

;; TODO: extend so this works with multi
; b is a list of node labels
(define (branch->clause b [gensym gensym])
  (struct con/sub (con sub)) ;; conjunction + substitution
  (define (make-wrappable e)
    (match e
      [(atom sym args) (function sym args)]
      [(concrete-multi lst) (function 'multi (list lst))]
      [else (error "Can't wrap this in a Q state." e)]))
  (define (resolvent->con/sub r)
    (match r [(resolvent c s) (con/sub c s)]))
  (define (synth con/subs full-evals last-node)
    (define collected-bindings
      (append-map con/sub-sub con/subs))
    (rule
     (apply-variable-substitution
      collected-bindings
      (atom
       (format-symbol "q~a" (label-index (first b)))
       (map make-wrappable (con/sub-con (first con/subs)))))
     (apply-variable-substitution
      collected-bindings
      (append
       full-evals
       (cond
         [(cycle? last-node)
          (list
           (atom
            (format-symbol "q~a" (cycle-index last-node))
            (map make-wrappable (con/sub-con (last con/subs)))))]
         [(null? (label-conjunction last-node))
          empty]
         [else
          (list
           (atom
            (format-symbol "q~a" (label-index last-node))
            (map make-wrappable (con/sub-con (last con/subs)))))])))
     #f))
  ;; note: full evals are just atoms
  (define (extend-con/subs n acc) ;; here, acc is a list of con/subs
    (define (append-potential-multi-elems conjunct idx acc)
      (cond
        [(and
          (findf (λ (r) (= idx (index-range-start r))) (generalization-abstracted-ranges n))
          (atom? conjunct))
         (append acc (list (concrete-multi (concrete-listify (list (atom->function conjunct))))))]
        [(and
          (findf (λ (r) (= idx (index-range-start r))) (generalization-abstracted-ranges n))
          (concrete-multi? conjunct))
         (append acc (list conjunct))]
        [(and
          (findf (λ (r) (and (> idx (index-range-start r)) (< idx (index-range-end-before r)))) (generalization-abstracted-ranges n))
          (atom? conjunct))
         (let ([lst (racket-listify (concrete-multi-lst (last acc)))])
           (append (drop-right acc 1) (list (concrete-multi (concrete-listify (append lst (list (atom->function conjunct))))))))]
        [(and
          (findf (λ (r) (and (> idx (index-range-start r)) (< idx (index-range-end-before r)))) (generalization-abstracted-ranges n))
          (concrete-multi? conjunct))
         (let ([lst (racket-listify (concrete-multi-lst (last acc)))])
           (append (drop-right acc 1) (list (concrete-multi (concrete-listify (append lst (racket-listify (concrete-multi-lst conjunct))))))))]
        [else
         (append acc (list conjunct))]))
    (match acc
      [(list con/subs evals selection)
       (cond
         [(and (tree-label? n) (rule? (tree-label-rule n)))
          (let ([next-resolvent
                 (resolve
                  (con/sub-con (last con/subs))
                  (some-v selection)
                  (tree-label-rule n)
                  gensym)])
            (and next-resolvent
                 (list
                  (append con/subs (list (resolvent->con/sub next-resolvent)))
                  evals
                  (label-selection n))))]
         [(and (tree-label? n) (full-evaluation? (tree-label-rule n)))
          (let ([next-resolvent
                 (resolvent
                  (remove-at-index
                   (con/sub-con (last con/subs))
                   (some-v selection))
                  empty)]) ;; don't need to track this substitution
            (list
             (append con/subs (list (resolvent->con/sub next-resolvent)))
             (append
              evals
              (list
               (list-ref
                (con/sub-con (last con/subs))
                (some-v selection))))
             (label-selection n)))]
         [(generalization? n)
          (let* ([next-con/sub
                  (con/sub
                   (foldl append-potential-multi-elems empty (con/sub-con (last con/subs)) (range (length (con/sub-con (last con/subs)))))
                   empty)])
            (list
             (append con/subs (list next-con/sub))
             evals
             (label-selection n)))]
         ;; unfold 'one and 'many are still left
         [else (error "Can't deal with this type of node yet." n)])]))
  (let* ([initial-con/sub
          (con/sub (concrete-synth-counterpart (label-conjunction (first b))) empty)]
         [numbered-nodes
          (filter (λ (n) (and (label-with-conjunction? n) (label-index n))) b)]
         [con/subs/full-evals
          (foldl
           extend-con/subs
           (list
            (list initial-con/sub)
            empty
            (label-selection (first b)))
           (cdr numbered-nodes))])
    (synth
     (first con/subs/full-evals)
     (second con/subs/full-evals)
     (last b))))

(module+ test
  (let ([mock-gensym
         (mock
          #:behavior
          (generator (_) (for ([i (in-naturals)]) (yield (format-symbol "Var~a" i)))))]
        [sorted-segments (sort-segments (set->list ps-segments))])
    (check-equal?
     (branch->clause (second sorted-segments) mock-gensym)
     ;; would be nice if I could write γ(q1(sort([Var2|Var3],[Var4|Var5])) :- del(Var4,[Var2|Var3],Var6), q5(perm(Var6,Var5),ord([Var4|Var5])).)
     (rule
      (atom
       'q1
       (list
        (function
         'sort
         (list
          (function (string->symbol "'[|]'") (list (variable 'Var2) (variable 'Var3)))
          (function (string->symbol "'[|]'") (list (variable 'Var4) (variable 'Var5)))))))
      (list
       (atom
        'del
        (list
         (variable 'Var4)
         (function (string->symbol "'[|]'") (list (variable 'Var2) (variable 'Var3)))
         (variable 'Var6)))
       (atom
        'q5
        (list
         (function
          'perm
          (list
           (variable 'Var6)
           (variable 'Var5)))
         (function
          'ord
          (list
           (function
            (string->symbol "'[|]'")
            (list
             (variable 'Var4)
             (variable 'Var5))))))))
      #f))))
(provide branch->clause)

(define (pretty-print-rule r)
  (match r
    [(rule h t _)
     (let ([h-synth (synth-str h)]
           [t-synth (synth-str t)])
       (if (non-empty-string? t-synth)
           (format "~a :- ~a." h-synth t-synth)
           (format "~a." h-synth)))]))
(provide pretty-print-rule)

(module+ test
  (let* ([mock-gensym
          (mock
           #:behavior
           (generator (_) (for ([i (in-naturals)]) (yield (format-symbol "Var~a" i)))))]
         [sorted-segments (sort-segments (set->list ps-segments))]
         [outcomes (map (compose pretty-print-rule (λ (b) (branch->clause b mock-gensym))) sorted-segments)]
         [expected-outcomes
          '("q1(sort([],[]))."
            "q1(sort('[|]'(Var4,Var5),'[|]'(Var6,Var7))) :- del(Var6,'[|]'(Var4,Var5),Var8),q5(perm(Var8,Var7),ord('[|]'(Var6,Var7)))."
            "q5(perm([],[]),ord('[|]'(G12,[])))."
            "q5(perm('[|]'(Var9,Var10),'[|]'(Var15,Var16)),ord('[|]'(Var14,'[|]'(Var15,Var16)))) :- del(Var15,'[|]'(Var9,Var10),Var13),lte(Var14,Var15),q5(perm(Var13,Var16),ord('[|]'(Var15,Var16))).")])
    (for-each
     (λ (o eo) (check-equal? o eo))
     outcomes
     expected-outcomes))
  (let* ([mock-gensym
          (mock
           #:behavior
           (generator (_) (for ([i (in-naturals)]) (yield (format-symbol "Var~a" i)))))]
         [sorted-segments (sort-segments (set->list primes-segments))]
         [outcomes (map (compose pretty-print-rule (λ (b) (branch->clause b mock-gensym))) sorted-segments)]
         [expected-outcomes
          '()])
    (for-each
     (λ (o eo) (check-equal? o eo))
     outcomes
     expected-outcomes)))