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
  (only-in cclp-analysis/interaction analysis-tree proceed)
  cclp-analysis/abstract-analysis
  (only-in cclp-common-data/abstract-knowledge full-evaluation?)
  cclp-common-data/concrete-domain
  cclp-common-data/concrete-knowledge
  cclp-analysis/concrete-resolve
  (only-in cclp-analysis/mi-map synth-str)
  cclp-analysis/concrete-substitution
  (only-in cclp-common/data-utils some-v)
  cclp-common/domain-switching
  cclp-common/gen-graph-structs
  positional-tree-utils
  list-utils)
(require (for-doc scribble/manual))

(define (atom->function a)
  (match a
    [(atom sym args)
     (function sym args)]))
(define (function->atom f)
  (match f
    [(function sym args)
     (atom sym args)]))

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
;(module+ test
;  (require
;    rackunit
;    repeated-application
;    (prefix-in permsort: cclp-programs/permutation-sort))
;  (define ps-tree
;    (analysis-tree
;     (apply↑* proceed permsort:initial-program-analysis)))
;  (check-equal?
;   (A-nodes ps-tree)
;   (set 1 5)))
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
;(module+ test
;  (define ps-segments ((compose sort-segments set->list synthesizable-segments) ps-tree))
;  (check-equal?
;   (list->set (map (λ (b) (map (λ (e) (and e (if (label-with-conjunction? e) (label-index e) e))) b)) ps-segments))
;   (set (list 1 2 3 #f) (list 1 2 4 5) (list 5 6 #f) (list 5 7 8 9 10 (cycle 5)))))
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

(define (branch->clause b [gensym gensym])
  (define (open-ended-multi? m)
    (and
     (concrete-multi? m)
     (not
      (list?
       (racket-listify
        (concrete-multi-lst m))))))
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
  (define (refresh c)
    (match c
      [(? list?) (map refresh c)]
      [(atom sym args) (atom sym (map refresh args))]
      [(function sym args) (function sym (map refresh args))]
      [(variable vn) (variable (gensym))]))
  (define (extend-con/subs n acc) ;; here, acc is a list of con/subs
    (define (in-grouping? i g)
      (ormap
       (match-lambda [(index-range s eb) (<= s i (sub1 eb))])
       (car g)))
    (define (bb-listify rng con)
      (cond
        [(and
          (= 1 (- (index-range-end-before rng) (index-range-start rng)))
          (concrete-multi? (list-ref con (index-range-start rng))))
         (concrete-multi-lst (list-ref con (index-range-start rng)))]
        [else
         (function
          cons-symbol
          (list
           (function
            'building_block
            (list
             (concrete-listify
              (map
               (compose atom->function (λ (i) (list-ref con i)))
               (stream->list
                (in-range
                 (index-range-start rng)
                 (index-range-end-before rng)))))))
           concrete-nil))]))
    (define (package-grouping rngs con)
      (match-let* ([first-oem-rng
                    (findf
                     (λ (rng)
                       (and
                        (= (index-range-start rng)
                           (sub1 (index-range-end-before rng)))
                        (open-ended-multi?
                         (list-ref con (index-range-start rng)))))
                     (drop-right rngs 1))] ; only matters if it's not the last one
                   [first-oem (and first-oem-rng (list-ref con (index-range-start first-oem-rng)))]
                   [tail-var-pre-extension
                    (and
                     first-oem
                     (cdr (last-pair (racket-listify (concrete-multi-lst first-oem)))))]
                   [tail-var-post-extension
                    (and tail-var-pre-extension (variable (gensym (string->symbol (format "Extended~a" (variable-name tail-var-pre-extension))))))]
                   [(list rec-multi rec-appends)
                    (if (not first-oem)
                        '(#f #f)
                        (package-grouping (cdr (member first-oem-rng rngs)) con))]
                   [mapped
                    (cond
                      [(not first-oem)
                       (map
                        (compose
                         racket-listify
                         (λ (rng)
                           (bb-listify rng con)))
                        rngs)]
                      [else
                       (match-let*
                           ([(list partial-rec-multi _)
                             (package-grouping (append (takef rngs (λ (rng) (not (equal? rng first-oem-rng)))) (list first-oem-rng)) con)])
                         (apply-variable-substitution (list (concrete-equality tail-var-pre-extension tail-var-post-extension)) partial-rec-multi))])]
                   [appends
                    (if
                     (not first-oem)
                     empty
                     (append rec-appends (list (atom 'append (list tail-var-pre-extension (concrete-multi-lst rec-multi) tail-var-post-extension)))))])
        (list
         (if (not first-oem)
             (concrete-multi
              (concrete-listify
               (apply
                append/impure
                mapped)))
             mapped)
         appends)))
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
          (let* ([ungeneralized-part
                  (filter-map
                   (match-lambda
                     [(cons c i)
                      (and
                       (not
                        (ormap
                         (curry in-grouping? i)
                         (generalization-groupings n)))
                       c)])
                   (enumerate (con/sub-con (last con/subs))))]
                 [packaged-groupings
                  (map
                   (match-lambda
                     [(cons rngs idx)
                      (append
                       (package-grouping
                        rngs
                        (con/sub-con
                         (last con/subs)))
                       (list idx))])
                   (generalization-groupings n))]
                 [next-con/sub
                  (con/sub
                   (foldl
                    (λ (pg acc)
                      (splice-in acc (first pg) (third pg)))
                    ungeneralized-part
                    packaged-groupings)
                   empty)])
            (list
             (append con/subs (list next-con/sub))
             (append evals (append-map second packaged-groupings))
             (label-selection n)))]
         [(and (tree-label? n) (eq? (tree-label-rule n) 'one))
          (let* ([cm (list-ref (con/sub-con (last con/subs)) (some-v selection))]
                 [first-bb (car (racket-listify (concrete-multi-lst cm)))]
                 [atoms (map function->atom (racket-listify (first (function-args first-bb))))]
                 [tail (cdr (last-pair (racket-listify (concrete-multi-lst cm))))])
            (list
             (append
              con/subs
              (list
               (con/sub
                (append
                 (take (con/sub-con (last con/subs)) (some-v selection))
                 atoms
                 (drop (con/sub-con (last con/subs)) (add1 (some-v selection))))
                (list
                 ;; no need to apply subst if we remove concrete multi entirely
                 (concrete-equality tail concrete-nil)))))
             evals
             (label-selection n)))]
         [(and (tree-label? n) (eq? (tree-label-rule n) 'many))
          (let* ([cm (list-ref (con/sub-con (last con/subs)) (some-v selection))]
                 [first-bb (car (racket-listify (concrete-multi-lst cm)))]
                 [remaining-bbs (cdr (racket-listify (concrete-multi-lst cm)))]
                 [atoms (map function->atom (racket-listify (first (function-args first-bb))))]
                 [tail (cdr (last-pair (racket-listify (concrete-multi-lst cm))))]
                 [nonempty-list-func
                  (let* ([remaining-abstract-multi (list-ref (label-conjunction n) (+ (some-v selection) (length atoms)))]
                         [counterpart (concrete-synth-counterpart remaining-abstract-multi)]
                         [block (refresh (car (racket-listify (concrete-multi-lst counterpart))))]) ;; FIXME: block needs to be renamed apart
                    (function
                     cons-symbol
                     (list
                      block
                      (variable (gensym 'Var)))))]
                 [subst
                  (list
                   (concrete-equality tail nonempty-list-func))])
            (list
             (append
              con/subs
              (list
               (con/sub
                (append
                 (take (con/sub-con (last con/subs)) (some-v selection))
                 atoms
                 (list (concrete-multi (apply-variable-substitution subst (concrete-listify remaining-bbs))))
                 (drop (con/sub-con (last con/subs)) (add1 (some-v selection))))
                subst)))
             evals
             (label-selection n)))]
         [else (error "Unexpected label or rule type." n)])]))
  (let* ([initial-con/sub
          (con/sub (concrete-synth-counterpart (label-conjunction (first b))) empty)]
         [numbered-nodes
          (filter (λ (n) (and (label-with-conjunction? n) (or (null? (label-conjunction n)) (label-index n)))) b)]
         [con/subs/full-evals
          (foldl
           extend-con/subs
           (list
            (list initial-con/sub)
            empty
            (label-selection (first b)))
           (cdr numbered-nodes))])
    (synth
     (first con/subs/full-evals)  ; the conjunctions + substitutions
     (second con/subs/full-evals) ; the full evals
     (last b))))

;(module+ test
;  (let ([mock-gensym
;         (mock
;          #:behavior
;          (generator (_) (for ([i (in-naturals)]) (yield (format-symbol "Var~a" i)))))]
;        [sorted-segments (sort-segments (set->list ps-segments))])
;    (check-equal?
;     (branch->clause (second sorted-segments) mock-gensym)
;     ;; would be nice if I could write γ(q1(sort([Var2|Var3],[Var4|Var5])) :- del(Var4,[Var2|Var3],Var6), q5(perm(Var6,Var5),ord([Var4|Var5])).)
;     (rule
;      (atom
;       'q1
;       (list
;        (function
;         'sort
;         (list
;          (function (string->symbol "'[|]'") (list (variable 'Var2) (variable 'Var3)))
;          (function (string->symbol "'[|]'") (list (variable 'Var4) (variable 'Var5)))))))
;      (list
;       (atom
;        'del
;        (list
;         (variable 'Var4)
;         (function (string->symbol "'[|]'") (list (variable 'Var2) (variable 'Var3)))
;         (variable 'Var6)))
;       (atom
;        'q5
;        (list
;         (function
;          'perm
;          (list
;           (variable 'Var6)
;           (variable 'Var5)))
;         (function
;          'ord
;          (list
;           (function
;            (string->symbol "'[|]'")
;            (list
;             (variable 'Var4)
;             (variable 'Var5))))))))
;      #f))))
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

;(module+ test
;  (let* ([mock-gensym
;          (mock
;           #:behavior
;           (generator (_) (for ([i (in-naturals)]) (yield (format-symbol "Var~a" i)))))]
;         [sorted-segments (sort-segments (set->list ps-segments))]
;         [outcomes (map (compose pretty-print-rule (λ (b) (branch->clause b mock-gensym))) sorted-segments)]
;         [expected-outcomes
;          '("q1(sort([],[]))."
;            "q1(sort('[|]'(Var4,Var5),'[|]'(Var6,Var7))) :- del(Var6,'[|]'(Var4,Var5),Var8),q5(perm(Var8,Var7),ord('[|]'(Var6,Var7)))."
;            "q5(perm([],[]),ord('[|]'(Var9,[])))."
;            "q5(perm('[|]'(Var10,Var11),'[|]'(Var16,Var17)),ord('[|]'(Var15,'[|]'(Var16,Var17)))) :- del(Var16,'[|]'(Var10,Var11),Var14),lte(Var15,Var16),q5(perm(Var14,Var17),ord('[|]'(Var16,Var17))).")])
;    (for-each
;     (λ (o eo) (check-equal? o eo))
;     outcomes
;     expected-outcomes)))