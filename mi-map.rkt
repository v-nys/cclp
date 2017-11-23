#lang at-exp racket
; MIT License
;
; Copyright (c) 2016-2017 Vincent Nys
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

(require scribble/srcdoc
         (for-doc scribble/manual)
         racket/set
         (only-in racket/syntax format-symbol)
         (only-in racket-list-utils/utils map-accumulatel)
         racket-tree-utils/src/tree
         sugar/coerce
         "abstract-analysis.rkt"
         (prefix-in ak: "abstract-knowledge.rkt")
         (only-in "abstract-substitution.rkt" apply-substitution)
         (only-in "abstraction-inspection-utils.rkt"
                  extract-abstract-compounds
                  extract-variables/duplicates
                  extract-all-variables/duplicates
                  extract-all-variables/duplicates/exclude-constraints
                  get-multi-id
                  maximum-var-index)
         "abstract-multi-domain.rkt"
         (only-in "abstract-renaming.rkt" rename-apart rename-apart/substitution)
         "cclp-interpreter.rkt"
         "concrete-domain.rkt"
         (prefix-in ck: "concrete-knowledge.rkt")
         "concrete-inspection-utils.rkt"
         (only-in "control-flow.rkt" aif it)
         (only-in "data-utils.rkt" some-v)
         "domain-switching.rkt"
         (only-in "gen-graph-structs.rkt" index-range index-range-start index-range-end-before)
         (only-in "io-utils.rkt" between?))

(define cons-symbol (string->symbol "'[|]'"))
(define concrete-nil (function (string->symbol "[]") (list)))

(define (mi-map-visit-from idx n)
  (match n
    [(node (tree-label (list) _ _ (ck:rule _ _ rule-idx) #f) _)
     (displayln (format "transition_from_to_via(~a,empty,rule~a)." idx rule-idx))]
    [(node (tree-label (list) _ _ (ak:full-evaluation _ _ rule-idx) #f) _)
     (displayln (format "transition_from_to_via(~a,empty,fullai~a)." idx rule-idx))]
    [(node (tree-label _ _ _ (ck:rule _ _ rule-idx) idx2) _)
     (displayln (format "transition_from_to_via(~a,~a,rule~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ (ak:full-evaluation _ _ rule-idx) idx2) _)
     (displayln (format "transition_from_to_via(~a,~a,fullai~a)." idx idx2 rule-idx))]
    [(node (tree-label _ _ _ 'one idx2) _)
     (displayln (format "transition_from_to_via(~a,~a,one)." idx idx2))]
    [(node (tree-label _ _ _ 'many idx2) _)
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
  (begin
    (visit mi-map-visitor tree)
    tree))
(provide
 (proc-doc/names
  display-mi-map
  (->
   node?
   node?)
  (tree)
  @{Summarizes the transitions between nodes in an abstract tree @racket[tree] in terms of the node numbers involved and the transition type (generalization, cycle, standard transition i.e. unfolding).}))

(define (compound-constructor l)
  (match l
    [(abstract-atom _ _) abstract-atom]
    [(abstract-function _ _) abstract-function]
    [(abstract-atom* _ _) abstract-atom*]
    [(abstract-function* _ _) abstract-function*]))

(define (rename-occurrence replacee replacer locus)
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
    [(multi patt asc? ic consec fc rta)
     (match-let
         ([(cons after-1 success-1?)
           (rename patt)]
          [zip (λ (l1 l2) (map (λ (e1 e2) (cons e1 e2)) l1 l2))])
       (cond [success-1?
              (cons (multi after-1 asc? ic consec fc rta) success-1?)]
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
     "foo(g1,a1),bar(g2,a2),baz(g3,a3),quux(g3,a3),poit(g4,a4)"))
   (cons
    (interpret-abstract-conjunction
     "foo(g1,a1),bar(g2,a2),baz(g3,a101),quux(g3,a3),poit(g4,a4)")
    #t)))

(define (rename-occurrences locus aliases)
  (match aliases
    [(list) locus]
    [(list-rest (cons replacee replacer) tail)
     (rename-occurrences
      (car (rename-occurrence replacee replacer locus))
      tail)]))
(module+ test
  (check-equal?
   (rename-occurrences
    (interpret-abstract-conjunction
     "foo(g1,a1),bar(g2,a2),baz(g3,a3),quux(g3,a3),narf(g3,a3),poit(g4,a4)")
    (list
     (cons (a 1) (a 5))
     (cons (a 3) (a 6))))
   (interpret-abstract-conjunction
    "foo(g1,a5),bar(g2,a2),baz(g3,a6),quux(g3,a3),narf(g3,a3),poit(g4,a4)")))
(provide
 (proc-doc/names
  rename-occurrences
  (->
   (listof abstract-conjunct?)
   (listof
    (or/c
     (cons/c a? a?)
     (cons/c g? g?)
     (cons/c a*? a*?)
     (cons/c g*? g*?)))
   (listof abstract-conjunct?))
  (ac al)
  @{Renames occurrences of the variables occurring in @racket[ac] as first elements in pairs in @racket[al] to variables occurring as the corresponding second elements.
 The list @racket[al] is an association list with duplicate keys.
 If @racket[al] contains entries with the same key, e.g. @racket[cons((a 1) (a 2))] and @racket[cons((a 1) (a 3))], the first occurrence of the key will be replaced with the first associated value, etc.}))

;; private function, should be clear enough
(define (extract-avar-constructor e)
  (match e
    [(a _) a]
    [(g _) g]
    [(a* i j _) (curry a* i j)]
    [(g* i j _) (curry g* i j)]))

;; private function, should be clear enough
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
(provide
 (proc-doc/names
  find-max-vars
  (->
   (or/c abstract-variable? abstract-variable*?)
   (hash/c
    symbol?
    (or/c abstract-variable? abstract-variable*?))
   (hash/c
    symbol?
    (or/c abstract-variable? abstract-variable*?)))
  (occ acc)
  @{Can be folded over a list of variables to obtain the maximum variable index for each "type" of variable, e.g. those with constructor @racket[a], @racket[g] or @racket[a*] and @racket[g*], where the multi ID and the symbolic index @racket['i] are curried over the constructor in the latter two cases.}))

(define occurrence-acc/c
  (list/c
   (listof
    (or/c
     (cons/c a? a?)
     (cons/c g? g?)
     (cons/c a*? a*?)
     (cons/c g*? g*?)))
   set?
   (listof
    (cons/c
     symbol?
     (or/c abstract-variable? abstract-variable*?)))))
(provide occurrence-acc/c)
  
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
(provide
 (proc-doc/names
  maybe-map-occurrence
  (->
   (or/c abstract-variable? abstract-variable*?)
   occurrence-acc/c
   occurrence-acc/c)
  (e acc)
  @{Foldable function which takes an abstract variable @racket[e] and updates an accumulator to reflect that the variable has either been encountered for the first time or has been given a new explicit alias.}))

(define (untangle init-ac)
  (define var-occurrences
    (extract-all-variables/duplicates/exclude-constraints init-ac))
  (define max-var-indices
    (foldl find-max-vars (hash) var-occurrences))
  ;; gives fresh variables for n-1 occurrences of each abstract variable in var-occurrences
  (define occurrence-renamings
    (reverse
     (first
      (foldl maybe-map-occurrence `(() ,(set) ,max-var-indices) var-occurrences))))
  (list
   (rename-occurrences init-ac occurrence-renamings)
   occurrence-renamings))
(module+ test
  (require rackunit)
  (check-equal?
   (untangle
    (interpret-abstract-conjunction "integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),filter(g4,a3,a4),sift(a4,a5),length(a5,g5)"))
   (list
    (interpret-abstract-conjunction "integers(g1,a6),filter(g2,a1,a7),filter(g3,a2,a8),filter(g4,a3,a9),sift(a4,a10),length(a5,g5)")
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
      (list (cons (a* 1 1 1) (a 2)))
      (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
      (list (cons (a* 1 'L 2) (a 3))))
     (abstract-atom 'filter (list (g 3) (a 3) (a 4)))
     (abstract-atom 'sift (list (a 4) (a 5)))
     (abstract-atom 'length (list (a 5) (g 4)))))
   (list
    (list
     (abstract-atom 'integers (list (g 1) (a 6)))
     (abstract-atom 'filter (list (g 2) (a 1) (a 2)))
     (multi
      (list
       (abstract-atom*
        'filter
        (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (list (cons (a* 1 1 1) (a 2)))
      (list (cons (a* 1 'i+1 1) (a* 1 'i 2))) ; note: use of a variable in consecutive is not considered aliasing!
      (list (cons (a* 1 'L 2) (a 3))))
     (abstract-atom 'filter (list (g 3) (a 3) (a 7)))
     (abstract-atom 'sift (list (a 4) (a 8)))
     (abstract-atom 'length (list (a 5) (g 4))))
    (list
     (cons (a 1) (a 6))
     (cons (a 4) (a 7))
     (cons (a 5) (a 8)))))
  (check-equal?
   (untangle
    (append
     (interpret-abstract-conjunction "collect(g1,a1),collect(g2,a2),append(a1,a2,a3),collect(g3,a4),append(a3,a4,a5)")
     (cons
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
       #f
       (list (cons (a* 1 1 2) (a 5)))
       (list (cons (a* 1 'i+1 1) (a* 1 'i 3)))
       (list (cons (a* 1 'L 3) (a 6))))
      (interpret-abstract-conjunction "collect(g4,a7),eq(a6,a7)"))))
   (list
    (append
     (interpret-abstract-conjunction "collect(g1,a8),collect(g2,a9),append(a1,a2,a10),collect(g3,a11),append(a3,a4,a5)")
     (cons
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 4)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
       #f
       (list (cons (a* 1 1 2) (a 5)))
       (list (cons (a* 1 'i+1 1) (a* 1 'i 3)))
       (list (cons (a* 1 'L 3) (a 6))))
      (interpret-abstract-conjunction "collect(g4,a12),eq(a6,a7)")))
    (list
     (cons (a 1) (a 8))
     (cons (a 2) (a 9))
     (cons (a 3) (a 10))
     (cons (a 4) (a 11))
     (cons (a* 1 'i 1) (a* 1 'i 4))
     (cons (a 7) (a 12))))))
(provide
 (proc-doc/names
  untangle
  (->
   (listof abstract-conjunct?)
   list?)
  (ac)
  @{Undoes the aliasing in abstract conjunction @racket[ac] and returns the modified conjunction plus association list with duplicate keys with the required information to restore aliasing.
 This is useful for generation of generalization/2 clauses for the meta-interpreter, as aliasing must be matched in these clauses and must not be enforced by unification.}))

(define (deconstruction-atoms replacements)
  (append-map
   (match-lambda
     [(or
       (cons (abstract-function sym args) repl-avar)
       (cons (abstract-function* sym args) repl-avar))
      (list
       (format "nonvar(~a)" (synth-str (concrete-synth-counterpart repl-avar)))
       (format
        "~a =.. [~a~a]"
        (synth-str (concrete-synth-counterpart repl-avar))
        sym
        (if
         (not
          (null? args))
         (string-join
          (map (compose synth-str concrete-synth-counterpart) args)
          ","
          #:before-first ",")
         "")))])
   replacements))

(define (groundness-atoms con repl)
  (remove-duplicates
   (filter
    (λ (v)
      (eqv?
       (string-ref
        (symbol->string
         (variable-name v))
        0)
       #\G))
    (append
     (extract-all-concrete-variables/duplicates con)
     (extract-all-concrete-variables/duplicates
      (map
       (match-lambda
         [(cons afunc _)
          (concrete-synth-counterpart afunc)])
       repl))))))

(define (append-atoms appends)
  (map
   (match-lambda
     [(list (variable name-tail1) contents-as-list (variable name-tail2))
      (format
       "append(~a,~a,~a)"
       name-tail1
       (synth-str
        (concrete-listify
         contents-as-list))
       name-tail2)])
   appends))

(define (aliasing-constraints aliasing)
  (map
   (match-lambda
     [(cons av1 av2)
      (format
       "~a == ~a"
       (synth-str (concrete-synth-counterpart av1))
       (synth-str (concrete-synth-counterpart av2)))])
   aliasing))

(define (pattern-check a-multi c-multi)
  (define (concrete-patternize e)
    (match e
      [(? list?)
       (string-join
        (map concrete-patternize e)
        ",")]
      [(or
        (abstract-atom* sym args)
        (abstract-function* sym args))
       (format
        "~a(~a)"
        sym
        (string-join
         (map
          concrete-patternize
          args)
         ","))]
      [(a* _ _ _) "_-a"]
      [(g* _ _ _) "_-g"]))
  (match* (a-multi c-multi)
    [((multi patt _ _ _ _ _) (concrete-multi c-lst))
     (format
      "check_pattern(~a,[building_block([~a])])"
      (synth-str c-lst)
      (concrete-patternize patt))]))

(define (localize e)
  (match e
    [(? list?) (map localize e)]
    [(abstract-atom* sym args)
     (abstract-atom sym (map localize args))]
    [(abstract-function* sym args)
     (abstract-function sym (map localize args))]
    [(a* _ _ i) (a i)]
    [(g* _ _ i) (g i)]))

(define (consecutive-check a-multi c-multi acon additional-vars)
  (define (correct-aliasing local-pattern consec fresh)
    (define (replacement-proc e fresh)
      (match e
        [(abstract-atom sym args)
         (match-let
             ([(cons replaced new-fresh)
               (map-accumulatel replacement-proc fresh args)])
           (cons (abstract-atom sym replaced) new-fresh))]
        [(abstract-function sym args)
         (match-let
             ([(cons replaced new-fresh)
               (map-accumulatel replacement-proc fresh args)])
           (cons (abstract-function sym replaced) new-fresh))]
        [(a i)
         ; would be more readable if consec was turned into a hash
         #:when (member e (map (compose localize car) consec))
         (cons (localize (cdr (findf (match-lambda [(cons k v) (equal? (localize k) e)]) consec))) fresh)]
        [(g i)
         ; would be more readable if consec was turned into a hash
         #:when (member e (map (compose localize car) consec))
         (cons (localize (cdr (findf (match-lambda [(cons k v) (equal? (localize k) e)]) consec))) fresh)]
        [(a i)
         (cons (a fresh) (add1 fresh))]
        [(g i)
         (cons (g fresh) (add1 fresh))]))
    (car
     (map-accumulatel
      replacement-proc
      fresh
      local-pattern)))
  (let* ([localized-pattern (localize (multi-conjunction a-multi))]
         [fresh (add1 (some-v (maximum-var-index localized-pattern (λ (_) #t))))]
         [correctly-aliased
          (correct-aliasing
           localized-pattern
           (multi-consecutive a-multi)
           fresh)]
         [joined-and-renamed
          (rename-apart
           (append localized-pattern correctly-aliased)
           (cons (abstract-atom 'dummy additional-vars) acon))]
         [masked (mask-singletons (concrete-synth-counterpart joined-and-renamed))])
    (format
     "check_consecutive(~a,building_block([~a]),building_block([~a]))"
     (synth-str (concrete-multi-lst c-multi))
     (synth-str (take masked (length localized-pattern)))
     (synth-str (drop masked (length localized-pattern))))))

(define (new-init-check a-multi c-multi acon con)
  (define (first-elem-lst lst-func)
    (match lst-func
      [(function cons-symbol args)
       (function cons-symbol (list (first args) concrete-nil))]))
  (define (erase-or-substitute constraints e)
    (match e
      [(? list?)
       (map (curry erase-or-substitute constraints) e)]
      [(abstract-atom sym args)
       (atom sym (map (curry erase-or-substitute constraints) args))]
      [(abstract-function sym args)
       (function sym (map (curry erase-or-substitute constraints) args))]
      [(? abstract-variable?)
       (aif
        (hash-ref constraints e #f)
        (concrete-synth-counterpart it)
        (variable '_))]))
  (match-let*
      ([(cons localized-pattern subst)
        (rename-apart/substitution
         (localize
          (multi-conjunction a-multi))
         acon)]
       [localized-constraints
        (map
         (match-lambda [(cons k v) (cons (apply-substitution subst (localize k)) v)])
         (multi-init a-multi))]
       [template-bb
        (erase-or-substitute
         (make-hash localized-constraints)
         localized-pattern)])
    (format
     "unchanged_under_substitution([~a],~a,[building_block([~a])])"
     (synth-str con)
     (synth-str (first-elem-lst (concrete-multi-lst c-multi)))
     (synth-str template-bb))))

;; TODO merge with previous function
(define (new-last-check a-multi c-multi acon con last-cntr)
  (define (erase-or-substitute constraints e)
    (match e
      [(? list?)
       (map (curry erase-or-substitute constraints) e)]
      [(abstract-atom sym args)
       (atom sym (map (curry erase-or-substitute constraints) args))]
      [(abstract-function sym args)
       (function sym (map (curry erase-or-substitute constraints) args))]
      [(? abstract-variable?)
       (aif
        (hash-ref constraints e #f)
        (concrete-synth-counterpart it)
        (variable '_))]))
  (match-let*
      ([(cons localized-pattern subst)
        (rename-apart/substitution
         (localize
          (multi-conjunction a-multi))
         acon)]
       [localized-constraints
        (map
         (match-lambda [(cons k v) (cons (apply-substitution subst (localize k)) v)])
         (multi-final a-multi))]
       [template-bb
        (erase-or-substitute
         (make-hash localized-constraints)
         localized-pattern)])
    (let ([last-bb (format "Last~a" last-cntr)])
      (format
       "last(~a,~a),unchanged_under_substitution([~a],[~a],[building_block([~a])])"
       (synth-str (concrete-multi-lst c-multi))
       last-bb
       (synth-str con)
       last-bb
       (synth-str template-bb)))))

(define (multi-checks acon con additional-vars)
  (car
   (foldl
    (λ (a c acc)
      (match acc
        [(cons strs last-cntr)
         (cond
           [(and (multi? a) (concrete-multi? c))
            (cons
             (append
              (list
               (pattern-check a c)
               (new-init-check a c acon con)
               (consecutive-check a c acon additional-vars)
               (new-last-check a c acon con last-cntr))
              strs)
             (add1 last-cntr))]
           [(xor (multi? a) (concrete-multi? c))
            (error "abstract and concrete conjunction are out of sync")]
           [else acc])]))
    (cons empty 1)
    acon
    con)))

(define (generate-generalization-clause parent blocks)
  (match-let*
      ([(list untangled aliasing)
        (untangle parent)]
       [(cons deconstructed compound-replacements)
        (deconstruct untangled)]
       [arg1 (concrete-synth-counterpart deconstructed)]
       [(list arg2 appends _ti)
        (generalization/2-head-arg2 arg1 blocks)]
       [body-atoms
        (append
         (deconstruction-atoms compound-replacements)
         (append-atoms appends) ; introduces new vars TailN but these will never clash with other names
         (multi-checks
          parent
          arg1
          (append
           (map cdr aliasing)
           (extract-variables/duplicates deconstructed)))
         (aliasing-constraints aliasing) ; should not produce this if abstract variable only occurs in Last
         (map
          (compose
           (λ (e)
             (format "ground(~a)" e))
           synth-str)
          (groundness-atoms arg1 compound-replacements))
         (list "!"))])
    (format
     "generalization([~a],[~a]) :- \n  ~a."
     (synth-str arg1)
     (synth-str arg2)
     (string-join body-atoms ",\n  "))))
(module+ test
  (let ([node-33-parent-conjunction
         (append
          (interpret-abstract-conjunction
           (string-append
            "integers(g1,a1),"
            "filter(g2,a1,a2),"
            "filter(g3,a2,a3),"
            "filter(g4,a3,a4),"
            "sift(a4,a5)"))
          (list
           (abstract-atom
            'alt_length
            (list
             (abstract-function
              cons-symbol ; interpret-... just parses straight brackets as 'cons instead of cons-symbol
              (list (g 5) (a 5)))
             (g 6)))))]
        [node-33-building-blocks
         (list
          (cons (list (index-range 1 2) (index-range 2 3)) 1))])
    (check-equal?
     (generate-generalization-clause
      node-33-parent-conjunction
      node-33-building-blocks)
     (string-append
      "generalization(["
      "integers(G1,A6),"
      "filter(G2,A1,A7),"
      "filter(G3,A2,A8),"
      "filter(G4,A3,A9),"
      "sift(A4,A10),"
      "alt_length(A11,G6)"
      "],["
      "integers(G1,A6),"
      "multi('[|]'("
      "building_block('[|]'(filter(G2,A1,A7),[])),"
      "'[|]'(building_block('[|]'(filter(G3,A2,A8),[])),[]))),"
      "filter(G4,A3,A9),"
      "sift(A4,A10),"
      "alt_length(A11,G6)"
      "]) :- \n"
      "  nonvar(A11),\n"
      "  A11 =.. ['[|]',G5,A5],\n"
      "  A1 == A6,\n"
      "  A2 == A7,\n"
      "  A3 == A8,\n"
      "  A4 == A9,\n"
      "  A5 == A10,\n"
      "  ground(G1),\n"
      "  ground(G2),\n"
      "  ground(G3),\n"
      "  ground(G4),\n"
      "  ground(G6),\n"
      "  ground(G5),\n"
      "  !.")))
  (let ([node-46-parent-conjunction
         (append
          (interpret-abstract-conjunction
           (string-append
            "integers(g1,a1),"
            "filter(g2,a1,a2)"))
          (list
           (multi
            (list
             (abstract-atom*
              'filter
              (list
               (g* 1 'i 1)
               (a* 1 'i 1)
               (a* 1 'i 2))))
            #t
            (list
              (cons (a* 1 1 1)
                    (a 2)))
            (list
              (cons (a* 1 'i+1 1)
                    (a* 1 'i 2)))
            (list
              (cons (a* 1 'L 2)
                    (a 3)))))
          (interpret-abstract-conjunction
           (string-append
            "filter(g3,a3,a4),"
            "sift(a4,a5),"
            "alt_length(a5,g4)")))]
        [node-46-building-blocks
         (list
          (cons
           (list
            (index-range 1 2)
            (index-range 2 3))
           1))])
    (check-equal?
     (generate-generalization-clause
      node-46-parent-conjunction
      node-46-building-blocks)
     (string-append
      "generalization(["
      "integers(G1,A6),"
      "filter(G2,A1,A2),"
      "multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1)),"
      "filter(G3,A3,A7),"
      "sift(A4,A8),"
      "alt_length(A5,G4)],"
      "[integers(G1,A6),"
      "multi('[|]'(building_block('[|]'(filter(G2,A1,A2),[])),'[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1))),"
      "filter(G3,A3,A7),"
      "sift(A4,A8),"
      "alt_length(A5,G4)]) :- \n"
      "  check_pattern('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),[building_block([filter(_-g,_-a,_-a)])]),\n"
      "  unchanged_under_substitution(["
      "integers(G1,A6),filter(G2,A1,A2),multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1)),filter(G3,A3,A7),sift(A4,A8),alt_length(A5,G4)],"
      "'[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),[]),"
      "[building_block([filter(_,A2,_)])]),\n"
      "  check_consecutive('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),building_block([filter(G5,A9,A10)]),building_block([filter(G7,A10,A12)])),\n"
      "  last('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),Last1),"
      "unchanged_under_substitution(["
      "integers(G1,A6),filter(G2,A1,A2),multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1)),filter(G3,A3,A7),sift(A4,A8),alt_length(A5,G4)],"
      "[Last1],"
      "[building_block([filter(_,_,A3)])]),\n"
      "  A1 == A6,\n"
      "  A4 == A7,\n"
      "  A5 == A8,\n"
      "  ground(G1),\n"
      "  ground(G2),\n"
      "  ground(G1i1),\n"
      "  ground(G3),\n"
      "  ground(G4),\n"
      "  !.")))
  (let ([node-61-parent-conjunction
         (append
          (list
           (abstract-atom 'integers (list (g 1) (a 1)))
           (multi
            (list
             (abstract-atom*
              'filter
              (list
               (g* 1 'i 1)
               (a* 1 'i 1)
               (a* 1 'i 2))))
            #t
            (list
              (cons (a* 1 1 1)
                    (a 1)))
            (list
              (cons (a* 1 'i+1 1)
                    (a* 1 'i 2)))
            (list
              (cons (a* 1 'L 2)
                    (a 2)))))
          (interpret-abstract-conjunction
           (string-append
            "filter(g2,a2,a3),"
            "filter(g3,a3,a4),"
            "sift(a4,a5)"))
          (list
           (abstract-atom
            'alt_length
            (list
             (abstract-function cons-symbol (list (g 4) (a 5)))
             (g 6)))))]
        [node-61-building-blocks
         (list
          (cons
           (list
            (index-range 1 2)
            (index-range 2 3))
           1))])
    (check-equal?
     (generate-generalization-clause
      node-61-parent-conjunction
      node-61-building-blocks)
     (string-append
      "generalization(["
      "integers(G1,A1),"
      "multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1)),"
      "filter(G2,A2,A6),"
      "filter(G3,A3,A7),"
      "sift(A4,A8),"
      "alt_length(A9,G6)],"
      "[integers(G1,A1),"
      "multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail2)),"
      "filter(G3,A3,A7),"
      "sift(A4,A8),"
      "alt_length(A9,G6)]) :- \n"
      "  nonvar(A9),\n"
      "  A9 =.. ['[|]',G4,A5],\n"
      "  append(Tail1,'[|]'(building_block('[|]'(filter(G2,A2,A6),[])),[]),Tail2),\n"
      "  check_pattern('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),[building_block([filter(_-g,_-a,_-a)])]),\n"
      "  unchanged_under_substitution(["
      "integers(G1,A1),multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1)),filter(G2,A2,A6),filter(G3,A3,A7),sift(A4,A8),alt_length(A9,G6)],"
      "'[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),[]),"
      "[building_block([filter(_,A1,_)])]),\n"
      "  check_consecutive('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),building_block([filter(G7,A10,A11)]),building_block([filter(G9,A11,A13)])),\n"
      "  last('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),Last1),"
      "unchanged_under_substitution(["
      "integers(G1,A1),multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1)),filter(G2,A2,A6),filter(G3,A3,A7),sift(A4,A8),alt_length(A9,G6)],"
      "[Last1],"
      "[building_block([filter(_,_,A2)])]),\n"
      "  A3 == A6,\n"
      "  A4 == A7,\n"
      "  A5 == A8,\n"
      "  ground(G1),\n"
      "  ground(G1i1),\n"
      "  ground(G2),\n"
      "  ground(G3),\n"
      "  ground(G6),\n"
      "  ground(G4),\n"
      "  !.")))
  (let ([graphcol-36-conjunction
         (interpret-abstract-conjunction
          (string-append
           "coloring(cons(a71,a72)),"
           "allsafe(g193,g194,cons(g476,g477),cons(a71,a72)),"
           "allsafe(g197,g198,cons(g476,g477),cons(a71,a72)),"
           "allsafe(g201,g202,cons(g476,g477),cons(a71,a72)),"
           "allsafe(g203,g204,cons(g476,g477),cons(a71,a72)),"
           "safe(cons(g476,g477),cons(a71,a72))"))]
        [graphcol-36-building-blocks
         (list
          (cons
           (list
            (index-range 2 3)
            (index-range 3 4)
            (index-range 4 5))
           2))])
    (check-equal?
     (generate-generalization-clause
      graphcol-36-conjunction
      graphcol-36-building-blocks)
     "generalization([coloring(A486),allsafe(G193,G194,A487,A488),allsafe(G197,G198,A489,A490),allsafe(G201,G202,A491,A492),allsafe(G203,G204,A493,A494),safe(A495,A496)],[coloring(A486),allsafe(G193,G194,A487,A488),multi('[|]'(building_block('[|]'(allsafe(G197,G198,A489,A490),[])),'[|]'(building_block('[|]'(allsafe(G201,G202,A491,A492),[])),'[|]'(building_block('[|]'(allsafe(G203,G204,A493,A494),[])),[])))),safe(A495,A496)]) :- \n  nonvar(A486),\n  A486 =.. [cons,A73,A74],\n  nonvar(A487),\n  A487 =.. [cons,G478,G479],\n  nonvar(A488),\n  A488 =.. [cons,A75,A76],\n  nonvar(A489),\n  A489 =.. [cons,G480,G481],\n  nonvar(A490),\n  A490 =.. [cons,A77,A78],\n  nonvar(A491),\n  A491 =.. [cons,G482,G483],\n  nonvar(A492),\n  A492 =.. [cons,A79,A80],\n  nonvar(A493),\n  A493 =.. [cons,G484,G485],\n  nonvar(A494),\n  A494 =.. [cons,A81,A82],\n  nonvar(A495),\n  A495 =.. [cons,G476,G477],\n  nonvar(A496),\n  A496 =.. [cons,A71,A72],\n  A71 == A73,\n  A72 == A74,\n  G476 == G478,\n  G477 == G479,\n  A71 == A75,\n  A72 == A76,\n  G476 == G480,\n  G477 == G481,\n  A71 == A77,\n  A72 == A78,\n  G476 == G482,\n  G477 == G483,\n  A71 == A79,\n  A72 == A80,\n  G476 == G484,\n  G477 == G485,\n  A71 == A81,\n  A72 == A82,\n  ground(G193),\n  ground(G194),\n  ground(G197),\n  ground(G198),\n  ground(G201),\n  ground(G202),\n  ground(G203),\n  ground(G204),\n  ground(G478),\n  ground(G479),\n  ground(G480),\n  ground(G481),\n  ground(G482),\n  ground(G483),\n  ground(G484),\n  ground(G485),\n  ground(G476),\n  ground(G477),\n  !."))
  (let ([graphcol-57-conjunction
         (append
          (interpret-abstract-conjunction
           "coloring([a103|a104]),allsafe(g479,g480,[g1880|g1881],[a103|a104])")
          (list
           (multi
            (list
             (abstract-atom*
              'allsafe
              (list
               (g* 1 'i 1)
               (g* 1 'i 2)
               (g* 1 'i 3)
               (a* 1 'i 1))))
            #t
            (list
              (cons (g* 1 1 3)
                    (abstract-function 'cons (list (g 1880) (g 1881))))
              (cons (a* 1 1 1)
                    (abstract-function 'cons (list (a 103) (a 104)))))
            (list
              (cons (g* 1 'i+1 3)
                    (g* 1 'i 3))
              (cons (a* 1 'i+1 1)
                    (a* 1 'i 1)))
            (list
              (cons (g* 1 'L 3)
                    (abstract-function 'cons (list (g 1880) (g 1881))))
              (cons (a* 1 'L 1)
                    (abstract-function 'cons (list (a 103) (a 104)))))))
          (interpret-abstract-conjunction
           "allsafe(g887,g888,[g1880|g1881],[a103|a104]),allsafe(g889,g890,[g1880|g1881],[a103|a104]),safe([g1880|g1881],[a103|a104])"))]
        [graphcol-57-building-blocks
         (list
          (cons
           (list
            (index-range 2 3)
            (index-range 3 4))
           2))])
    (check-equal?
     (generate-generalization-clause
      graphcol-57-conjunction
      graphcol-57-building-blocks)
     "generalization([coloring(A1888),allsafe(G479,G480,A1889,A1890),multi('[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),Tail1)),allsafe(G887,G888,A1891,A1892),allsafe(G889,G890,A1893,A1894),safe(A1895,A1896)],[coloring(A1888),allsafe(G479,G480,A1889,A1890),multi('[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),Tail2)),allsafe(G889,G890,A1893,A1894),safe(A1895,A1896)]) :- \n  nonvar(A1888),\n  A1888 =.. [cons,A105,A106],\n  nonvar(A1889),\n  A1889 =.. [cons,G1882,G1883],\n  nonvar(A1890),\n  A1890 =.. [cons,A107,A108],\n  nonvar(A1891),\n  A1891 =.. [cons,G1884,G1885],\n  nonvar(A1892),\n  A1892 =.. [cons,A109,A110],\n  nonvar(A1893),\n  A1893 =.. [cons,G1886,G1887],\n  nonvar(A1894),\n  A1894 =.. [cons,A111,A112],\n  nonvar(A1895),\n  A1895 =.. [cons,G1880,G1881],\n  nonvar(A1896),\n  A1896 =.. [cons,A103,A104],\n  append(Tail1,'[|]'(building_block('[|]'(allsafe(G887,G888,A1891,A1892),[])),[]),Tail2),\n  check_pattern('[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),Tail1),[building_block([allsafe(_-g,_-g,_-g,_-a)])]),\n  unchanged_under_substitution([coloring(A1888),allsafe(G479,G480,A1889,A1890),multi('[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),Tail1)),allsafe(G887,G888,A1891,A1892),allsafe(G889,G890,A1893,A1894),safe(A1895,A1896)],'[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),[]),[building_block([allsafe(_,_,cons(G1880,G1881),cons(A103,A104))])]),\n  check_consecutive('[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),Tail1),building_block([allsafe(G1888,G1889,G1890,A1897)]),building_block([allsafe(G1891,G1892,G1890,A1897)])),\n  last('[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),Tail1),Last1),unchanged_under_substitution([coloring(A1888),allsafe(G479,G480,A1889,A1890),multi('[|]'(building_block('[|]'(allsafe(G1i1,G1i2,G1i3,A1i1),[])),Tail1)),allsafe(G887,G888,A1891,A1892),allsafe(G889,G890,A1893,A1894),safe(A1895,A1896)],[Last1],[building_block([allsafe(_,_,cons(G1880,G1881),cons(A103,A104))])]),\n  A103 == A105,\n  A104 == A106,\n  G1880 == G1882,\n  G1881 == G1883,\n  A103 == A107,\n  A104 == A108,\n  G1880 == G1884,\n  G1881 == G1885,\n  A103 == A109,\n  A104 == A110,\n  G1880 == G1886,\n  G1881 == G1887,\n  A103 == A111,\n  A104 == A112,\n  ground(G479),\n  ground(G480),\n  ground(G1i1),\n  ground(G1i2),\n  ground(G1i3),\n  ground(G887),\n  ground(G888),\n  ground(G889),\n  ground(G890),\n  ground(G1882),\n  ground(G1883),\n  ground(G1884),\n  ground(G1885),\n  ground(G1886),\n  ground(G1887),\n  ground(G1880),\n  ground(G1881),\n  !.")))

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
    [(node (generalization _ _ _ _ building-blocks) _)
     (display
      (generate-generalization-clause
       conjunction1
       building-blocks))]
    [_ (void)]))

(define (generalization-clause-visitor n)
  (match n
    [(node (and (? label-with-conjunction?) label) children)
     (for ([ch children])
       (generalization-clause-visit-from (label-conjunction label) ch))]
    [(node (cycle idx) (list)) (void)]
    [_ (displayln (format "don't know how to visit ~a yet" n))]))

(define (display-generalization-clauses tree)
  (begin
    (visit generalization-clause-visitor tree)
    tree))
(provide
 (proc-doc/names
  display-generalization-clauses
  (->
   node?
   node?)
  (tree)
  @{Prints out the @code{generalization/2} clauses required by the Prolog meta-interpreter.}))

(define (compute-subst c fresh)
  (cons
   (cons
    c
    (cond
      [(abstract-function? c)
       (a fresh)]
      [(abstract-function*? c)
       ;; README:
       ;; strictly speaking, 1 is incorrect
       ;; but we can't get the multi ID if there are no variables in the terms (which does happen)
       ;; however, we only use these substitutions for synthesis
       ;; there, always using 1 does not impact the resulting program as long as the index is fresh
       (a* 1 'i fresh)]))
   (add1 fresh)))
(module+ test
  (check-equal?
   (compute-subst
    (interpret-abstract-term "foo(bar(baz))")
    7)
   (cons
    (cons (interpret-abstract-term "foo(bar(baz))") (a 7))
    8))
  (check-equal?
   (compute-subst
    (abstract-function* 'nil empty)
    10)
   (cons
    (cons (abstract-function* 'nil empty) (a* 1 'i 10))
    11)))




;; note: if every instance of the compound is replaced, aliasing can be encoded in the head of a rule
;; therefore, we should only apply it to the *first* instance
(define (apply-compound-subst s acc)
  (define (aux s obj/success?)
    (define rec (curry aux s))
    (match obj/success?
      [(cons _ #t) obj/success?]
      [(cons (list-rest h t) #f)
       (match-let
           ([(cons h-after success-1?)
             (rec (cons h #f))]
            [(cons t-after success-2?)
             (rec (cons t #f))])
         (cond
           [success-1?
            (cons (cons h-after t) success-1?)]
           [success-2?
            (cons (cons h t-after) success-2?)]
           [else obj/success?]))]
      [(cons (multi patt asc? ic cc fc rta) #f)
       (match s
         [(cons (? abstract-function*?) _)
          (match-let
              ([(cons pattern-after success?)
                (rec (cons patt #f))])
            (cons
             (multi pattern-after asc? ic cc fc rta)
             success?))]
         [(cons (? abstract-function?) _)
          ;; we don't replace these in a multi (they can only occur inside constraints)
          obj/success?])]
      [(cons
        (and
         subst-context
         (or
          (abstract-atom sym (list-rest h t))
          (abstract-atom* sym (list-rest h t))))
        #f)
       (match-let
           ([(cons h-after success-1?)
             (rec (cons h #f))]
            [(cons t-after success-2?)
             (rec (cons t #f))])
         (cond
           [success-1?
            (cons ((compound-constructor subst-context) sym (cons h-after t)) success-1?)]
           [success-2?
            (cons ((compound-constructor subst-context) sym (cons h t-after)) success-2?)]
           [else obj/success?]))]
      [(cons
        (and
         subst-context
         (or
          (abstract-function  sym (list-rest h t))
          (abstract-function* sym (list-rest h t))))
        #f)
       (if (equal? subst-context (car s))
           (cons (cdr s) #t)
           (match-let
               ([(cons h-after success-1?)
                 (rec (cons h #f))]
                [(cons t-after success-2?)
                 (rec (cons t #f))])
             (cond
               [success-1?
                (cons ((compound-constructor subst-context) sym (cons h-after t)) success-1?)]
               [success-2?
                (cons ((compound-constructor subst-context) sym (cons h t-after)) success-2?)]
               [else obj/success?])))]
      [(cons
        (and
         subst-context
         (or
          (abstract-function  sym (list))
          (abstract-function* sym (list))))
        #f)
       (if (equal? subst-context (car s))
           (cons (cdr s) #t)
           obj/success?)]
      [else obj/success?]))
  (car (aux s (cons acc #f))))
(module+ test
  (check-equal?
   (apply-compound-subst
    (cons (abstract-function 'nil empty) (a 1000))
    (interpret-abstract-conjunction "foo(bar(a1,nil)),baz(nil)"))
   (interpret-abstract-conjunction "foo(bar(a1,a1000)),baz(nil)")))

(define (deconstruct ac)
  (let* ([compounds (extract-abstract-compounds ac)]
         [var-occurrences (extract-all-variables/duplicates ac)]
         [max-var-index
          (let ([indices
                 (map
                  (match-lambda
                    [(a i) i]
                    [(g i) i]
                    [(a* _ _ i) i]
                    [(g* _ _ i) i])
                  var-occurrences)])
            (apply max (cons 0 indices)))]
         [substs
          (car
           (map-accumulatel
            compute-subst
            (add1 max-var-index)
            compounds))])
    (cons
     (foldr
      apply-compound-subst
      ac
      substs)
     substs)))
(module+ test
  (check-equal?
   (deconstruct
    (interpret-abstract-conjunction
     "sift(a40,a39),alt_length([g28|a390],g20)"))
   (cons
    (interpret-abstract-conjunction
     "sift(a40,a39),alt_length(a391,g20)")
    (list
     (cons
      (abstract-function 'cons (list (g 28) (a 390)))
      (a 391)))))
  (check-equal?
   (deconstruct
    (list
     (abstract-atom 'filter (list (g 89) (a 121) (a 122)))
     (multi
      (list
       (abstract-atom*
        'filter
        (list
         (g* 1 'i 25)
         (a* 1 'i 34)
         (a* 1 'i 35))))
      #t
      (list
        (cons (a* 1 1 34)
              (abstract-function 'cons (list (g 90) (a 1220)))))
      (list
        (cons (a* 1 'i+1 34)
              (a* 1 'i 35)))
      (list
        (cons (a* 1 'L 35)
              (a 124))))))
   (cons
    (list
     (abstract-atom 'filter (list (g 89) (a 121) (a 122)))
     (multi
      (list
       (abstract-atom*
        'filter
        (list
         (g* 1 'i 25)
         (a* 1 'i 34)
         (a* 1 'i 35))))
      #t
      (list
        (cons (a* 1 1 34)
              (abstract-function 'cons (list (g 90) (a 1220)))))
      (list
        (cons (a* 1 'i+1 34)
              (a* 1 'i 35)))
      (list
        (cons (a* 1 'L 35)
              (a 124)))))
    (list))))
(provide
 (proc-doc/names
  deconstruct
  (->
   (listof abstract-conjunct?)
   (cons/c
    (listof abstract-conjunct?)
    (listof
     (cons/c
      (or/c abstract-function? abstract-function*?)
      (or/c abstract-variable? abstract-variable*?)))))
  (ac)
  @{Computes fresh abstract variables to replace the compounds in an abstract conjunction in @code{generalization/2} clauses required by the Prolog meta-interpreter.}))

(define (synth-str concrete-e)
  (match concrete-e
    [(? list?)
     (string-join
      (map synth-str concrete-e)
      ",")]
    [(concrete-multi lst)
     (format
      "multi(~a)"
      (synth-str lst))]
    [(or
      (atom sym (list))
      (function sym (list)))
     (format "~a" sym)]
    [(or
      (atom sym lst)
      (function sym lst))
     (format "~a(~a)" sym (synth-str lst))]
    [(variable v)
     (symbol->string v)]
    [else (error (format "can't print this: ~a" else))]))
(module+ test
  ; really shows the need for ag lang extension...
  (check-equal?
   (synth-str
    (list
     (atom 'integers (map variable '(G24 A31A)))
     (concrete-multi
      (function
       cons-symbol
       (list
        (function
         'building_block
         (list
          (function
           cons-symbol
           (list         
            (function
             'filter
             (map variable '(G25 A31B A35A)))
            concrete-nil))))
        (function
         cons-symbol
         (list
          (function
           'building_block
           (list
            (function
             cons-symbol
             (list
              (function
               'filter
               (map variable '(G27 A35B A38A)))
              concrete-nil))))
          concrete-nil)))))
     (atom 'filter (map variable '(G28 A38B A40A)))
     (atom 'sift (map variable '(A40B A39A)))
     (atom 'alt_length (map variable '(G28A39B G20)))))   "integers(G24,A31A),multi('[|]'(building_block('[|]'(filter(G25,A31B,A35A),[])),'[|]'(building_block('[|]'(filter(G27,A35B,A38A),[])),[]))),filter(G28,A38B,A40A),sift(A40B,A39A),alt_length(G28A39B,G20)"))
(provide synth-str)

(define (generalization/2-head-arg1 ac)
  (match-let*
      ([(list untangled aliasing)
        (untangle ac)]
       [(cons deconstructed compound-replacements)
        (deconstruct untangled)])
    (concrete-synth-counterpart deconstructed)))
;(format "[~a]" (synth-str counterpart))
(module+ test
  ;; based on node 33
  (check-equal?
   (format
    "[~a]"
    (synth-str
     (generalization/2-head-arg1
      (interpret-abstract-conjunction
       "integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),filter(g4,a3,a4),sift(a4,a5),alt_length([g5|a5],g6)"))))
   "[integers(G1,A6),filter(G2,A1,A7),filter(G3,A2,A8),filter(G4,A3,A9),sift(A4,A10),alt_length(A11,G6)]")
  ;; based on node 46
  (check-equal?
   (format
    "[~a]"
    (synth-str
     (generalization/2-head-arg1
      (append
       (interpret-abstract-conjunction
        "integers(g1,a6),filter(g2,a1,a7)")
       (cons
        (multi
         (list
          (abstract-atom*
           'filter
           (list
            (g* 1 'i 1)
            (a* 1 'i 1)
            (a* 1 'i 3))))
         #t
         (list
           (cons (a* 1 1 1) (a 2)))
         (list
           (cons
            (a* 1 'i+1 1)
            (a* 1 'i   2))) ; unseen aliasing is not an issue for this part of generation
         (list
           (cons
            (a* 1 'L 2)
            (a 8))))
        (interpret-abstract-conjunction
         "filter(g3,a3,a9),sift(a4,a10),alt_length(a5,g4)"))))))
   "[integers(G1,A6),filter(G2,A1,A7),multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i3),[])),Tail1)),filter(G3,A3,A9),sift(A4,A10),alt_length(A5,G4)]")
  
  ;; based on node 80  
  (check-equal?
   (format
    "[~a]"
    (synth-str
     (generalization/2-head-arg1
      (append
       (list
        (abstract-atom 'integers (list (g 1) (a 7)))
        (multi
         (list
          (abstract-atom*
           'filter
           (list
            (g* 1 'i 1)
            (a* 1 'i 1)
            (a* 1 'i 3))))
         #t
         (list
           (cons
            (a* 1 1 1)
            (a 1)))
         (list
           (cons
            (a* 1 'i+1 1)
            (a* 1 'i   2)))
         (list
           (cons
            (a* 1 'L 2)
            (a 8))))
        (abstract-atom 'filter (list (g 2) (a 2) (a 9)))
        (multi
         (list
          (abstract-atom*
           'filter
           (list
            (g* 2 'i 1)
            (a* 2 'i 1)
            (a* 2 'i 3))))
         #t
         (list
           (cons
            (a* 2 1 1)
            (a 3)))
         (list
           (cons
            (a* 2 'i+1 1)
            (a* 2 'i   2)))
         (list
           (cons
            (a* 2 'L 2)
            (a 10)))))
       (interpret-abstract-conjunction
        "filter(g3,a4,a11),sift(a5,a12),alt_length(a6,g4)")))))
   (string-append
    "[integers(G1,A7),"
    "multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i3),[])),Tail1)),"
    "filter(G2,A2,A9),"
    "multi('[|]'(building_block('[|]'(filter(G2i1,A2i1,A2i3),[])),Tail2)),"
    "filter(G3,A4,A11),"
    "sift(A5,A12),"
    "alt_length(A6,G4)]")))

(define (generalization/2-head-arg2 arg1 bbs)
  (define (elem-append lst e) (append lst (list e)))
  (define bb-ranges (append-map (λ (p) (car p)) bbs))
  (define bb-starts (map index-range-start bb-ranges))
  (define bb-ends (map index-range-end-before bb-ranges))
  (define max-ti
    (apply
     max
     (map
      (compose
       string->number
       (λ (s) (substring s 4))
       symbol->string
       variable-name)
      (cons
       (variable 'Tail0)
       (filter
        (compose (λ (s) (string-prefix? s "Tail")) symbol->string variable-name)
        (extract-all-concrete-variables arg1))))))
  (define (blockify atoms)
    (function
     'building_block
     (list
      (concrete-listify
       (map atom->function atoms)))))
  (define (wrapper-proc conjunct idx acc)
    (match acc
      [(cons conjunction building-block)
       (match* ((->boolean (member idx bb-starts))
                (->boolean (member idx bb-ends))
                (->boolean building-block)
                (concrete-multi? conjunct))
         [(#f #f #f  _) (cons (elem-append conjunction conjunct) #f)]
         [(#f #f #t #f) (cons conjunction (elem-append building-block conjunct))]
         [(#f #t #f  _) (cons (elem-append conjunction conjunct) #f)]
         [(#f #t #t  _) (cons (append conjunction (list (blockify building-block)) (list conjunct)) #f)]
         [(#t #f #f #f) (cons conjunction (list conjunct))]
         [(#t #f #f #t) (cons (elem-append conjunction conjunct) #f)]
         [(#t #t #f #f) (cons conjunction (list conjunct))]
         [(#t #t #f #t) (cons (elem-append conjunction conjunct) #f)]
         [(#t #t #t #f) (cons (append conjunction (list (blockify building-block))) (list conjunct))]
         [(#t #t #t #t) (cons (append conjunction (list (blockify building-block)) (list conjunct)) #f)]
         [( _  _  _  _) (error "impossible pattern for building block grouping")])]))
  (define (complete-wrap res)
    (match res
      [(cons con #f) con]
      [(cons con bb)
       (append
        con
        (list
         (function
          'building_block
          bb)))]))
  (define (group-bb-info bbs)
    (car
     (map-accumulatel
      (λ (e acc)
        (match e
          [(cons rngs pos)
           (let ([new-range
                  (index-range
                   (index-range-start
                    (first rngs))
                   (+
                    (index-range-start
                     (first rngs))
                    (length rngs)))]
                 [δ (- (+
                        (index-range-start
                         (first rngs))
                        (length rngs))
                       (index-range-end-before (last rngs)))])
             (cons
              (cons
               new-range
               (- pos acc))
              (+ acc δ)))]))
      0
      bbs)))
  (let* ([wrapped
          (complete-wrap
           (foldl
            wrapper-proc
            (cons empty #f)
            arg1
            (range (length arg1))))]
         [grouped-bbs
          (group-bb-info bbs)])
    (foldl
     (λ (wci acc)
       (match acc
         [(list conjunction appends ti)
          (let
              ([containing-range
                (findf
                 (λ (r) ; only using car r / car containing-range -> probably don't need multi index position...
                   (between?
                    wci
                    (index-range-start (car r))
                    (sub1
                     (index-range-end-before (car r)))))
                 grouped-bbs)])
            (cond
              [(and containing-range (= (index-range-start (car containing-range)) wci))
               (match-let ([(list new-multi new-appends new-ti)
                            (group-multi-range wrapped (car containing-range) ti)])
                 (list
                  (append conjunction (list new-multi))
                  (if
                   (not (null? new-appends))
                   (append appends new-appends)
                   appends)
                  new-ti))] ; take all the conjuncts in the range and group them
              [containing-range acc]
              [else (list (append conjunction (list (list-ref wrapped wci))) appends ti)]))]))
     (list empty empty (add1 max-ti)) ; conjunction, append clauses, tail index
     (stream->list
      (in-range
       (length wrapped))))))

(define (group-multi-range conjunction/bbs rng fresh-tail-index)
  (define (contents-as-list e)
    
    (if (concrete-multi? e)
        (racket-listify
         (concrete-multi-lst e))
        (list e)))
  (define (folded-proc e acc)
    (match acc
      ; contents contains contents of a concrete-multi, but it is an (improper) Racket list!
      ; building blocks are just list elements, but in case an existing multi is added, contents becomes an improper list
      [(list contents appends ti)
       #:when (variable? (improper-tail contents))
       (list
        (append
         (proper-prefix contents)
         (variable (format-symbol "Tail~a" fresh-tail-index)))
        (cons
         (list
          (improper-tail contents)
          (contents-as-list e)
          (variable
           (format-symbol "Tail~a" fresh-tail-index)))
         appends)
        (add1 ti))]
      [(list contents appends ti)
       (list
        (append contents (contents-as-list e))
        appends
        ti)]))
  (let* ([rng-contents
          (take
           (drop
            conjunction/bbs
            (index-range-start rng))
           (-
            (index-range-end-before rng)
            (index-range-start rng)))]
         [raw (foldl folded-proc (list empty empty fresh-tail-index) rng-contents)])
    (list (concrete-multi (concrete-listify (first raw))) (second raw) (third raw))))
; TODO: test for building block followed by multi, for multi followed by bb, for several tailed multis
(module+ test
  (check-equal?
   (group-multi-range
    (list
     (function
      'building_block
      (list
       (concrete-listify
        (list
         (function 'filter (list (variable 'G1) (variable 'A1) (variable 'A2)))))))
     (function
      'building_block
      (list
       (concrete-listify
        (list
         (function 'filter (list (variable 'G2) (variable 'A2) (variable 'A3))))))))
    (index-range 0 2)
    1)
   (list
    (concrete-multi
     (concrete-listify
      (list
       (function
        'building_block
        (list
         (concrete-listify
          (list
           (function 'filter (list (variable 'G1) (variable 'A1) (variable 'A2)))))))
       (function
        'building_block
        (list
         (concrete-listify
          (list
           (function 'filter (list (variable 'G2) (variable 'A2) (variable 'A3))))))))))
    empty
    1)))

; TODO definitely needs tests for when existing multi is extended
(module+ test
  (check-equal?
   (format
    "[~a]"
    (synth-str
     (first
      (generalization/2-head-arg2
       (generalization/2-head-arg1
        (interpret-abstract-conjunction
         "integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),filter(g4,a3,a4),sift(a4,a5),alt_length([g5|a5],g6)"))
       (list
        (cons
         (list
          (index-range 1 2)
          (index-range 2 3))
         1))))))
   "[integers(G1,A6),multi('[|]'(building_block('[|]'(filter(G2,A1,A7),[])),'[|]'(building_block('[|]'(filter(G3,A2,A8),[])),[]))),filter(G4,A3,A9),sift(A4,A10),alt_length(A11,G6)]")
  (let* ([pre
          (list
           (abstract-atom 'integers (list (g 36) (a 820)))
           (abstract-atom 'filter (list (g 62) (a 821) (a 830)))
           (multi
            (list
             (abstract-atom*
              'filter
              (list
               (g* 1 'i 1)
               (a* 1 'i 1)
               (a* 1 'i 2))))
            #t
            (list
              (cons
               (a* 1 1 1)
               (a 831)))
            (list
              (cons
               (a* 1 'i+1 1)
               (a* 1 'i   2)))
            (list
              (cons
               (a* 1 'L 2)
               (a 380))))
           (abstract-atom 'filter (list (g 28) (a 381) (a 400)))
           (abstract-atom 'sift (list (a 401) (a 420)))
           (abstract-atom 'alt_length (list (a 421) (g 32))))]
         [arg1 (generalization/2-head-arg1 pre)])
    (check-equal?
     (format
      "[~a]"
      (synth-str
       (first
        (generalization/2-head-arg2
         arg1
         (list
          (cons
           (list
            (index-range 1 2)
            (index-range 2 3))
           1))))))
     "[integers(G36,A820),multi('[|]'(building_block('[|]'(filter(G62,A821,A830),[])),'[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1))),filter(G28,A381,A400),sift(A401,A420),alt_length(A421,G32)]")))
