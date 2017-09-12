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
         (only-in "abstraction-inspection-utils.rkt"
                  extract-abstract-compounds
                  extract-variables/duplicates
                  extract-all-variables/duplicates
                  extract-all-variables/duplicates/exclude-consecutive
                  get-multi-id
                  maximum-var-index)
         "abstract-multi-domain.rkt"
         (only-in "abstract-renaming.rkt" rename-apart)
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
    [(node (generalization _ _ idx2 _ _ _) _)
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
(provide
 (proc-doc/names
  display-mi-map
  (->
   node?
   void?)
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
     "foo(γ1,α1),bar(γ2,α2),baz(γ3,α3),quux(γ3,α3),narf(γ3,α3),poit(γ4,α4)")
    (list
     (cons (a 1) (a 5))
     (cons (a 3) (a 6))))
   (interpret-abstract-conjunction
    "foo(γ1,α5),bar(γ2,α2),baz(γ3,α6),quux(γ3,α3),narf(γ3,α3),poit(γ4,α4)")))
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
    (extract-all-variables/duplicates/exclude-consecutive init-ac))
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
    (interpret-abstract-conjunction "integers(γ1,α1),filter(γ2,α1,α2),filter(γ3,α2,α3),filter(γ4,α3,α4),sift(α4,α5),length(α5,γ5)"))
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
     (abstract-atom 'length (list (a 5) (g 4)))))
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
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))) ; note: use of a variable in consecutive is not considered aliasing!
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
      (interpret-abstract-conjunction "collect(γ4,α7),eq(α6,α7)"))))
   (list
    (append
     (interpret-abstract-conjunction "collect(γ1,α8),collect(γ2,α9),append(α1,α2,α10),collect(γ3,α11),append(α3,α4,α12)")
     (cons
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 4)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
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
     [(cons (cons (abstract-function sym args) top?) repl-avar)
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
         [(cons (cons afunc _) _)
          (concrete-synth-counterpart afunc)])
       repl))))))

(define (append-atoms appends)
  (map
   (match-lambda
     [(list (variable name-tail1) contents-as-list (variable name-tail2))
      (format
       "append(~a,~a,~a)"
       name-tail1
       (concrete-listify
        contents-as-list)
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
    [((multi patt _ _ _ _) (concrete-multi c-lst))
     (format
      "check_pattern(~a,building_block([~a]))"
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
    (define (replacement-proc e acc)
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
         [correctly-aliased (correct-aliasing localized-pattern (consecutive-constraints (multi-consecutive a-multi)) fresh)]
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

(define (last-check a-multi c-multi)
  ;; note: this is different from the erase-or-substitute used for init check!
  ;; it is similar, though, so maybe consolidating is possible...
  (define (erase-or-substitute constraints e)
    (match e
      [(? list?)
       (map (curry erase-or-substitute constraints) e)]
      [(abstract-atom sym args)
       (abstract-atom sym (map (curry erase-or-substitute constraints) args))]
      [(abstract-function sym args)
       (abstract-function sym (map (curry erase-or-substitute constraints) args))]
      [(? abstract-variable?)
       (aif
        (hash-ref constraints e #f)
        it
        (abstract-function 'd empty))]))
  (let* ([localized-pattern
          (localize (multi-conjunction a-multi))]
         [localized-constraints
          (map (match-lambda [(cons k v) (cons (localize k) v)]) (final-constraints (multi-final a-multi)))]
         [aarg2
          (erase-or-substitute
           (make-hash localized-constraints)
           localized-pattern)])
    (format "check_last(~a,building_block([~a]))" (synth-str (concrete-multi-lst c-multi)) (synth-str (concrete-synth-counterpart aarg2)))))

(define (init-check a-multi c-multi)
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
        (function '- (list (variable '_) (function 'a empty))))]))
  (let* ([localized-pattern
          (localize (multi-conjunction a-multi))]
         [localized-constraints
          (map (match-lambda [(cons k v) (cons (localize k) v)]) (init-constraints (multi-init a-multi)))]
         [aarg2
          (erase-or-substitute
           (make-hash localized-constraints)
           localized-pattern)])
    (format "check_init(~a,building_block([~a]))" (synth-str (first-elem-lst (concrete-multi-lst c-multi))) (synth-str aarg2))))

(define (multi-checks acon con additional-vars)
  (foldl
   (λ (a c acc)
     (cond
       [(and (multi? a) (concrete-multi? c))
        (list
         (pattern-check a c)
         (init-check a c)
         (consecutive-check a c acon additional-vars)
         (last-check a c))]
       [(xor (multi? a) (concrete-multi? c))
        (error "abstract and concrete conjunction are out of sync")]
       [else acc]))
   empty
   acon
   con))

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
         (aliasing-constraints aliasing) ; should not produce this if abstract variable only occurs in Last
         (map (compose (λ (e) (format "ground(~a)" e)) synth-str) (groundness-atoms arg1 compound-replacements))
         (append-atoms appends) ; introduces new vars TailN but these will never clash with other names
         (multi-checks
          parent
          arg1
          (append
            (map cdr aliasing)
            (extract-variables/duplicates deconstructed)))
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
            "integers(γ1,α1),"
            "filter(γ2,α1,α2),"
            "filter(γ3,α2,α3),"
            "filter(γ4,α3,α4),"
            "sift(α4,α5)"))
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
            "integers(γ1,α1),"
            "filter(γ2,α1,α2)"))
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
            (init
             (list
              (cons (a* 1 1 1)
                    (a 2))))
            (consecutive
             (list
              (cons (a* 1 'i+1 1)
                    (a* 1 'i 2))))
            (final
             (list
              (cons (a* 1 'L 2)
                    (a 3))))))
          (interpret-abstract-conjunction
           (string-append
            "filter(γ3,α3,α4),"
            "sift(α4,α5),"
            "alt_length(α5,γ4)")))]
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
      "filter(G2,A1,A7),"
      "multi('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1)),"
      "filter(G3,A3,A9),"
      "sift(A4,A10),"
      "alt_length(A5,G4)],"
      "["
      "integers(G1,A6),"
      "multi('[|]'("
        "building_block('[|]'(filter(G2,A1,A7),[])),"
        "'[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),"
          "Tail1))),"
      "filter(G3,A3,A9),"
      "sift(A4,A10),"
      "alt_length(A5,G4)]) :- \n"
      "  A1 == A6,\n"
      "  A2 == A7,\n"
      "  A4 == A9,\n"
      "  A5 == A10,\n"
      "  ground(G1),\n"
      "  ground(G2),\n"
      "  ground(G1i1),\n"
      "  ground(G3),\n"
      "  ground(G4),\n"
      "  check_pattern('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),building_block([filter(_-g,_-a,_-a)])),\n"
      "  check_init('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),[]),building_block([filter(-(_,a),A2,-(_,a))])),\n"
      "  check_consecutive('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),building_block([filter(G5,A11,A12)]),building_block([filter(G7,A12,A13)])),\n"
      ; note: A8 should be safe here, because we don't have an aliasing constraint for it
      ; it is introduced for the aliasing constraints (by untangle)...
      ; check_last takes care of this, anyway...
      ; so should untangle always ignore Last...? looks very iffy for the abstract results, though...
      "  check_last('[|]'(building_block('[|]'(filter(G1i1,A1i1,A1i2),[])),Tail1),building_block([filter(d,d,A3)])),\n"
      "  !."))))

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
    [(node (generalization conjunction2 _ _ _ _ building-blocks) _)
     (display
      (generate-generalization-clause
       conjunction1
       conjunction2
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
  (visit generalization-clause-visitor tree)
  tree)
(provide
 (proc-doc/names
  display-generalization-clauses
  (->
   node?
   void?)
  (tree)
  @{Prints out the @code{generalization/2} clauses required by the Prolog meta-interpreter.}))

; note: compound can be an abstract-function or a pair (abstract-function*, multi id)
; also note: this is a map-accumulated function, not a folded one!
(define (compute-subst comp occs)
  (match comp
    [(cons c top?)
     (let* ([corresponding-maximum
             (if (abstract-function? c)
                 (hash-ref occs 'a (a 1))
                 (hash-ref
                  occs
                  (format-symbol "a-~a-i" (cdr c))
                  (a* (cdr c) 'i 1)))]
            [new-maximum
             ((extract-avar-constructor corresponding-maximum)
              (add1 (local-index corresponding-maximum)))])
       (cons
        (cons (cons c top?) new-maximum)
        (hash-set occs (symbolize-avar-constructor new-maximum) new-maximum)))]))
(module+ test
  (check-equal?
   (compute-subst
    (cons (interpret-abstract-term "foo(bar(baz))") #t)
    (hash 'a (a 7)))
   (cons
    (cons (cons (interpret-abstract-term "foo(bar(baz))") #t) (a 8))
    (hash 'a (a 8))))
  (check-equal?
   (compute-subst
    (cons (cons (abstract-function* 'nil empty) 3) #t)
    (hash 'a-3-i (a* 3 'i 10)))
   (cons
    (cons (cons (cons (abstract-function* 'nil empty) 3) #t) (a* 3 'i 11))
    (hash 'a-3-i (a* 3 'i 11)))))

; NAME IS TERRIBLE!
(define (apply-subst e acc)
  (if (cdr acc)
      acc
      (match (car acc) ; reusing this a few times...
        [(list-rest h t)
         (match-let
             ([(cons h-after success-1?)
               (apply-subst e (cons h #f))]
              [(cons t-after success-2?)
               (apply-subst e (cons t #f))])
           (cond
             [success-1?
              (cons (cons h-after t) success-1?)]
             [success-2?
              (cons (cons h t-after) success-2?)]
             [else acc]))]
        [(multi patt asc? (init ic) (consecutive cc) (final fc))
         (match e
           [(cons (cons (? abstract-function*?) m-id) _)
            #:when (equal? m-id (get-multi-id (car acc)))
            (match-let
                ([(cons pattern-after success?)
                  (apply-subst e (cons patt #f))])
              (cons
               (multi pattern-after asc? (init ic) (consecutive cc) (final fc))
               success?))]
           [(cons (cons (? abstract-function*?) m-id) _) acc]
           [(cons (? abstract-function?) _)
            (match-let
                ([(cons init-after success?)
                  (apply-subst e (cons (map cdr ic) #f))])
              (if success?
                  (cons
                   (multi
                    patt
                    asc?
                    (init
                     (map cons (map car ic) init-after))
                    (consecutive cc)
                    (final fc))
                   success?)
                  acc))])]
        [(or
          (abstract-atom  sym (list-rest h t))
          (abstract-atom* sym (list-rest h t)))
         (match-let
             ([(cons h-after success-1?)
               (apply-subst e (cons h #f))]
              [(cons t-after success-2?)
               (apply-subst e (cons t #f))])
           (cond
             [success-1?
              (cons ((compound-constructor (car acc)) sym (cons h-after t)) success-1?)]
             [success-2?
              (cons ((compound-constructor (car acc)) sym (cons h t-after)) success-2?)]
             [else acc]))]
        [(or
          (abstract-function  sym (list))
          (abstract-function* sym (list)))
         (if
          (equal?
           (car acc)
           (car e))
          (cons (cdr e) #t)
          acc)]
        [(or
          (abstract-function  sym (list-rest h t))
          (abstract-function* sym (list-rest h t)))
         (if (equal? (car acc) (car e))
             (cons (cdr e) #t)
             (match-let
                 ([(cons h-after success-1?)
                   (apply-subst e (cons h #f))]
                  [(cons t-after success-2?)
                   (apply-subst e (cons t #f))])
               (cond
                 [success-1?
                  (cons ((compound-constructor (car acc)) sym (cons h-after t)) success-1?)]
                 [success-2?
                  (cons ((compound-constructor (car acc)) sym (cons h t-after)) success-2?)]
                 [else acc])))]
        [_ acc])))
(module+ test
  (check-equal?
   (apply-subst
    (cons (abstract-function 'nil empty) (a 1000))
    (cons (interpret-abstract-conjunction "foo(bar(α1,nil)),baz(nil)") #f))
   (cons (interpret-abstract-conjunction "foo(bar(α1,α1000)),baz(nil)") #t)))

(define (deconstruct ac)
  (let* ([compounds (extract-abstract-compounds ac)]
         [var-occurrences (extract-all-variables/duplicates ac)]
         [max-var-indices (foldl find-max-vars (hash) var-occurrences)]
         [substs (car (map-accumulatel compute-subst max-var-indices compounds))])
    (cons
     (car
      (foldr
       apply-subst
       (cons ac #f)
       (filter-map
        (λ (s)
          (and
           (cdr (car s))
           (cons (car (car s)) (cdr s))))
        substs)))
     ;; TODO: can remove info about top-level or not here
     substs)))
(module+ test
  (check-equal?
   (deconstruct
    (interpret-abstract-conjunction
     "sift(α40,α39),alt_length([γ28|α390],γ20)"))
   (cons
    (interpret-abstract-conjunction
     "sift(α40,α39),alt_length(α391,γ20)")
    (list
     (cons
      (cons (abstract-function 'cons (list (g 28) (a 390))) #t)
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
      (init
       (list
        (cons (a* 1 1 34)
              (abstract-function 'cons (list (g 90) (a 1220))))))
      (consecutive
       (list
        (cons (a* 1 'i+1 34)
              (a* 1 'i 35))))
      (final
       (list
        (cons (a* 1 'L 35)
              (a 124)))))))
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
      (init
       (list
        (cons (a* 1 1 34)
              (a 1221))))
      (consecutive
       (list
        (cons (a* 1 'i+1 34)
              (a* 1 'i 35))))
      (final
       (list
        (cons (a* 1 'L 35)
              (a 124))))))
    (list
     (cons
      (cons
       (abstract-function 'cons (list (g 90) (a 1220)))
       #t)
      (a 1221))))))
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
  ; really shows the need for αγ lang extension...
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
       "integers(γ1,α1),filter(γ2,α1,α2),filter(γ3,α2,α3),filter(γ4,α3,α4),sift(α4,α5),alt_length([γ5|α5],γ6)"))))
   "[integers(G1,A6),filter(G2,A1,A7),filter(G3,A2,A8),filter(G4,A3,A9),sift(A4,A10),alt_length(A11,G6)]")
  ;; based on node 46
  (check-equal?
   (format
    "[~a]"
    (synth-str
     (generalization/2-head-arg1
      (append
       (interpret-abstract-conjunction
        "integers(γ1,α6),filter(γ2,α1,α7)")
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
         (init
          (list
           (cons (a* 1 1 1) (a 2))))
         (consecutive
          (list
           (cons
            (a* 1 'i+1 1)
            (a* 1 'i   2)))) ; unseen aliasing is not an issue for this part of generation
         (final
          (list
           (cons
            (a* 1 'L 2)
            (a 8)))))
        (interpret-abstract-conjunction
         "filter(γ3,α3,α9),sift(α4,α10),alt_length(α5,γ4)"))))))
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
         (init
          (list
           (cons
            (a* 1 1 1)
            (a 1))))
         (consecutive
          (list
           (cons
            (a* 1 'i+1 1)
            (a* 1 'i   2))))
         (final
          (list
           (cons
            (a* 1 'L 2)
            (a 8)))))
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
         (init
          (list
           (cons
            (a* 2 1 1)
            (a 3))))
         (consecutive
          (list
           (cons
            (a* 2 'i+1 1)
            (a* 2 'i   2))))
         (final
          (list
           (cons
            (a* 2 'L 2)
            (a 10))))))
       (interpret-abstract-conjunction
        "filter(γ3,α4,α11),sift(α5,α12),alt_length(α6,γ4)")))))
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
     (list empty empty 1)
     (stream->list
      (in-range
       (length wrapped))))))

(define (group-multi-range conjunction/bbs rng fresh-tail-index)
  (define (contents-as-list e)
    (define (racket-listify lst)
      (match lst
        [(variable sym) lst] ; can create improper lists!
        [(function (quote \[\]) (list)) empty]
        [(function (quote \'\[\|\]\') (list-rest c1 c2))
         (cons c1 (racket-listify (first c2)))]))
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
         "integers(γ1,α1),filter(γ2,α1,α2),filter(γ3,α2,α3),filter(γ4,α3,α4),sift(α4,α5),alt_length([γ5|α5],γ6)"))
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
            (init
             (list
              (cons
               (a* 1 1 1)
               (a 831))))
            (consecutive
             (list
              (cons
               (a* 1 'i+1 1)
               (a* 1 'i   2))))
            (final
             (list
              (cons
               (a* 1 'L 2)
               (a 380)))))
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