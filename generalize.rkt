#lang at-exp racket
(require
  racket/struct
  scribble/srcdoc
  "abstract-multi-domain.rkt"
  (only-in "abstract-domain-ordering.rkt" renames?)
  (only-in "abstraction-inspection-utils.rkt" assemble-var-indices extract-subscripted-variables extract-variables)
  (only-in "abstract-renaming.rkt" offset-vars)
  (only-in "abstract-unify.rkt" abstract-unify)
  (only-in "abstract-substitution.rkt" abstract-equality apply-substitution)
  (only-in "data-utils.rkt" some-v)
  "gen-graph-structs.rkt"
  (only-in "generational-graph.rkt" gen-number< gen-add1 gen-sub1)
  (only-in "multi-folding-unfolding.rkt" remove-multi-subscripts))
(require (for-doc scribble/manual))

;; gets the origin from either a generation or a generation range
(define (gen-thing-origin gen-thing)
  (match gen-thing
    [(? gen-range?) (gen-range-origin gen-thing)]
    [(? gen?) (gen-origin gen-thing)]))

;; accumulator struct used when folding over a level of identified gen nodes
(struct grouping (completed potential current-gen next-multi-id)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'grouping)
      (λ (obj) (list (grouping-completed obj)
                     (grouping-potential obj)
                     (grouping-current-gen obj)
                     (grouping-next-multi-id obj)))))])

(define (prefix-subscripts multi-id idx var)
  (define curried (curry prefix-subscripts multi-id idx))
  (match var
    [(? list?) (map curried var)]
    [(abstract-atom sym args)
     (abstract-atom* sym (map curried args))]
    [(abstract-function sym args)
     (abstract-function* sym (map curried args))]
    [(a i) (a* multi-id idx i)]
    [(g i) (g* multi-id idx i)]))

(define (multi-id m)
  (define vars (extract-subscripted-variables m))
  (abstract-variable*-multi-id (first vars)))

;; this turns the "growing abstraction" into a single multi abstraction
;; that is, a single generation stays a single generation
;; a single multi stays a single multi
;; two consecutive generations become a single multi
;; a generation and a multi (in either order) become a single multi
;; two multis become a single multi
;; note that this should never fail
;; if a conjunct is in the growing abstraction, it belongs there and it respects the pattern
;; also returns the next valid fresh id for a multi (which may be the input id if it was not needed)
(define (group-sequential-generations potential fresh-id)
  (define partitioning (group-by gen-node-range potential))
  (define (aux partitioning)
    (match partitioning
      [(list _)
       (cons (map gen-node-conjunct potential) fresh-id)]
      [(list
        (and (list-rest (gen-node (? abstract-atom?) _ (gen genn-1 _) _ _) first-rest) lvl-1)
        (and (list-rest (gen-node (? abstract-atom?) _ (gen genn-2 _) _ _) second-rest) lvl-2))
       (cons
        (let* ([gen-1 (map gen-node-conjunct lvl-1)]
               [gen-2 (map gen-node-conjunct lvl-2)]
               [offset (apply max (assemble-var-indices (λ (_) #t) (append gen-1 gen-2)))]
               [offset-gen-2 (offset-vars gen-2 offset offset)]
               [subst (some-v (abstract-unify (map abstract-equality gen-1 offset-gen-2) 0))]
               [shared (filter (match-lambda [(abstract-equality v1 v2) (and (member (offset-vars v2 (- offset) (- offset)) (extract-variables gen-1)) (member (offset-vars v2 (- offset) (- offset)) (extract-variables gen-2)))]) subst)]
               [new-consecutive (map (match-lambda [(abstract-equality (a idx1) (a idx2)) (cons (a* fresh-id 'i+1 idx1) (a* fresh-id 'i (- idx2 offset)))] [(abstract-equality (g idx1) (g idx2)) (cons (g* fresh-id 'i+1 idx1) (g* fresh-id 'i (- idx2 offset)))]) shared)]
               [new-final (map (match-lambda [(abstract-equality (a idx1) (a idx2)) (cons (a* fresh-id 'L idx1) (a (- idx2 offset)))] [(abstract-equality (g idx1) (g idx2)) (cons (g* fresh-id 'L idx1) (g (- idx2 offset)))]) subst)])
          (list
           (multi
            (prefix-subscripts fresh-id 'i (map gen-node-conjunct lvl-1))
            (gen-number< genn-1 genn-2)
            (init (map (λ (v) (cons (prefix-subscripts fresh-id 1 v) v)) (extract-variables (map gen-node-conjunct lvl-1))))
            (consecutive new-consecutive)
            (final new-final))))
        (add1 fresh-id))]
      [(list
        (and (list-rest (gen-node (? abstract-atom?) _ _ _ _) first-rest) single-gen)
        (list (gen-node (and (? multi?) existing-multi) _ _ _ _)))
       (let* ([existing-instance (map gen-node-conjunct single-gen)]
              [subscriptless-instance (remove-multi-subscripts (multi-conjunction existing-multi))]
              [offset (apply max (assemble-var-indices (λ (_) #t) subscriptless-instance))]
              [unifiable-instance (offset-vars subscriptless-instance offset offset)]
              [unification (some-v (abstract-unify (list (abstract-equality unifiable-instance existing-instance)) 0))]
              [new-init
               (init
                (map
                 (match-lambda
                   [(abstract-equality var1 var2)
                    (cons
                     (prefix-subscripts (multi-id existing-multi) 1 (offset-vars var1 (- offset) (- offset)))
                     var2)])
                 unification))])
         (cons (list (struct-copy multi existing-multi [init new-init])) fresh-id))]
      [(list
        (list (gen-node (and (? multi?) existing-multi) _ _ _ _))
        (and (list-rest (gen-node (? abstract-atom?) _ _ _ _) first-rest) single-gen))
       (let* ([existing-instance (map gen-node-conjunct single-gen)]
              [subscriptless-instance (remove-multi-subscripts (multi-conjunction existing-multi))]
              [offset (apply max (assemble-var-indices (λ (_) #t) subscriptless-instance))]
              [unifiable-instance (offset-vars subscriptless-instance offset offset)]
              [unification (some-v (abstract-unify (list (abstract-equality unifiable-instance existing-instance)) 0))]
              ; only this is different wrt previous case
              [new-final
               (final
                (map
                 (match-lambda
                   [(abstract-equality var1 var2)
                    (cons
                     (prefix-subscripts (multi-id existing-multi) 'L (offset-vars var1 (- offset) (- offset)))
                     var2)])
                 unification))])
         (cons (list (struct-copy multi existing-multi [final new-final])) fresh-id))]
      [(list
        (list (gen-node (and (? multi?) existing-multi-1) _ _ _ _))
        (list (gen-node (multi placeholder-2 _ _ _ final-2) _ _ _ _)))
       (let* ([placeholder-1 (multi-conjunction existing-multi-1)]
              [subscriptless-instance-1 (remove-multi-subscripts placeholder-1)]
              [subscriptless-instance-2 (remove-multi-subscripts placeholder-2)]
              [offset (apply max (append (assemble-var-indices (λ (_) #t) subscriptless-instance-1) (assemble-var-indices (λ (_) #t) subscriptless-instance-2)))]
              [offset-instance-2 (offset-vars subscriptless-instance-2 offset offset)]
              [subst (some-v (abstract-unify (list (abstract-equality offset-instance-2 subscriptless-instance-1)) 0))]
              [new-final
               (final
                (map
                 (match-lambda
                   [(cons lhs rhs)
                    (cons (prefix-subscripts (multi-id existing-multi-1) 'L (apply-substitution subst (offset-vars (remove-multi-subscripts lhs) offset offset)))
                          rhs)])
                 (final-constraints final-2)))])
         (cons (list (struct-copy multi existing-multi-1 [final new-final])) fresh-id))]))
  (aux partitioning))
(module+ test
  (check-equal?
   (group-sequential-generations
    (list
     (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t))
    1)
   (cons
    (list
     (abstract-atom 'filter (list (g 1) (a 1) (a 2))))
    1))
  (check-equal?
   (group-sequential-generations
    (list (gen-node (multi (list) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l 1 #t) #f #t)) 1)
   (cons
    (list (multi (list) #t (init (list)) (consecutive (list)) (final (list)))) 1))
  (check-equal?
   (group-sequential-generations
    (list
     (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 2) (a 2) (a 3))) 3 (gen 2 1) #f #t))
    1)
   (cons
    (list
     (multi
      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init
       (list
        (cons (g* 1 1 1) (g 1))
        (cons (a* 1 1 1) (a 1))
        (cons (a* 1 1 2) (a 2))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final
       (list
        (cons (g* 1 'L 1) (g 2))
        (cons (a* 1 'L 1) (a 2))
        (cons (a* 1 'L 2) (a 3))))))
    2))
  (check-equal?
   (group-sequential-generations
    (list
     (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen (symsum 'l 1) 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 2) (a 1) (a 3))) 3 (gen (symsum 'l 1) 1) #f #t)
     (gen-node
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
       #f
       (init
        (list (cons (a* 1 1 2) (a 3))))
       (consecutive
        (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
       (final ; doesn't matter
        (list)))
      4 (gen-range 'l 1 1 #f) #f #t))
    2)
   (cons
    (list
     (multi
      (list
       (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
       (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
      #f
      (init
       (list
        (cons (g* 1 1 1) (g 1))
        (cons (a* 1 1 1) (a 1))
        (cons (a* 1 1 2) (a 2))
        (cons (a* 1 1 3) (a 3))))
      (consecutive
       (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
      (final
       (list))))
    2))
  (check-equal?
   (group-sequential-generations
    (list
     (gen-node
      (multi
       (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
       #t
       (init
        (list
         (cons (g* 1 1 1) (g 1))
         (cons (a* 1 1 1) (a 1))
         (cons (a* 1 1 2) (a 2))))
       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
       (final
        (list
         (cons (g* 1 'L 1) (g 3))
         (cons (a* 1 'L 1) (a 3))
         (cons (a* 1 'L 2) (a 4))))) 2 (gen-range 1 'l 1 #t) #f #t)
     (gen-node
      (multi
       (list (abstract-atom* 'filter (list (g* 2 'i 1) (a* 2 'i 1) (a* 2 'i 2))))
       #t
       (init
        (list
         (cons (a* 2 1 1) (a 4))))
       (consecutive (list (cons (a* 2 'i+1 1) (a* 2 'i 2))))
       (final
        (list
         (cons (g* 2 'L 1) (g 4))
         (cons (a* 2 'L 1) (a 5))
         (cons (a* 2 'L 2) (a 6))))) 3 (gen-range (symsum 'l 1) 'n 1 #t) #f #t))
    3)
   (cons
    (list
     (multi
      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init
       (list
        (cons (g* 1 1 1) (g 1))
        (cons (a* 1 1 1) (a 1))
        (cons (a* 1 1 2) (a 2))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final
       (list
        (cons (g* 1 'L 1) (g 4))
        (cons (a* 1 'L 1) (a 5))
        (cons (a* 1 'L 2) (a 6)))))) 3))
  (check-equal?
   (group-sequential-generations
    (list
     (gen-node
      (multi
       (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
       #t
       (init
        (list
         (cons (a* 1 1 1) (a 1))))
       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
       (final
        (list
         (cons (a* 1 'L 2) (a 4)))))
      4 (gen-range 1 'l 1 #t) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (a 4) (a 5))) 5 (gen (symsum 'l 1) 1) #f #t))
    2)
   (cons
    (list
     (multi
      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init
       (list
        (cons (a* 1 1 1) (a 1))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final
       (list
        (cons (g* 1 'L 1) (g 4))
        (cons (a* 1 'L 1) (a 4))
        (cons (a* 1 'L 2) (a 5))))))
    2)))

;; checks whether the current generation in a grouping renames the last generation
;; (from a syntactic perspective) in the potential abstraction
;; if #f is supplied as a keyword argument, a partial match with the last generation is also acceptable
(define (current-is-renaming? g #:full [full? #t])
  (match g
    [(grouping _ #f _ _) #f]
    [(grouping _ lst cur _)
     (let* ([cur-conjunct (map gen-node-conjunct cur)]
            [last-gen
             (match (last lst)
               [(gen-node (? abstract-atom?) _ (gen num id) _ _)
                (map gen-node-conjunct (filter (λ (gn) (equal? (gen-node-range gn) (gen num id))) lst))]
               [(gen-node (multi conj asc? i consec f) _ (? gen-range?) _ _)
                (remove-multi-subscripts conj)])])
       (if full?
           (renames? cur-conjunct last-gen)
           (and
            (<= (length cur-conjunct) (length last-gen))
            (renames? cur-conjunct (take last-gen (length cur-conjunct))))))]))
(module+ test
  (check-true
   (current-is-renaming?
    (grouping
     (list)
     (list (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t))
     (list (gen-node (abstract-atom 'filter (list (g 2) (a 3) (a 4))) 3 (gen 2 1) #f #t))
     1)))
  (check-true
   (current-is-renaming?
    (grouping
     (list)
     (list
      (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t)
      (gen-node (abstract-atom 'filter (list (g 2) (a 3) (a 4))) 3 (gen 2 1) #f #t))
     (list (gen-node (abstract-atom 'filter (list (g 3) (a 5) (a 6))) 4 (gen 3 1) #f #t))
     1)))
  (check-false
   (current-is-renaming?
    (grouping
     (list)
     (list (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t))
     (list (gen-node (abstract-atom 'filter (list (g 2) (abstract-function 'cons (list (g 3) (a 3))) (a 4))) 3 (gen 2 1) #f #t))
     1)))
  (check-true
   (current-is-renaming?
    (grouping
     (list)
     (list (gen-node (multi (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2)))) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l 1 #t) #f #t))
     (list (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 3 (gen (symsum 'l 1) 1) #f #t))
     1)))
  (check-true
   (current-is-renaming?
    (grouping
     (list)
     (list
      (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 1 (gen 2 1) #f #t)
      (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 2 (gen 2 1) #f #t)
      (gen-node (abstract-atom 'append (list (a 1) (a 2) (a 3))) 3 (gen 2 1) #f #t))
     (list (gen-node (abstract-atom 'collect (list (g 4) (a 4))) 4 (gen 1 1) #f #t))
     1)
    #:full #f)))

;; if folding ends and there is still an abstraction and/or current generation...
;; TODO complete!
;; if current generation renames generations in temporary abstraction, move it there - if not, just leave it
;; if temporary abstraction spans multiple generations, merge into a multi
;; finally, append everything
(define (finalize g)
  (grouping-completed g))

; completed: name says it all
; potential: conjunctions of at most two consecutive generations (or generation ranges, or a mixture) which can become a multi
; current-gen: sequential conjuncts with the same (non-dummy) generation which could be joined by the conjunct in node if it has the right value
; node: the list element currently subjected to the foldl function
(define (group-conjuncts node acc)
  (match-let ([(grouping completed potential current-gen next-multi-id) acc])
    (match* (completed potential current-gen node)
      ;; BLOCK: temporary abstraction #f and empty current gen
      [(_ #f (list)
          (gen-node conjunct _ (gen 0 #f) #f _))
       (struct-copy grouping acc [completed (append completed (list conjunct))])]
      [(_ #f (list)
          (gen-node conjunct _ (gen n id) #f #f))
       (struct-copy grouping acc [completed (append completed (list conjunct))])]
      [(_ #f (list)
          (gen-node conjunct _ (gen n id) #f #t))
       (struct-copy grouping acc [current-gen (list node)])]
      [(_ #f (list)
          (gen-node conjunct _ (gen-range _ _ _ _) #f #f))
       (struct-copy grouping acc [completed (append completed (list conjunct))])]
      [(_ #f (list)
          (gen-node conjunct _ (gen-range _ _ _ _) #f #t))
       (struct-copy grouping acc [potential (list node)])]
      ;; BLOCK: temporary abstraction #f, non-empty current gen
      [(_ #f (list-rest _ _) (gen-node conjunct _ (gen 0 #f) #f _))
       (struct-copy grouping acc [completed (append completed (map gen-node-conjunct current-gen) (list conjunct))] [current-gen '()])]
      [(_ #f (list-rest (gen-node _ _ (gen n o) #f _) _) (gen-node conjunct _ (gen n o) #f _))
       (struct-copy grouping acc [current-gen (append current-gen (list node))])]
      [(_ #f (list-rest (gen-node _ _ (gen n o-1) #f _) _) (gen-node conjunct _ (gen m o-2) #f _)) #:when (not (eqv? o-1 o-2))
       (struct-copy grouping acc [completed (append completed (map gen-node-conjunct current-gen))] [current-gen (list node)])]
      [(_ #f (list-rest (gen-node _ _ (gen n-1 o) #f _) _) (gen-node conjunct _ (gen n-2 o) #f _))
       #:when (or (eqv? (gen-add1 n-1) n-2) (eqv? (gen-sub1 n-1) n-2))
       (struct-copy grouping acc
                    [potential current-gen]
                    [current-gen (list node)])]
      [(_ #f (list-rest (gen-node _ _ (gen n-1 o) #f _) _) (gen-node conjunct _ (gen n-2 o) #f _))
       (error "non-consecutive generations with the same origin, should not happen")]
      [(_ #f (list-rest (gen-node _ _ (gen n o-1) #f _) _) (gen-node conjunct _ (gen-range m l o-2 asc) #f _))
       #:when (not (eqv? o-1 o-2))
       (struct-copy grouping acc [completed (append completed (map gen-node-conjunct current-gen))] [potential (list node)] [current-gen (list)])]
      [(_ #f (list-rest (gen-node _ _ (gen n o-1) #f _) _) (gen-node conjunct _ (gen-range n m o-1 asc) #f _))
       (error "non-abstracted conjunction and abstracted conjunction have the same generation, should not happen")]
      ; TODO: from #f / (list aat ...) / n+-1 : m id onwards
      [(_ _ _ _) acc])))

(define (generalize-level lvl)
  (define multis (filter multi? (map gen-node-conjunct lvl)))
  (define multivars
    (apply append (map extract-subscripted-variables multis)))
  (define next-multi-id
    (if (not (null? multivars))
        (add1 (apply max (map abstract-variable*-multi-id multivars)))
        1))
  (finalize
   (foldl
    group-conjuncts
    (grouping (list) #f (list) next-multi-id)
    (sort lvl < #:key gen-node-id))))
(module+ test
  (require rackunit)
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 3 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 1) (a 2) (a 3))) 4 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 4) (a 4))) 5 (gen 2 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 3) (a 4) (a 5))) 6 (gen 2 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 6) (a 6))) 7 (gen 1 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 5) (a 6) (a 7))) 8 (gen 1 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 8) (a 8))) 9 (gen 0 #f) #f #t)
     (gen-node (abstract-atom 'eq (list (a 7) (a 8))) 10 (gen 0 #f) #f #t)))
   (list
    (abstract-atom 'collect (list (g 1) (a 1)))
    (abstract-atom 'collect (list (g 2) (a 2)))
    (abstract-atom 'append (list (a 1) (a 2) (a 3)))
    (multi
     (list
      (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
      (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
     #f
     (init
      (list
       (cons (g* 1 1 1) (g 4))
       (cons (a* 1 1 1) (a 4))
       (cons (a* 1 1 2) (a 3))
       (cons (a* 1 1 3) (a 5))))
     (consecutive
      (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
     (final
      (list
       (cons (g* 1 'L 1) (g 6))
       (cons (a* 1 'L 1) (a 5))
       (cons (a* 1 'L 2) (a 6))
       (cons (a* 1 'L 3) (a 7)))))))
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
     (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 3 (gen 1 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 3) (a 2) (a 3))) 4 (gen 2 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5))) 5 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 6) (a 5) (a 6))) 6 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 7) (a 6) (a 7))) 7 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 8) (a 7) (a 8))) 8 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'sift (list (a 8) (a 9))) 9 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'len (list (a 9) (g 9))) 10 (gen 0 #f) #f #t)))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 2))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive
      (list
       (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 3))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5)))
    (multi
     (list (abstract-atom* 'filter (list (g* 2 'i 1) (a* 2 'i 1) (a* 2 'i 2))))
     #t
     (init
      (list
       (cons (g* 2 1 1) (g 6))
       (cons (a* 2 1 1) (a 5))
       (cons (a* 2 1 2) (a 6))))
     (consecutive
      (list
       (cons (a* 2 'i+1 1) (a* 2 'i 2))))
     (final
      (list
       (cons (g* 2 'L 1) (g 7))
       (cons (a* 2 'L 1) (a 6))
       (cons (a* 2 'L 2) (a 7)))))
    (abstract-atom 'filter (list (g 8) (a 7) (a 8)))
    (abstract-atom 'sift (list (a 8) (a 9)))
    (abstract-atom 'len (list (a 9) (g 9)))))
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
     (gen-node
      (multi
       (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
       #t
       (init
        (list
         (cons (g* 1 1 1) (g 2))
         (cons (a* 1 1 1) (a 1))
         (cons (a* 1 1 2) (a 2))))
       (consecutive
        (list
         (cons (a* 1 'i+1 1) (a* 1 'i 2))))
       (final
        (list
         (cons (g* 1 'L 1) (g 3))
         (cons (a* 1 'L 1) (a 2))
         (cons (a* 1 'L 2) (a 3)))))
      3
      (gen-range 1 2 1 #t)
      #f
      #t)
     (gen-node (abstract-atom 'filter (list (g 4) (a 3) (a 4))) 4 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'filter (list (abstract-function 'cons (list (g 5) (a 4) (a 5))))) 5 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 6) (a 5) (a 6))) 6 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'sift (list (a 6) (a 7))) 7 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'len (list (a 7) (g 7))) 8 (gen 0 #f) #f #t)))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 2))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive
      (list
       (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 4))
       (cons (a* 1 'L 1) (a 3))
       (cons (a* 1 'L 2) (a 4)))))
    (abstract-atom 'filter (list (g 6) (a 5) (a 6)))
    (abstract-atom 'sift (list (a 6) (a 7)))
    (abstract-atom 'len (list (a 7) (g 7)))))
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 3 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 1) (a 2) (a 3))) 4 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 4) (a 4))) 5 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 3) (a 4) (a 5))) 6 (gen 4 1) #f #t)
     (gen-node
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
       #f
       (init
        (list
         (cons (a* 1 1 2) (a 5))))
       (consecutive
        (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
       (final
        (list
         (cons (g* 1 'L 1) (g 7))
         (cons (a* 1 'L 1) (a 8))
         (cons (a* 1 'L 2) (a 9))
         (cons (a* 1 'L 3) (a 10)))))
      7
      (gen-range 3 1 1 #f)
      #f
      #t)
     (gen-node (abstract-atom 'collect (list (g 11) (a 11))) 8 (gen 0 #f) #f #t)
     (gen-node (abstract-atom 'eq (list (a 10) (a 11))) 9 (gen 0 #f) #f #t)))
   (list
    (abstract-atom 'collect (list (g 1) (a 1)))
    (abstract-atom 'collect (list (g 2) (a 2)))
    (abstract-atom 'append (list (a 1) (a 2) (a 3)))
    (multi
     (list
      (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
      (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
     #f
     (init
      (list
       (cons (g* 1 1 1) (g 4))
       (cons (a* 1 1 1) (a 4))
       (cons (a* 1 1 2) (a 3))
       (cons (a* 1 1 3) (a 5))))
     (consecutive
      (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
     (final
      (list
       (cons (g* 1 'L 1) (g 7))
       (cons (a* 1 'L 1) (a 8))
       (cons (a* 1 'L 2) (a 9))
       (cons (a* 1 'L 3) (a 10)))))
    (abstract-atom 'collect (list (g 11) (a 11)))
    (abstract-atom 'eq (list (a 10) (a 11)))))
  (let ([node1 (gen-node (abstract-atom 'allsafe (list (g 1) (g 2) (g 3) (abstract-function 'cons (list (g 4) (a 1))))) 2 (gen 1 1) #f #f)]
        [node2
         (gen-node
          (multi
           (list
            (abstract-atom* 'allsafe (list (g* 1 'i 1) (g* 1 'i 2) (g* 1 'i 3) (abstract-function* 'cons (list (g* 1 'i 4) (a* 1 'i 1))))))
           #t
           (init (list (cons (g* 1 1 4) (g 4))))
           (consecutive
            (list
             (cons (g* 1 'i+1 4) (g* 1 'i 4))
             (cons (a* 1 'i+1 1) (a* 1 'i 1))))
           (final (list)))
          3
          (gen-range 2 5 1 #t)
          #f
          #f)])
    (check-equal?
     (generalize-level (list node1 node2))
     (list (gen-node-conjunct node1) (gen-node-conjunct node2)))))
(provide
 (proc-doc/names
  generalize-level
  (-> (listof gen-node?) (listof abstract-conjunct?))
  (lvl)
  @{Attempts to generalize the conjunction represented by @racket[lvl].}))