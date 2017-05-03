#lang at-exp racket
(require
  racket/struct
  scribble/srcdoc
  (only-in "abstract-analysis.rkt" tree-label? tree-label-conjunction generalization?)
  "abstract-multi-domain.rkt"
  (only-in "abstract-domain-ordering.rkt" renames?)
  (only-in "abstraction-inspection-utils.rkt" assemble-var-indices extract-subscripted-variables extract-variables)
  (only-in "abstract-renaming.rkt" offset-vars)
  (only-in "abstract-unify.rkt" abstract-unify)
  (only-in "abstract-substitution.rkt" abstract-equality apply-substitution)
  (only-in "data-utils.rkt" some-v)
  "gen-graph-structs.rkt"
  (only-in "generational-graph.rkt" gen-number< gen-add1 gen-sub1 generational-graph-skeleton annotate-general! candidate-targets rdag-level)
  (only-in "multi-folding-unfolding.rkt" remove-multi-subscripts)
  (only-in "multi-unfolding.rkt" unfold-multi-many unfold-multi-many-bounded unfold-multi-many-right)
  racket/logging)
(require (for-doc scribble/manual))

(define gen-range-descending? (compose not gen-range-ascending?))

;; gets the origin from either a generation or a generation range
(define (gen-thing-origin gen-thing)
  (match gen-thing
    [(? gen-range?) (gen-range-origin gen-thing)]
    [(? gen?) (gen-origin gen-thing)]))

;; accumulator struct used when folding over a level of identified gen nodes
(struct grouping (completed potential current-gen next-multi-id dummy-id lvl)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'grouping)
      (λ (obj) (list (grouping-completed obj)
                     (grouping-potential obj)
                     (grouping-current-gen obj)
                     (grouping-next-multi-id obj)
                     (grouping-dummy-id obj)
                     (grouping-lvl obj)))))])

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

(define (generalized-ranges pre post)
  (define post-ids (map gen-node-id post))
  (define (aux id acc)
    (match-let ([(cons idx rngs) acc])
      (if (member id post-ids)
          (cons (add1 idx) rngs)
          (match rngs
            [(list-rest (index-range hs he) t)
             #:when (eqv? idx he)
             (cons (add1 idx) (cons (index-range hs (add1 he)) t))]
            [_ (cons (add1 idx) (cons (index-range idx (add1 idx)) rngs))]))))
  (reverse (cdr (foldl aux (cons 0 '()) (map gen-node-id pre)))))
(module+ test
  (check-equal?
   (generalized-ranges
    (list
     (gen-node (abstract-atom 'integers '()) 2 (gen 0 #f) #f #t)
     (gen-node (abstract-atom 'filter '()) 3 (gen 1 1) #f #t)
     (gen-node (abstract-atom 'filter '()) 4 (gen 2 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5))) 5 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'filter '()) 6 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'filter '()) 7 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'filter '()) 8 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'sift '()) 9 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'len '()) 10 (gen 0 #f) #f #t))
    (list
     (gen-node (abstract-atom 'integers '()) 2 (gen 0 #f) #f #t)
     (gen-node (multi '() #t (init '()) (consecutive '()) (final '())) 11 (gen-range 1 2 1 #t) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5))) 5 (gen 3 1) #f #t)
     (gen-node (multi '() #t (init '()) (consecutive '()) (final '())) 12 (gen-range 4 5 1 #t) #f #t)
     (gen-node (abstract-atom 'filter '()) 8 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'sift '()) 9 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'len '()) 10 (gen 0 #f) #f #t)))
   (list (index-range 1 3) (index-range 4 6))))

;; this turns the "growing abstraction" into a single multi abstraction
;; that is, a single generation stays a single generation
;; a single multi stays a single multi
;; three consecutive generations become a single multi (two would introduce undesired aliasing when the multi is unfolded to a single conjunction)
;; a generation and a multi (in either order) become a single multi
;; two multis become a single multi
;; note that this should never fail
;; if a conjunct is in the growing abstraction, it belongs there and it respects the pattern
;; also returns the next valid fresh id for a multi (which may be the input id if it was not needed)
(define (group-sequential-generations potential fresh-id dummy-id lvl)
  (define partitioning (group-by gen-node-range potential))
  (define (aux partitioning)
    (match partitioning
      [(list
        (and (list-rest (gen-node (? abstract-atom?) _ (gen genn-1 id) _ _) first-rest) lvl-1)
        (and (list-rest (gen-node (? abstract-atom?) _ (gen genn-2 id) _ _) second-rest) lvl-2))
       (cons
        (let* ([gen-1 (map gen-node-conjunct lvl-1)]
               [gen-2 (map gen-node-conjunct lvl-2)]
               [offset (apply max (assemble-var-indices (λ (_) #t) (append gen-1 gen-2)))]
               [offset-gen-2 (offset-vars gen-2 offset offset)]
               [subst-1 (some-v (abstract-unify (map abstract-equality gen-1 offset-gen-2) 0))]
               [shared (filter (match-lambda [(abstract-equality v1 v2) (and (member (offset-vars v2 (- offset) (- offset)) (extract-variables gen-1)) (member (offset-vars v2 (- offset) (- offset)) (extract-variables gen-2)))]) subst-1)]
               [new-consecutive (map (match-lambda [(abstract-equality (a idx1) (a idx2)) (cons (a* fresh-id 'i+1 idx1) (a* fresh-id 'i (- idx2 offset)))] [(abstract-equality (g idx1) (g idx2)) (cons (g* fresh-id 'i+1 idx1) (g* fresh-id 'i (- idx2 offset)))]) shared)]
               [gen-vars (extract-variables (map gen-node-conjunct (append lvl-1 lvl-2)))]
               [context (foldr (λ (el acc) (if (or (member el lvl-1) (member el lvl-2)) acc (cons el acc))) '() lvl)]
               [context-vars (extract-variables (map gen-node-conjunct context))]
               [new-final (foldr (λ (el acc)
                                   (match el
                                     [(abstract-equality (a idx1) (a idx2)) (if (member (a (- idx2 offset)) context-vars) (cons (cons (a* fresh-id 'L idx1) (a (- idx2 offset))) acc) acc)]
                                     [(abstract-equality (g idx1) (g idx2)) (if (member (g (- idx2 offset)) context-vars) (cons (cons (g* fresh-id 'L idx1) (g (- idx2 offset))) acc) acc)])) '() subst-1)])
          (list
           (gen-node
            (multi
             (prefix-subscripts fresh-id 'i (map gen-node-conjunct lvl-1))
             (gen-number< genn-1 genn-2)
             (init (map (λ (v) (cons (prefix-subscripts fresh-id 1 v) v)) (filter (λ (v) (member v context-vars)) (extract-variables (map gen-node-conjunct lvl-1)))))
             (consecutive new-consecutive)
             (final new-final))
            dummy-id
            (gen-range genn-1 genn-2 id (gen-number< genn-1 genn-2))
            #f
            #t)))
        (add1 fresh-id))]
      [(list
        (and (list-rest (gen-node (? abstract-atom?) _ (gen n id) _ _) first-rest) single-gen)
        (list (gen-node (and (? multi?) existing-multi) _ (gen-range m o id asc?) _ _)))
       (let* ([existing-instance (map gen-node-conjunct single-gen)]
              [subscriptless-instance (remove-multi-subscripts (multi-conjunction existing-multi))]
              [offset (apply max (assemble-var-indices (λ (_) #t) (append subscriptless-instance (map gen-node-conjunct single-gen))))]
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
         (cons (list (gen-node (struct-copy multi existing-multi [init new-init]) dummy-id (gen-range n o id asc?) #f #t)) fresh-id))]     
      [(list
        (list (gen-node (and (? multi?) existing-multi) _ (gen-range m o id asc?) _ _))
        (and (list-rest (gen-node (? abstract-atom?) _ (gen n id) _ _) first-rest) single-gen))
       (let* ([existing-instance (map gen-node-conjunct single-gen)]
              [subscriptless-instance (remove-multi-subscripts (multi-conjunction existing-multi))]
              [offset (apply max (assemble-var-indices (λ (_) #t) (append subscriptless-instance (map gen-node-conjunct single-gen))))]
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
         (cons (list (gen-node (struct-copy multi existing-multi [final new-final]) dummy-id (gen-range m n id asc?) #f #t)) fresh-id))]      
      [(list
        (list (gen-node (and (? multi?) existing-multi-1) _ (gen-range n m id asc?) _ _))
        (list (gen-node (multi placeholder-2 _ _ _ final-2) _ (gen-range o p id asc?) _ _)))
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
         (cons (list (gen-node (struct-copy multi existing-multi-1 [final new-final]) dummy-id (gen-range n p id asc?) #f #t)) fresh-id))]
      [(or (list _) (list _ _))
       (cons potential fresh-id)]))
  (aux partitioning))
;(module+ test
;  (check-equal?
;   (group-sequential-generations
;    (list
;     (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t))
;    1)
;   (cons
;    (list
;     (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t))
;    1))
;  (check-equal?
;   (group-sequential-generations
;    (list (gen-node (multi (list) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l 1 #t) #f #t)) 1)
;   (cons
;    (list (multi (list) #t (init (list)) (consecutive (list)) (final (list)))) 1))
;  (check-equal?
;   (group-sequential-generations
;    (list
;     (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t)
;     (gen-node (abstract-atom 'filter (list (g 2) (a 2) (a 3))) 3 (gen 2 1) #f #t))
;    1)
;   (cons
;    (list
;     (multi
;      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
;      #t
;      (init
;       (list
;        (cons (g* 1 1 1) (g 1))
;        (cons (a* 1 1 1) (a 1))
;        (cons (a* 1 1 2) (a 2))))
;      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
;      (final
;       (list
;        (cons (g* 1 'L 1) (g 2))
;        (cons (a* 1 'L 1) (a 2))
;        (cons (a* 1 'L 2) (a 3))))))
;    2))
;  (check-equal?
;   (group-sequential-generations
;    (list
;     (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen (symsum 'l 1) 1) #f #t)
;     (gen-node (abstract-atom 'append (list (a 2) (a 1) (a 3))) 3 (gen (symsum 'l 1) 1) #f #t)
;     (gen-node
;      (multi
;       (list
;        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
;        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
;       #f
;       (init
;        (list (cons (a* 1 1 2) (a 3))))
;       (consecutive
;        (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
;       (final ; doesn't matter
;        (list)))
;      4 (gen-range 'l 1 1 #f) #f #t))
;    2)
;   (cons
;    (list
;     (multi
;      (list
;       (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
;       (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
;      #f
;      (init
;       (list
;        (cons (g* 1 1 1) (g 1))
;        (cons (a* 1 1 1) (a 1))
;        (cons (a* 1 1 2) (a 2))
;        (cons (a* 1 1 3) (a 3))))
;      (consecutive
;       (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
;      (final
;       (list))))
;    2))
;  (check-equal?
;   (group-sequential-generations
;    (list
;     (gen-node
;      (multi
;       (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
;       #t
;       (init
;        (list
;         (cons (g* 1 1 1) (g 1))
;         (cons (a* 1 1 1) (a 1))
;         (cons (a* 1 1 2) (a 2))))
;       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
;       (final
;        (list
;         (cons (g* 1 'L 1) (g 3))
;         (cons (a* 1 'L 1) (a 3))
;         (cons (a* 1 'L 2) (a 4))))) 2 (gen-range 1 'l 1 #t) #f #t)
;     (gen-node
;      (multi
;       (list (abstract-atom* 'filter (list (g* 2 'i 1) (a* 2 'i 1) (a* 2 'i 2))))
;       #t
;       (init
;        (list
;         (cons (a* 2 1 1) (a 4))))
;       (consecutive (list (cons (a* 2 'i+1 1) (a* 2 'i 2))))
;       (final
;        (list
;         (cons (g* 2 'L 1) (g 4))
;         (cons (a* 2 'L 1) (a 5))
;         (cons (a* 2 'L 2) (a 6))))) 3 (gen-range (symsum 'l 1) 'n 1 #t) #f #t))
;    3)
;   (cons
;    (list
;     (multi
;      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
;      #t
;      (init
;       (list
;        (cons (g* 1 1 1) (g 1))
;        (cons (a* 1 1 1) (a 1))
;        (cons (a* 1 1 2) (a 2))))
;      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
;      (final
;       (list
;        (cons (g* 1 'L 1) (g 4))
;        (cons (a* 1 'L 1) (a 5))
;        (cons (a* 1 'L 2) (a 6)))))) 3))
;  (check-equal?
;   (group-sequential-generations
;    (list
;     (gen-node
;      (multi
;       (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
;       #t
;       (init
;        (list
;         (cons (a* 1 1 1) (a 1))))
;       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
;       (final
;        (list
;         (cons (a* 1 'L 2) (a 4)))))
;      4 (gen-range 1 'l 1 #t) #f #t)
;     (gen-node (abstract-atom 'filter (list (g 4) (a 4) (a 5))) 5 (gen (symsum 'l 1) 1) #f #t))
;    2)
;   (cons
;    (list
;     (multi
;      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
;      #t
;      (init
;       (list
;        (cons (a* 1 1 1) (a 1))))
;      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
;      (final
;       (list
;        (cons (g* 1 'L 1) (g 4))
;        (cons (a* 1 'L 1) (a 4))
;        (cons (a* 1 'L 2) (a 5))))))
;    2)))

;; checks whether the current generation in a grouping renames the last generation
;; (from a syntactic perspective) in the potential abstraction
;; if #f is supplied as a keyword argument, a partial match with the last generation is also acceptable
(define (current-is-renaming? g #:full [full? #t])
  (match g
    [(grouping _ #f _ _ _ _) #f]
    [(grouping _ lst cur _ _ _)
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
     1
     1
     (list))))
  (check-true
   (current-is-renaming?
    (grouping
     (list)
     (list
      (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t)
      (gen-node (abstract-atom 'filter (list (g 2) (a 3) (a 4))) 3 (gen 2 1) #f #t))
     (list (gen-node (abstract-atom 'filter (list (g 3) (a 5) (a 6))) 4 (gen 3 1) #f #t))
     1
     1
     (list))))
  (check-false
   (current-is-renaming?
    (grouping
     (list)
     (list (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 2 (gen 1 1) #f #t))
     (list (gen-node (abstract-atom 'filter (list (g 2) (abstract-function 'cons (list (g 3) (a 3))) (a 4))) 3 (gen 2 1) #f #t))
     1
     1
     (list))))
  (check-true
   (current-is-renaming?
    (grouping
     (list)
     (list (gen-node (multi (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2)))) #t (init (list)) (consecutive (list)) (final (list))) 2 (gen-range 1 'l 1 #t) #f #t))
     (list (gen-node (abstract-atom 'filter (list (g 1) (a 1) (a 2))) 3 (gen (symsum 'l 1) 1) #f #t))
     1
     1
     (list))))
  (check-true
   (current-is-renaming?
    (grouping
     (list)
     (list
      (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 1 (gen 2 1) #f #t)
      (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 2 (gen 2 1) #f #t)
      (gen-node (abstract-atom 'append (list (a 1) (a 2) (a 3))) 3 (gen 2 1) #f #t))
     (list (gen-node (abstract-atom 'collect (list (g 4) (a 4))) 4 (gen 1 1) #f #t))
     1
     1
     (list))
    #:full #f)))

;; folding could keep some conjuncts in current-gen or potential
;; these should be considered completed, as well
(define (finalize g)
  (match g
    [(grouping co #f cu _ _ _)
     (append co cu)]
    [(grouping co po cu _ _ _)
     (append co po cu)]))

;; TODO: group-conjuncts is an absolute monstrosity, find a way to eliminate boilerplate and improve readability
; completed: name says it all
; potential: conjunctions of at most two consecutive generations (or generation ranges, or a mixture) which can become a multi
; current-gen: sequential conjuncts with the same (non-dummy) generation which could be joined by the conjunct in node if it has the right value
; node: the list element currently subjected to the foldl function
;; FIXME: using pattern matching was a mistake - this is unmaintainable. could try to specify a set of rules (e.g. if next gen is (0 #f) or is not foldable, potential always becomes #f and current-gen becomes empty; grouping requires (a representation of) three or more generations, etc.)
(define (group-conjuncts node acc)
  (match-let ([(grouping completed potential current-gen next-multi-id dummy-id lvl) acc])
    (match* (potential current-gen node)
      ;; BLOCK: temporary abstraction #f and empty current gen
      [(#f (list)
           (gen-node conjunct _ (gen 0 #f) #f _))
       (struct-copy grouping acc [completed (append completed (list node))])]
      [(#f (list)
           (gen-node conjunct _ (gen n id) #f #f))
       (struct-copy grouping acc [completed (append completed (list node))])]
      [(#f (list)
           (gen-node conjunct _ (gen n id) #f #t))
       (struct-copy grouping acc [current-gen (list node)])]
      [(#f (list)
           (gen-node conjunct _ (gen-range _ _ _ _) #f #f))
       (struct-copy grouping acc [completed (append completed (list node))])]
      [(#f (list)
           (gen-node conjunct _ (gen-range _ _ _ _) #f #t))
       (struct-copy grouping acc [potential (list node)])]
      ;; BLOCK: temporary abstraction #f, non-empty current gen
      [(#f (list-rest _ _) (gen-node conjunct _ (gen 0 #f) #f _))
       (struct-copy grouping acc [completed (append completed current-gen (list node))] [current-gen '()])]
      [(#f (list-rest _ _) (gen-node conjunct _ (gen _ _) #f #f))
       (struct-copy grouping acc [completed (append completed current-gen (list node))] [current-gen '()])]
      [(#f (list-rest (gen-node _ _ (gen n o) #f _) _) (gen-node conjunct _ (gen n o) #f #t))
       (struct-copy grouping acc [current-gen (append current-gen (list node))])]
      [(#f (list-rest (gen-node _ _ (gen n-1 o) #f _) _) (gen-node conjunct _ (gen n-2 o) #f #t))
       #:when (or (equal? (gen-add1 n-1) n-2) (equal? (gen-sub1 n-1) n-2))
       (struct-copy grouping acc
                    [potential current-gen]
                    [current-gen (list node)])]
      [(#f (list-rest (gen-node _ _ (gen n o-1) #f _) _) (gen-node conjunct _ (gen m o-2) #f _)) #:when (not (eqv? o-1 o-2))
       (struct-copy grouping acc [completed (append completed current-gen)] [current-gen (list node)])]
      [(#f (list-rest (gen-node _ _ (gen n o) #f _) _) (gen-node conjunct _ (gen-range m p o asc) #f #f))
       (struct-copy grouping acc [completed (append completed current-gen (list node))] [current-gen (list)])]
      [(#f (list-rest (gen-node _ _ (gen n o) #f _) _) (gen-node conjunct _ (gen-range m p o asc) #f #t))
       #:when (and
               (or (and asc (equal? (gen-add1 n) m)) (and (not asc) (equal? (gen-sub1 n) m)))
               (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct))])
                 (renames?
                  (append (map gen-node-conjunct current-gen) (list conjunct))
                  (unfold-multi-many conjunct offset offset))))
       (struct-copy grouping acc [potential (car (group-sequential-generations (append current-gen (list node)) next-multi-id dummy-id lvl))] [current-gen (list)] [dummy-id (add1 dummy-id)])]
      [(#f (list-rest (gen-node _ _ (gen n o-1) #f _) _) (gen-node conjunct _ (gen-range m p o-2 asc) #f #t))
       #:when (or (and asc (equal? (gen-add1 n) m) (eqv? o-1 o-2)) (and (not asc) (equal? (gen-sub1 n) m) (eqv? o-1 o-2)) (not (eqv? o-1 o-2)))
       (struct-copy grouping acc [completed (append completed current-gen)] [potential (list node)] [current-gen (list)])]
      ;; BLOCK: temporary abstraction consisting of a list of atoms, empty current gen
      [((list-rest (gen-node _ _ (gen _ _) _ _) _) (list) (gen-node conjunct _ (gen 0 #f) #f _))
       (struct-copy grouping acc [completed (append completed potential (list node))] [potential #f] [current-gen (list)])]
      [((list-rest (gen-node _ _ (gen _ _) _ _) _) (list) (gen-node conjunct _ (gen _ _) #f #f))
       (struct-copy grouping acc [completed (append completed potential (list node))] [potential #f] [current-gen (list)])]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list) (gen-node conjunct _ (gen m id) #f #t))
       (struct-copy grouping acc [current-gen (list node)])]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list) (gen-node conjunct _ (gen m id-2) #f #t))
       #:when (not (eqv? id-1 id-2))
       (struct-copy grouping acc [completed (append completed potential)] [current-gen (list node)])]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list) (gen-node conjunct _ (gen-range _ _ _ _) #f #f))
       (struct-copy grouping acc [completed (append completed potential current-gen)] [potential #f])]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list) (gen-node conjunct _ (gen-range m o id asc?) #f #t))
       #:when
       (and
        (or (and asc? (equal? (gen-add1 n) m)) (and (not asc?) (equal? (gen-sub1 n) m)))
        (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct))])
          (renames?
           (append (map gen-node-conjunct potential) (list conjunct))
           (unfold-multi-many conjunct offset offset))))
       (struct-copy grouping acc [potential (car (group-sequential-generations (append potential node) next-multi-id dummy-id lvl))] [dummy-id (add1 dummy-id)])]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list) (gen-node conjunct _ (gen-range m o id asc?) #f #t))
       (struct-copy grouping acc [completed (append completed potential)] [potential (list node)])]
      ;; BLOCK: temporary abstraction consisting of a list of atoms, nonempty current gen
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list-rest (gen-node _ _ (gen m id) #f _) _) (gen-node conjunct _ (gen 0 #f) #f #t))
       #:when (and (or (equal? (gen-add1 n) m) (equal? (gen-sub1 n) m)) (renames? (map gen-node-conjunct potential) (map gen-node-conjunct current-gen)))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp) (list node))] [potential #f] [current-gen (list)] [next-multi-id (cdr grp)] [dummy-id (add1 dummy-id)]))]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list-rest (gen-node _ _ (gen m id) #f _) _) (gen-node conjunct _ (gen 0 #f) #f #t))
       #:when (and (or (equal? (gen-add1 n) m) (equal? (gen-sub1 n) m)))
       (struct-copy grouping acc [completed (append completed potential current-gen (list node))] [potential #f] [current-gen (list)])]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list-rest (gen-node _ _ (gen m id) #f _) _) (gen-node conjunct _ (gen _ _) #f #f))
       #:when (renames? (map gen-node-conjunct potential) (map gen-node-conjunct current-gen))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp) (list node))] [potential #f] [current-gen (list)] [next-multi-id (cdr grp)] [dummy-id (add1 dummy-id)]))]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list-rest (gen-node _ _ (gen m id) #f _) _) (gen-node conjunct _ (gen _ _) #f #f))
       (struct-copy grouping acc [completed (append completed potential current-gen (list node))] [potential #f] [current-gen (list)])]
      [((list-rest (gen-node _ _ (gen n id) _ _) _) (list-rest (gen-node _ _ (gen m id) #f _) _) (gen-node conjunct _ (gen m id) #f #t))
       (struct-copy grouping acc [current-gen (append current-gen (list node))])]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen o id-2) #f #t))
       #:when (and (not (eqv? id-1 id-2)) (renames? (map gen-node-conjunct potential) (map gen-node-conjunct current-gen)))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp))] [potential #f] [current-gen (list node)] [next-multi-id (cdr grp)] [dummy-id (add1 dummy-id)]))]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen o id-2) #f #t))
       #:when (and (not (eqv? id-1 id-2)))
       (struct-copy grouping acc [completed (append completed potential current-gen)] [potential #f] [current-gen (list node)])]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen o id-1) #f #t))
       #:when (and (or (equal? (gen-add1 m) o) (equal? (gen-sub1 m) o)) (renames? (map gen-node-conjunct potential) (map gen-node-conjunct current-gen)))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [potential (car grp)] [current-gen (list node)] [next-multi-id (cdr grp)] [dummy-id (add1 dummy-id)]))]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen o id-1) #f #t))
       #:when (and (or (equal? (gen-add1 m) o) (equal? (gen-sub1 m) o)))
       (struct-copy grouping acc [completed (append completed potential)] [potential current-gen] [current-gen (list node)])]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen-range o p id-2 asc?) #f #f))
       #:when (renames? (map gen-node-conjunct potential) (map gen-node-conjunct current-gen))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp) (list node))] [potential #f] [current-gen (list)] [next-multi-id (cdr grp)] [dummy-id (add1 dummy-id)]))]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen-range o p id-2 asc?) #f #f))
       (struct-copy grouping acc [completed (append completed potential current-gen (list node))] [potential #f] [current-gen (list)])]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen-range o p id-1 asc?) #f #t))
       #:when (and (or (and asc? (equal? o (gen-add1 m))) (and (not asc?) (equal? o (gen-sub1 m))))
                   (renames? (map gen-node-conjunct potential) (map gen-node-conjunct current-gen))
                   (renames? (append (map gen-node-conjunct current-gen) (list conjunct)) (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct))]) (unfold-multi-many conjunct offset offset))))
       (let* ([grp-1 (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)]
              [grp-2 (group-sequential-generations (append (car grp-1) (list conjunct)) (add1 next-multi-id) (add1 dummy-id) lvl)])
         (struct-copy grouping acc [potential (car grp-2)] [current-gen (list)] [next-multi-id (cdr grp-2)] [dummy-id (+ dummy-id 2)]))]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen-range o p id-1 asc?) #f #t))
       #:when (and (or (and asc? (equal? o (gen-add1 m))) (and (not asc?) (equal? o (gen-sub1 m))))
                   (renames? (append (map gen-node-conjunct current-gen) (list conjunct)) (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct))]) (unfold-multi-many conjunct offset offset))))
       (let* ([grp (group-sequential-generations (append current-gen (list node)) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed potential)] [potential (car grp)] [current-gen (list)] [next-multi-id (cdr grp)] [dummy-id (add1 dummy-id)]))]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen-range o p id-1 asc?) #f #t))
       #:when (and (or (and asc? (equal? o (gen-add1 m))) (and (not asc?) (equal? o (gen-sub1 m))))
                   (renames? (map gen-node-conjunct potential) (map gen-node-conjunct current-gen)))
       (let* ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp))] [potential (list node)] [current-gen (list)] [next-multi-id (cdr grp)] [dummy-id (add1 dummy-id)]))]
      [((list-rest (gen-node _ _ (gen n id-1) _ _) _) (list-rest (gen-node _ _ (gen m id-1) #f _) _) (gen-node conjunct _ (gen-range o p id-1 asc?) #f #t))
       (struct-copy grouping acc [completed (append completed potential current-gen)] [potential (list node)] [current-gen (list)])]
      ;; BLOCK: temporary abstraction consisting of a multi, current gen empty
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list) (gen-node conjunct-2 _ (gen 0 #f) #f #t))
       (struct-copy grouping acc [completed (append completed potential (list node))] [potential #f])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list) (gen-node conjunct-2 _ (gen _ _) #f #f))
       (struct-copy grouping acc [completed (append completed potential (list node))] [potential #f])]
      [((list (gen-node _ _ (gen-range n m id asc?) _ _)) (list) (gen-node conjunct _ (gen o id) #f #t))
       #:when (or (and asc? (equal? o (gen-add1 m))) (and (not asc?) (equal? o (gen-sub1 m))))
       (struct-copy grouping acc [current-gen (append current-gen (list node))])]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list) (gen-node conjunct-2 _ (gen _ id-2) #f #t))
       #:when (not (eqv? id-1 id-2))
       (struct-copy grouping acc [completed (append completed potential)] [potential #f] [current-gen (list node)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list) (gen-node conjunct-2 _ (gen-range _ _ _ _) #f #f))
       (struct-copy grouping acc [completed (append completed potential (list node))] [potential #f])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list) (gen-node conjunct-2 _ (gen-range o p id asc?) #f #t))
       #:when (and
               (or (and asc? (equal? o (gen-add1 m))) (and (not asc?) (equal? o (gen-sub1 m))))
               (renames? (append (drop (unfold-multi-many-right conjunct-1) 1) (list conjunct-2)) (unfold-multi-many conjunct-2)))
       (struct-copy grouping acc [potential (car (group-sequential-generations (append potential (list node)) next-multi-id dummy-id lvl))] [dummy-id (add1 dummy-id)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list) (gen-node conjunct-2 _ (gen-range o p id asc?) #f #t))
       #:when (or (and asc? (equal? o (gen-add1 m))) (and (not asc?) (equal? o (gen-sub1 m))))
       (struct-copy grouping acc [completed (append completed potential)] [potential (list node)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list) (gen-node conjunct-2 _ (gen-range _ _ id-2 _) #f #t))
       #:when (not (eqv? id-1 id-2))
       (struct-copy grouping acc [completed (append completed potential)] [potential (list node)])]
      ;; BLOCK: temporary abstraction consisting of a multi, current gen not empty
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list-rest (gen-node _ _ (gen o id) #f _) _) (gen-node conjunct-2 _ (gen 0 #f) #f #t))
       #:when (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-1))])
                (renames?
                 (unfold-multi-many-right conjunct-1 offset offset)
                 (append (map gen-node-conjunct potential) (map gen-node-conjunct current-gen))))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp) (list node))] [current-gen (list)] [potential #f] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node _ _ (gen-range n m id asc?) _ _)) (list-rest (gen-node _ _ (gen o id) #f _) _) (gen-node conjunct _ (gen 0 #f) #f #t))
       (struct-copy grouping acc [completed (append completed potential current-gen (list node))] [current-gen (list)] [potential #f])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list-rest (gen-node _ _ (gen o id) #f _) _) (gen-node conjunct-2 _ (gen _ _) #f #f))
       #:when (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-1))])
                (renames?
                 (unfold-multi-many-right conjunct-1 offset offset)
                 (append (map gen-node-conjunct potential) (map gen-node-conjunct current-gen))))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp) (list node))] [current-gen (list)] [potential #f] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list-rest (gen-node _ _ (gen o id) #f _) _) (gen-node conjunct-2 _ (gen _ _) #f #f))
       (struct-copy grouping acc [completed (append completed potential current-gen (list node))] [current-gen (list)] [potential #f])]

      
      [((list (gen-node _ _ (gen-range n m id asc?) _ _)) (list-rest (gen-node _ _ (gen o id) #f _) _) (gen-node conjunct _ (gen o id) #f #t))
       (struct-copy grouping acc [current-gen (append current-gen (list node))])]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list-rest (gen-node _ _ (gen o id-1) #f _) _) (gen-node conjunct-2 _ (gen p id-2) #f #t))
       #:when (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-1))])
                (and (equal? id-1 id-2)
                     (if asc? (equal? p (gen-add1 o)) (equal? p (gen-sub1 o)))
                     (renames? (unfold-multi-many-right conjunct-1 offset offset) (cons conjunct-1 (map gen-node-conjunct current-gen)))))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [potential (car grp)] [current-gen (list node)] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list-rest (gen-node _ _ (gen o id-1) #f _) _) (gen-node conjunct-2 _ (gen p id-2) #f #t))
       #:when (and (equal? id-1 id-2)
                   (if asc? (equal? p (gen-add1 o)) (equal? p (gen-sub1 o))))
       (struct-copy grouping acc [completed (append completed potential)] [potential current-gen] [current-gen (list node)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list-rest (gen-node _ _ (gen o id-1) #f _) _) (gen-node conjunct-2 _ (gen p id-2) #f #t))
       #:when (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-1))])
                (and (not (equal? id-1 id-2))
                     (renames? (unfold-multi-many-right conjunct-1 offset offset) (cons conjunct-1 (map gen-node-conjunct current-gen)))))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp))] [potential #f] [current-gen (list node)] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list-rest (gen-node _ _ (gen o id-1) #f _) _) (gen-node conjunct-2 _ (gen p id-2) #f #t))
       #:when (not (equal? id-1 id-2))
       (struct-copy grouping acc [completed (append completed potential current-gen)] [potential #f] [current-gen (list node)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list-rest (gen-node _ _ (gen o id) #f _) _) (gen-node conjunct-2 _ (gen-range _ _ _ _) #f #f))
       #:when (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-1))])
                (renames?
                 (unfold-multi-many-right conjunct-1 offset offset)
                 (append (map gen-node-conjunct potential) (map gen-node-conjunct current-gen))))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp) (list node))] [potential #f] [current-gen (list)] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _)) (list-rest (gen-node _ _ (gen o id) #f _) _) (gen-node conjunct-2 _ (gen-range _ _ _ _) #f #f))
       (struct-copy grouping acc [completed (append completed potential current-gen (list node))] [potential #f] [current-gen (list)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _))
        (list-rest (gen-node _ _ (gen o id) #f _) _)
        (gen-node conjunct-2 _ (gen-range p q id asc?) #f #t))
       #:when (and
               (or (and asc? (equal? p (gen-add1 o)))
                   (and (not asc?) (equal? p (gen-sub1 o))))
               (let* ([offset (apply max (assemble-var-indices (λ (_) #t) (cons conjunct-1 (cons conjunct-2 (map gen-node-conjunct current-gen)))))]
                      [unf-left (unfold-multi-many conjunct-2 offset offset)]
                      [lock-left (renames? (append (map gen-node-conjunct current-gen) (list conjunct-2)) unf-left)]
                      [lock-right (renames? (cons conjunct-1 (map gen-node-conjunct current-gen)) (unfold-multi-many-right conjunct-1 offset offset))]) (and lock-left lock-right)))
       (struct-copy grouping acc [potential (car (group-sequential-generations (append (car (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)) (list node)) (add1 next-multi-id) (add1 dummy-id) lvl))] [current-gen '()] [dummy-id (+ dummy-id 2)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _))
        (list-rest (gen-node _ _ (gen o id) #f _) _)
        (gen-node conjunct-2 _ (gen-range p q id asc?) #f #t))
       #:when (and
               (or (and asc? (equal? p (gen-add1 o)))
                   (and (not asc?) (equal? p (gen-sub1 o))))
               (let* ([offset (apply max (assemble-var-indices (λ (_) #t) (cons conjunct-1 (cons conjunct-2 (map gen-node-conjunct current-gen)))))]
                      [unf-left (unfold-multi-many conjunct-2 offset offset)]
                      [lock-left? (renames? (append (map gen-node-conjunct current-gen) (list conjunct-2)) unf-left)]) lock-left?))
       (struct-copy grouping acc [completed (append completed potential)] [potential (car (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl))] [current-gen '()] [dummy-id (add1 dummy-id)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _))
        (list-rest (gen-node _ _ (gen o id) #f _) _)
        (gen-node conjunct-2 _ (gen-range p q id asc?) #f #t))
       #:when (and
               (or (and asc? (equal? p (gen-add1 o)))
                   (and (not asc?) (equal? p (gen-sub1 o))))
               (let ([offset (apply max (assemble-var-indices (λ (_) #t) (list conjunct-1 conjunct-2)))])
                 (and (renames? (cons conjunct-1 (map gen-node-conjunct current-gen)) (unfold-multi-many-right conjunct-1 offset offset))
                      (renames? (append (map gen-node-conjunct current-gen) (list conjunct-2)) (unfold-multi-many conjunct-2)))))
       (let* ([grp-1 (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)]
              [grp-2 (group-sequential-generations (append (car grp-1) (list node)) (add1 next-multi-id) (add1 dummy-id) lvl)])
         (struct-copy grouping acc [potential (car grp-2)] [current-gen (list)] [dummy-id (+ dummy-id 2)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _))
        (list-rest (gen-node _ _ (gen o id) #f _) _)
        (gen-node conjunct-2 _ (gen-range p q id asc?) #f #t))
       #:when (and
               (or (and asc? (equal? p (gen-add1 o)))
                   (and (not asc?) (equal? p (gen-sub1 o))))
               (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-1))])
                 (and (renames? (cons conjunct-1 (map gen-node-conjunct current-gen)) (unfold-multi-many-right conjunct-1 offset offset)))))
       (let* ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp))] [potential (list node)] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _))
        (list-rest (gen-node _ _ (gen o id) #f _) _)
        (gen-node conjunct-2 _ (gen-range p q id asc?) #f #t))
       #:when (and
               (or (and asc? (equal? p (gen-add1 o)))
                   (and (not asc?) (equal? p (gen-sub1 o))))
               (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-2))])
                 (renames? (append (map gen-node-conjunct current-gen) (list conjunct-2)) (unfold-multi-many conjunct-2))))
       (let* ([grp (group-sequential-generations (append current-gen (list node)) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed potential)] [potential (car grp)] [current-gen (list)] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id asc?) _ _))
        (list-rest (gen-node _ _ (gen o id) #f _) _)
        (gen-node conjunct-2 _ (gen-range p q id asc?) #f #t))
       #:when (and
               (or (and asc? (equal? p (gen-add1 o)))
                   (and (not asc?) (equal? p (gen-sub1 o)))))
       (struct-copy grouping acc [completed (append completed potential current-gen)] [potential (list node)] [current-gen (list)])]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list-rest (gen-node _ _ (gen o id-1) #f _) _) (gen-node conjunct-2 _ (gen-range _ _ id-2 _) #f #t))
       #:when (and (not (eqv? id-1 id-2))
                   (let ([offset (apply max (assemble-var-indices (λ (_) #t) conjunct-1))])
                     (renames?
                      (unfold-multi-many-right conjunct-1 offset offset)
                      (append (map gen-node-conjunct potential) (map gen-node-conjunct current-gen)))))
       (let ([grp (group-sequential-generations (append potential current-gen) next-multi-id dummy-id lvl)])
         (struct-copy grouping acc [completed (append completed (car grp))] [potential (list node)] [current-gen (list)] [dummy-id (add1 dummy-id)]))]
      [((list (gen-node conjunct-1 _ (gen-range n m id-1 asc?) _ _)) (list-rest (gen-node _ _ (gen o id-1) #f _) _) (gen-node conjunct-2 _ (gen-range _ _ id-2 _) #f #t))
       (struct-copy grouping acc [completed (append completed potential current-gen)] [potential (list node)] [current-gen (list)])]
      [(_ _ _) (error (format "potential: ~a current-gen: ~a node: ~a" potential current-gen node))])))

(define (generalize-level lvl)
  (define multis (filter multi? (map gen-node-conjunct lvl)))
  (define multivars
    (apply append (map extract-subscripted-variables multis)))
  (define next-multi-id
    (if (not (null? multivars))
        (add1 (apply max (map abstract-variable*-multi-id multivars)))
        1))
  (define dummy-id (add1 (apply max (map gen-node-id lvl))))
  (finalize
   (foldl
    group-conjuncts
    (grouping (list) #f (list) next-multi-id dummy-id lvl)
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
      (abstract-atom* 'collect (list (g* 1 'i 4) (a* 1 'i 4)))
      (abstract-atom* 'append (list (a* 1 'i 3) (a* 1 'i 4) (a* 1 'i 5))))
     #f
     (init
      (list
       (cons (g* 1 1 4) (g 4))
       (cons (a* 1 1 4) (a 4))
       (cons (a* 1 1 3) (a 3))
       (cons (a* 1 1 5) (a 5))))
     (consecutive
      (list (cons (a* 1 'i+1 3) (a* 1 'i 5))))
     (final
      (list
       (cons (g* 1 'L 4) (g 6))
       (cons (a* 1 'L 4) (a 6))
       (cons (a* 1 'L 3) (a 5))
       (cons (a* 1 'L 5) (a 7)))))
    (abstract-atom 'collect (list (g 8) (a 8)))
    (abstract-atom 'eq (list (a 7) (a 8)))))
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
     (list (abstract-atom* 'filter (list (g* 1 'i 2) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 2) (g 2))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive
      (list
       (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 2) (g 3))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5)))
    (multi
     (list (abstract-atom* 'filter (list (g* 2 'i 6) (a* 2 'i 5) (a* 2 'i 6))))
     #t
     (init
      (list
       (cons (g* 2 1 6) (g 6))
       (cons (a* 2 1 5) (a 5))
       (cons (a* 2 1 6) (a 6))))
     (consecutive
      (list
       (cons (a* 2 'i+1 5) (a* 2 'i 6))))
     (final
      (list
       (cons (g* 2 'L 6) (g 7))
       (cons (a* 2 'L 5) (a 6))
       (cons (a* 2 'L 6) (a 7)))))
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
         (cons (a* 1 'L 1) (a 3))
         (cons (a* 1 'L 2) (a 4)))))
      3
      (gen-range 1 3 1 #t)
      #f
      #t)
     (gen-node (abstract-atom 'filter (list (g 4) (a 4) (a 5))) 4 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 10) (abstract-function 'cons (list (g 5) (a 5))) (a 6))) 5 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 6) (a 6) (a 7))) 6 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'sift (list (a 7) (a 8))) 7 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'len (list (a 8) (g 7))) 8 (gen 0 #f) #f #t)))
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
       (cons (a* 1 'L 1) (a 4))
       (cons (a* 1 'L 2) (a 5)))))
    (abstract-atom 'filter (list (g 10) (abstract-function 'cons (list (g 5) (a 5))) (a 6)))
    (abstract-atom 'filter (list (g 6) (a 6) (a 7)))
    (abstract-atom 'sift (list (a 7) (a 8)))
    (abstract-atom 'len (list (a 8) (g 7)))))
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
  (-> (listof gen-node?) (listof gen-node?))
  (lvl)
  @{Attempts to generalize the conjunction represented by @racket[lvl].}))

(define (generalize br)
  (define gr (generational-graph-skeleton br))
  (define root (gen-node (car (tree-label-conjunction (car br))) 1 #f #t #t))
  (define annotated-root (struct-copy gen-node root [range (gen 0 #f)]))
  (define depth (length br))
  (define targets (map (λ (e) (struct-copy gen-node e [range (gen 0 #f)])) (candidate-targets gr root depth))) ; is this really the best place for this? might want to smooth over generation elsewhere...
  (annotate-general! gr root targets depth)
  (define lvl (sort (rdag-level gr annotated-root depth) < #:key gen-node-id))
  (define gen-lvl (generalize-level lvl))
  (cons (map gen-node-conjunct gen-lvl) (generalized-ranges lvl gen-lvl)))
(provide
 (proc-doc/names
  generalize
  (-> (listof (or/c tree-label? generalization?)) (cons/c (listof abstract-conjunct?) (listof index-range?)))
  (candidate-branch)
  @{Applies generalization to the bottom level of @racket[candidate-branch].
 Returns a pair consisting of the generalized conjunction and the generalized index ranges.
 If generalization has no effect, the generalized conjunction is identical to the initial conjunction and the list of ranges is empty.}))