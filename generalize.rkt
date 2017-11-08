#lang at-exp 2d racket
(require
  2d/match
  racket/struct
  scribble/srcdoc
  (only-in "abstract-analysis.rkt" tree-label? tree-label-conjunction generalization?)
  cclp/abstract-multi-domain
  (only-in "abstract-domain-ordering.rkt" renames?)
  (only-in "abstraction-inspection-utils.rkt" assemble-var-indices extract-subscripted-variables extract-variables get-multi-id)
  (only-in "abstract-renaming.rkt" offset-vars)
  (only-in "abstract-unify.rkt" abstract-unify)
  (only-in "abstract-substitution.rkt" abstract-equality apply-substitution)
  (only-in "data-utils.rkt" some-v)
  cclp/gen-graph-structs
  cclp/genealogical-graph
  (only-in "multi-folding-unfolding.rkt" remove-multi-subscripts)
  (only-in "multi-unfolding.rkt" unfold-multi-many unfold-multi-many-bounded unfold-multi-many-right)
  (only-in racket-list-utils/utils replace-sublist map-accumulatel group-by)
  cclp/clustering
  graph)
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
     (gen-node (multi '() #t empty empty empty) 11 (gen-range 1 2 1 #t) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5))) 5 (gen 3 1) #f #t)
     (gen-node (multi '() #t empty empty empty) 12 (gen-range 4 5 1 #t) #f #t)
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
  ;; !!!!! POTENTIAL HERE IS NOT THE POTENTIAL FIELD OF A GROUPING BUT AN EXTENSION !!!!!
  ;; RENAME!
  ;; TODO: simplify, this thing is bloated
  (define partitioning (group-by gen-node-range potential))
  (define (aux partitioning)
    (match partitioning
      [(list
        (and (list-rest (gen-node (? abstract-atom?) _ (gen genn-1 id) _ _) first-rest) lvl-1)
        (and (list-rest (gen-node (? abstract-atom?) _ (gen genn-2 id) _ _) second-rest) lvl-2))
       (cons
        (let* ([gen-1 (map gen-node-conjunct lvl-1)]
               [gen-2 (map gen-node-conjunct lvl-2)]
               [offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) (append gen-1 gen-2))))]
               [offset-gen-2 (offset-vars gen-2 offset offset)]
               [subst-1 (some-v (abstract-unify (map abstract-equality gen-1 offset-gen-2) 0))]
               [shared (filter (match-lambda [(abstract-equality v1 v2) (and (member (offset-vars v2 (- offset) (- offset)) (extract-variables gen-1)) (member (offset-vars v2 (- offset) (- offset)) (extract-variables gen-2)))]) subst-1)]
               [new-consecutive (map (match-lambda [(abstract-equality (a idx1) (a idx2)) (cons (a* fresh-id 'i+1 idx1) (a* fresh-id 'i (- idx2 offset)))] [(abstract-equality (g idx1) (g idx2)) (cons (g* fresh-id 'i+1 idx1) (g* fresh-id 'i (- idx2 offset)))]) shared)]
               [context (foldr (λ (el acc) (if (or (member el lvl-1) (member el lvl-2)) acc (cons el acc))) '() lvl)]
               [context-vars (extract-variables (map gen-node-conjunct context))]
               [new-init (map (λ (v) (cons (prefix-subscripts fresh-id 1 v) v)) (filter (λ (v) (member v context-vars)) (extract-variables (map gen-node-conjunct lvl-1))))]
               [new-final (foldr (λ (el acc)
                                   (match el
                                     [(abstract-equality (a idx1) (a idx2)) (if (member (a (- idx2 offset)) context-vars) (cons (cons (a* fresh-id 'L idx1) (a (- idx2 offset))) acc) acc)]
                                     [(abstract-equality (g idx1) (g idx2)) (if (member (g (- idx2 offset)) context-vars) (cons (cons (g* fresh-id 'L idx1) (g (- idx2 offset))) acc) acc)])) '() subst-1)])
          (list
           (gen-node
            (multi
             (prefix-subscripts fresh-id 'i (map gen-node-conjunct lvl-1))
             (gen-number< genn-1 genn-2)
             new-init
             new-consecutive
             new-final
             id)
            dummy-id
            (gen-range genn-1 genn-2 id (gen-number< genn-1 genn-2))
            #f
            #t)))
        (add1 fresh-id))]
      [(list
        (and (list-rest (gen-node (? abstract-atom?) _ (gen n id) _ _) first-rest) lvl-1)
        (and (list (gen-node (and (? multi?) existing-multi) _ (gen-range m o id asc?) _ _)) lvl-2))
       (let* ([existing-instance (map gen-node-conjunct lvl-1)]
              [subscriptless-instance (remove-multi-subscripts (multi-conjunction existing-multi))]
              [offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) (append subscriptless-instance (map gen-node-conjunct lvl-1)))))]
              [unifiable-instance (offset-vars subscriptless-instance offset offset)]
              [unification (some-v (abstract-unify (list (abstract-equality unifiable-instance existing-instance)) 0))]
              [context (foldr (λ (el acc) (if (or (member el lvl-1) (member el lvl-2)) acc (cons el acc))) '() lvl)]
              [context-vars (extract-variables (map gen-node-conjunct context))]
              [new-init
               (filter
                 (match-lambda [(cons v1 v2) (member v2 context-vars)])
                 (map
                  (match-lambda
                    [(abstract-equality var1 var2)
                     (cons
                      (prefix-subscripts (multi-id existing-multi) 1 (offset-vars var1 (- offset) (- offset)))
                      var2)])
                  unification))])
         (cons (list (gen-node (struct-copy multi existing-multi [init new-init]) dummy-id (gen-range n o id asc?) #f #t)) fresh-id))]     
      [(list
        (and (list (gen-node (and (? multi?) existing-multi) _ (gen-range m o id asc?) _ _)) lvl-1)
        (and (list-rest (gen-node (? abstract-atom?) _ (gen n id) _ _) first-rest) lvl-2))
       (let* ([existing-instance (map gen-node-conjunct lvl-2)]
              [subscriptless-instance (remove-multi-subscripts (multi-conjunction existing-multi))]
              [offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) (append subscriptless-instance (map gen-node-conjunct lvl-2)))))]
              [unifiable-instance (offset-vars subscriptless-instance offset offset)]
              [unification (some-v (abstract-unify (list (abstract-equality unifiable-instance existing-instance)) 0))]
              [context (foldr (λ (el acc) (if (or (member el lvl-1) (member el lvl-2)) acc (cons el acc))) '() lvl)]
              [context-vars (extract-variables (map gen-node-conjunct context))]
              ; only this is different wrt previous case
              [new-final
               (filter
                (match-lambda [(cons v1 v2) (member v2 context-vars)])
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
        (list (gen-node (multi placeholder-2 _ _ _ final-2 rta) _ (gen-range o p id asc?) _ _)))
       (let* ([placeholder-1 (multi-conjunction existing-multi-1)]
              [subscriptless-instance-1 (remove-multi-subscripts placeholder-1)]
              [subscriptless-instance-2 (remove-multi-subscripts placeholder-2)]
              [offset (apply max (cons 0 (append (assemble-var-indices (λ (_) #t) subscriptless-instance-1) (assemble-var-indices (λ (_) #t) subscriptless-instance-2))))]
              [offset-instance-2 (offset-vars subscriptless-instance-2 offset offset)]
              [subst (some-v (abstract-unify (list (abstract-equality offset-instance-2 subscriptless-instance-1)) 0))]
              [new-final
               (map
                (match-lambda
                  [(cons lhs rhs)
                   (cons (prefix-subscripts (multi-id existing-multi-1) 'L (apply-substitution subst (offset-vars (remove-multi-subscripts lhs) offset offset)))
                         rhs)])
                final-2)])
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

;; folding could keep some conjuncts in current-gen or potential
;; these should be considered completed, as well
(define (finalize g)
  (match g
    [(grouping co #f cu _ _ _)
     (append co cu)]
    [(grouping co po cu _ _ _)
     (append co po cu)]))

;; some elements can never go in the current-gen component, regardless of current state
(define (can-be-in-current-gen? node)
  (and (not (multi? (gen-node-conjunct node)))
       (not (equal? (gen-node-range node)
                    (gen 0 #f)))
       (gen-node-foldable? node)))

;; whether we can append also depends on contents of current gen
(define (can-append-to-current-gen? current-gen node)
  (and (can-be-in-current-gen? node)
       (or (null? current-gen)
           (equal? (gen-node-range (first current-gen)) (gen-node-range node)))))

(define (next-current-gen current-gen node)
  (cond
    [(can-append-to-current-gen? current-gen node)
     (append current-gen (list node))]
    [(can-be-in-current-gen? node)
     (list node)]
    [else empty]))

(define (can-group?/pcgn potential current-gen node)
  (and (multi? (gen-node-conjunct node))
       (strung-together? current-gen node) ; implies not current-gen not empty
       (let ([combined
              (caar
               (group-sequential-generations
                (append current-gen (list node))
                (add1 (get-multi-id (gen-node-conjunct node)))
                1 ; dummy ID is irrelevant here
                (append (or potential empty) current-gen (list node))))])
         (strung-together? potential combined))))

(define (can-group? potential current-gen node)
  (and (not (can-append-to-current-gen? current-gen node))
       (or
        (can-group?/pcgn potential current-gen node)
        (if (null? current-gen)
            (strung-together? potential node)
            (or (strung-together? potential current-gen)
                (strung-together? current-gen node))))))

(define (next-dummy-id potential current-gen node dummy-id)
  (if (can-group? potential current-gen node)
      (add1 dummy-id)
      dummy-id))

(define (needs-new-multi-id? potential current-gen node)
  (and (can-group? potential current-gen node)
       (not (or (multi? (gen-node-conjunct node))
                (multi? (gen-node-conjunct (first potential)))))))

(define (next-multi-id potential current-gen node multi-id)
  (if (needs-new-multi-id? potential current-gen node)
      (add1 multi-id)
      multi-id))  

(define (resets-potential? potential current-gen node)
  (or (not (gen-node-foldable? node))
      (equal? (gen-node-range node) (gen 0 #f))
      ;; cannot append to current gen -> has to be shifted if possible
      ;; no point shifting current gen -> even if we can group with current-gen, potential becomes #f
      (and (not (null? current-gen))
           (not (can-append-to-current-gen? current-gen node))
           (not (subsequent-gens?
                 (gen-node-range (first current-gen))
                 (gen-node-range node)))
           (not (multi? (gen-node-conjunct node))))
      (and potential
           (null? current-gen) ; implies potential is multi
           (not (subsequent-gens?
                 (gen-node-range (first potential))
                 (gen-node-range node))))))
(provide
 (proc-doc/names
  resets-potential?
  (-> (or/c (listof abstract-atom?) (listof multi?))
      (listof gen-node?)
      gen-node?
      boolean?)
  (potential current-gen node)
  @{Tests whether the current combination of a potential abstraction @racket[potential], a current generation @racket[current-gen] and a node @racket[node] being processed leads to the complete absence of a potential abstraction in the next step, represented as @racket[#f].}))

(define (next-potential potential current-gen node fresh-multi-id fresh-dummy-id lvl)
  (define (group h t lvl)
    (car
     (group-sequential-generations
      (append h t)
      fresh-multi-id
      fresh-dummy-id
      lvl)))
  (cond
    [(resets-potential? potential current-gen node) (cons #f 'reset)] ; only case for #f
    [(and (not (can-append-to-current-gen? current-gen node))
          (can-group?/pcgn potential current-gen node))
     (let ([first-grouping (group current-gen (list node) lvl)])
       (cons (group potential first-grouping (replace-sublist lvl (append current-gen (list node)) first-grouping)) 'extended))]
    [(and (not (can-append-to-current-gen? current-gen node))
          (not (null? current-gen))
          (strung-together? potential current-gen)
          (not (multi? (gen-node-conjunct node))))
     (cons (group potential current-gen lvl) 'extended)]
    [(and (null? current-gen)
          (strung-together? potential node))
     (cons (group potential (list node) lvl) 'extended)]
    [(and (multi? (gen-node-conjunct node))
          (not (null? current-gen))
          (strung-together? current-gen node))
     (cons (group current-gen (list node) lvl) 'replaced-by-cgn)]
    [(and
      (not (null? current-gen))
      (abstract-atom? (gen-node-conjunct node))
      (subsequent-gens? (gen-node-range (first current-gen)) (gen-node-range node)))
     (cons current-gen 'replaced-by-current-gen)] ; only case leading to list of atoms
    [(multi? (gen-node-conjunct node))
     (cons (list node) 'replaced-by-node)]
    [(can-append-to-current-gen? current-gen node)
     (cons potential 'retained)]
    [else (error "missing an option")]))

(define (strung-together? potential suffix)
  (define (atoms-join-atoms? a1 a2)
    (and (or (equal?
              (gen-increment (gen-node-range (first a1)))
              (gen-node-range (first a2)))
             (equal?
              (gen-decrement (gen-node-range (first a1)))
              (gen-node-range (first a2))))
         (renames?
          (map gen-node-conjunct a1)
          (map gen-node-conjunct a2))))
  (define (atoms-join-multi? a m)
    (match* (a m)
      [((list-rest
         (gen-node _ _ (gen n r) _ _) _)
        (gen-node (multi conjunct asc? i c f rta) _ (gen-range l _ r asc?) _ _))
       (and
        (or
         (and asc? (equal? (gen-add1 n) l))
         (and (not asc?) (equal? (gen-sub1 n) l)))
        (let ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) (map gen-node-conjunct (cons m a)))))])
          (renames?
           (append (map gen-node-conjunct potential) (list (multi conjunct asc? i c f rta)))
           (unfold-multi-many (multi conjunct asc? i c f rta) offset offset))))]
      [(_ _) #f]))
  (define (multi-joins-atoms? m a)
    (match* (m a)
      [((gen-node (multi conjunct asc? i c f rta) _ (gen-range _ l r asc?) _ _)
        (list-rest
         (gen-node _ _ (gen n r) _ _) _))
       (and
        (or
         (and asc? (equal? (gen-add1 l) n))
         (and (not asc?) (equal? (gen-sub1 l) n)))
        (let ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) (map gen-node-conjunct (cons m a)))))])
          (renames?
           (unfold-multi-many-right (multi conjunct asc? i c f rta) offset offset)
           (append (map gen-node-conjunct potential) (map gen-node-conjunct a)))))]
      [(_ _) #f]))
  (define (multi-joins-multi? m1 m2)
    (match* (m1 m2)
      [((list (gen-node (and (multi _ asc? _ _ _ rta) conjunct-1) _ (gen-range _ m r asc?) _ _))
        (gen-node (and (multi _ asc? _ _ _ rta) conjunct-2) _ (gen-range o _ r asc?) _ _))
       (let ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) (cons conjunct-2 (map gen-node-conjunct m1)))))])
         (and
          (or (and asc? (equal? o (gen-add1 m))) (and (not asc?) (equal? o (gen-sub1 m))))
          (renames? (append (drop (unfold-multi-many-right conjunct-1 offset offset) 1) (list conjunct-2)) (unfold-multi-many conjunct-2 offset offset))))]
      [(_ _) #f]))
  #2dmatch
  ╔══════════════════════════════════════════════════════╦═════════════════════════════════════════════════════════════╦════════════════════════════════════════════════╦════╗
  ║ potential suffix                                     ║ (list-rest (gen-node (? abstract-atom?) _ _ _ _) _)         ║ (gen-node (? multi?) _ _ _ _)                  ║ _  ║
  ╠══════════════════════════════════════════════════════╬═════════════════════════════════════════════════════════════╬════════════════════════════════════════════════╬════╣
  ║ (list-rest (gen-node (? abstract-atom?) _ _ _ _) _)  ║ (atoms-join-atoms? potential suffix)                        ║ (atoms-join-multi? potential suffix)           ║    ║
  ╠══════════════════════════════════════════════════════╬═════════════════════════════════════════════════════════════╬════════════════════════════════════════════════╣    ║
  ║ (list-rest (gen-node (? multi?) _ _ _ _) _)          ║ (multi-joins-atoms? (first potential) suffix)               ║ (multi-joins-multi? (first potential) suffix)  ║    ║
  ╠══════════════════════════════════════════════════════╬═════════════════════════════════════════════════════════════╩════════════════════════════════════════════════╝    ║
  ║ _                                                    ║                                                                                                                #f ║
  ╚══════════════════════════════════════════════════════╩═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝)



(define (next-completed completed potential current-gen node expl fresh-multi-id fresh-dummy-id lvl)
  (define (eq-any? e0 e1 . es)
    (or (eq? e0 e1)
        (ormap (λ (e) (eq? e0 e)) es)))
  (append
   completed
   (cond
     [(or
       (not potential)
       (eq-any? expl 'extended 'retained)) empty]
     [(and
       (eq-any? expl 'replaced-by-node 'reset)
       (strung-together? potential current-gen)) ; length is that associated with potential + length of current-gen
      (car (group-sequential-generations (append potential current-gen) fresh-multi-id fresh-dummy-id lvl))]
     [(eq-any? expl 'replaced-by-node 'reset 'replaced-by-current-gen 'replaced-by-cgn) ; length is that associated with potential
      potential])
   (if
    (and
     (not (null? current-gen)) ; so (first current-gen) is safe after this
     (and
      (or ; can't append new node to current-gen
       (not
        (equal?
         (gen-node-range (first current-gen))
         (gen-node-range node)))
       (and
        (equal?
         (gen-node-range (first current-gen))
         (gen-node-range node))
        (not
         (gen-node-foldable? node))))
      (not ; can't shift current-gen to become next potential
       (and
        (subsequent-gens?
         (gen-node-range (first current-gen))
         (gen-node-range node))
        (gen-node-foldable? node)
        (abstract-atom?
         (gen-node-conjunct node))))
      (not ; can't subsume in any way: we know current-gen is not null so it must be part of group
       (can-group?
        potential
        current-gen
        node))))
    current-gen
    empty)
   ;; not the same as "can-be-in-current-gen?" because that includes multi
   (if (or (equal? (gen-node-range node) (gen 0 #f))
           (not (gen-node-foldable? node)))
       (list node)
       empty)))

(define (group-conjuncts node acc)
  (match-let* ([(and (grouping completed potential current-gen fresh-multi-id fresh-dummy-id lvl) acc-grouping) acc]
               [(cons next-potential-value next-potential-explanation)
                (next-potential potential current-gen node fresh-multi-id fresh-dummy-id lvl)])
    (struct-copy
     grouping
     acc-grouping
     [completed (next-completed completed potential current-gen node next-potential-explanation fresh-multi-id fresh-dummy-id lvl)]
     [potential next-potential-value]
     [current-gen (next-current-gen current-gen node)]
     [next-multi-id (next-multi-id potential current-gen node fresh-multi-id)]
     [dummy-id (next-dummy-id potential current-gen node fresh-dummy-id)])))

(define (generalize-level lvl)
  (define multis (filter multi? (map gen-node-conjunct lvl)))
  (define multivars
    (apply append (map extract-subscripted-variables multis)))
  (define next-multi-id
    (if (not (null? multivars))
        (add1 (apply max (map abstract-variable*-multi-id multivars)))
        1))
  (define dummy-id (add1 (apply max (map gen-node-id lvl))))
  (finalize ; move stragglers to completed
   (foldl
    group-conjuncts
    (grouping (list) #f (list) next-multi-id dummy-id lvl)
    (sort lvl < #:key gen-node-id))))
(module+ test
  (require rackunit)
  (check-equal?
   (map
    gen-node-conjunct
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
      (gen-node (abstract-atom 'eq (list (a 7) (a 8))) 10 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'collect (list (g 1) (a 1)))
    (abstract-atom 'collect (list (g 2) (a 2)))
    (abstract-atom 'append (list (a 1) (a 2) (a 3)))
    (multi
     (list
      (abstract-atom* 'collect (list (g* 1 'i 4) (a* 1 'i 4)))
      (abstract-atom* 'append (list (a* 1 'i 3) (a* 1 'i 4) (a* 1 'i 5))))
     #f
     (list (cons (a* 1 1 3) (a 3)))
     (list (cons (a* 1 'i+1 3) (a* 1 'i 5)))
     (list (cons (a* 1 'L 5) (a 7)))
     1)
    (abstract-atom 'collect (list (g 8) (a 8)))
    (abstract-atom 'eq (list (a 7) (a 8)))))
  (check-equal?
   (map
    gen-node-conjunct
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
      (gen-node (abstract-atom 'len (list (a 9) (g 9))) 10 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 2) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (list
      (cons (a* 1 1 1) (a 1)))
     (list
      (cons (a* 1 'i+1 1) (a* 1 'i 2)))
     (list
      (cons (a* 1 'L 2) (a 3)))
     1)
    (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5)))
    (multi
     (list (abstract-atom* 'filter (list (g* 2 'i 6) (a* 2 'i 5) (a* 2 'i 6))))
     #t
     (list
      (cons (a* 2 1 5) (a 5)))
     (list
      (cons (a* 2 'i+1 5) (a* 2 'i 6)))
     (list
      (cons (a* 2 'L 6) (a 7)))
     1)
    (abstract-atom 'filter (list (g 8) (a 7) (a 8)))
    (abstract-atom 'sift (list (a 8) (a 9)))
    (abstract-atom 'len (list (a 9) (g 9)))))
  (check-equal?
   (map
    gen-node-conjunct
    (generalize-level
     (list
      (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
      (gen-node
       (multi
        (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
        #t
        (list
         (cons (a* 1 1 1) (a 1)))
        (list
         (cons (a* 1 'i+1 1) (a* 1 'i 2)))
        (list
         (cons (a* 1 'L 2) (a 4)))
        1)
       3
       (gen-range 1 3 1 #t)
       #f
       #t)
      (gen-node (abstract-atom 'filter (list (g 4) (a 4) (a 5))) 4 (gen 4 1) #f #t)
      (gen-node (abstract-atom 'filter (list (g 10) (abstract-function 'cons (list (g 5) (a 5))) (a 6))) 5 (gen 5 1) #f #t)
      (gen-node (abstract-atom 'filter (list (g 6) (a 6) (a 7))) 6 (gen 6 1) #f #t)
      (gen-node (abstract-atom 'sift (list (a 7) (a 8))) 7 (gen 6 1) #f #t)
      (gen-node (abstract-atom 'len (list (a 8) (g 7))) 8 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (list
      (cons (a* 1 1 1) (a 1)))
     (list
      (cons (a* 1 'i+1 1) (a* 1 'i 2)))
     (list
      (cons (a* 1 'L 2) (a 5)))
     1)
    (abstract-atom 'filter (list (g 10) (abstract-function 'cons (list (g 5) (a 5))) (a 6)))
    (abstract-atom 'filter (list (g 6) (a 6) (a 7)))
    (abstract-atom 'sift (list (a 7) (a 8)))
    (abstract-atom 'len (list (a 8) (g 7)))))
  (check-equal?
   (map
    gen-node-conjunct
    (generalize-level
     (list
      (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
      (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 3 (gen 1 1) #f #t)
      (gen-node
       (multi
        (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
        #t
        (list
         (cons (a* 1 1 1) (abstract-function 'cons (list (g 3) (a 2)))))
        (list
         (cons (a* 1 'i+1 1) (a* 1 'i 2)))
        (list
         (cons (a* 1 'L 2) (a 3)))
        1)
       3
       (gen-range 2 'n 1 #t)
       #f
       #t)
      (gen-node (abstract-atom 'filter (list (g 4) (a 3) (a 4))) 4 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'sift (list (a 4) (a 5))) 5 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'len (list (a 5) (g 6))) 6 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (abstract-atom 'filter (list (g 2) (a 1) (a 2)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (list
      (cons (a* 1 1 1) (abstract-function 'cons (list (g 3) (a 2)))))
     (list
      (cons (a* 1 'i+1 1) (a* 1 'i 2)))
     (list
      (cons (a* 1 'L 2) (a 3)))
     1)
    (abstract-atom 'filter (list (g 4) (a 3) (a 4)))
    (abstract-atom 'sift (list (a 4) (a 5)))
    (abstract-atom 'len (list (a 5) (g 6)))))
  (check-equal?
   (map
    gen-node-conjunct
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
        (list
         (cons (a* 1 1 2) (a 5)))
        (list (cons (a* 1 'i+1 2) (a* 1 'i 3)))
        (list
         (cons (a* 1 'L 3) (a 10)))
        1)
       7
       (gen-range 3 1 1 #f)
       #f
       #t)
      (gen-node (abstract-atom 'collect (list (g 11) (a 11))) 8 (gen 0 #f) #f #t)
      (gen-node (abstract-atom 'eq (list (a 10) (a 11))) 9 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'collect (list (g 1) (a 1)))
    (abstract-atom 'collect (list (g 2) (a 2)))
    (abstract-atom 'append (list (a 1) (a 2) (a 3)))
    (multi
     (list
      (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
      (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
     #f
     (list
      (cons (a* 1 1 2) (a 3)))
     (list (cons (a* 1 'i+1 2) (a* 1 'i 3)))
     (list
      (cons (a* 1 'L 3) (a 10)))
     1)
    (abstract-atom 'collect (list (g 11) (a 11)))
    (abstract-atom 'eq (list (a 10) (a 11)))))
  (let ([node1 (gen-node (abstract-atom 'allsafe (list (g 1) (g 2) (g 3) (abstract-function 'cons (list (g 4) (a 1))))) 2 (gen 1 1) #f #f)]
        [node2
         (gen-node
          (multi
           (list
            (abstract-atom* 'allsafe (list (g* 1 'i 1) (g* 1 'i 2) (g* 1 'i 3) (abstract-function* 'cons (list (g* 1 'i 4) (a* 1 'i 1))))))
           #t
           (list (cons (g* 1 1 4) (g 4)))
           (list
            (cons (g* 1 'i+1 4) (g* 1 'i 4))
            (cons (a* 1 'i+1 1) (a* 1 'i 1)))
           empty
           1)
          3
          (gen-range 2 5 1 #t)
          #f
          #f)])
    (check-equal?
     (generalize-level (list node1 node2))
     (list node1 node2)))
  (check-equal?
   (map
    gen-node-conjunct
    (generalize-level
     (list
      (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
      (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 3 (gen 1 1) #f #t)
      (gen-node (abstract-atom 'filter (list (g 3) (a 2) (a 3))) 4 (gen 2 1) #f #t)
      (gen-node
       (multi
        (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
        #t
        (list
         (cons (a* 1 1 1) (a 3)))
        (list
         (cons (a* 1 'i+1 1) (a* 1 'i 2)))
        (list
         (cons (a* 1 'L 2) (a 4)))
        1)
       5
       (gen-range 3 'n 1 #t)
       #f
       #t)
      (gen-node (abstract-atom 'filter (list (g 4) (a 4) (a 5))) 6 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'sift (list (a 5) (a 6))) 7 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'len (list (a 6) (g 5))) 8 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (list
      (cons (a* 1 1 1) (a 1)))
     (list
      (cons (a* 1 'i+1 1) (a* 1 'i 2)))
     (list
      (cons (a* 1 'L 2) (a 4)))
     1)
    (abstract-atom 'filter (list (g 4) (a 4) (a 5)))
    (abstract-atom 'sift (list (a 5) (a 6)))
    (abstract-atom 'len (list (a 6) (g 5)))))
  (check-equal?
   (map
    gen-node-conjunct
    (generalize-level
     (list
      (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
      (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 3 (gen 1 1) #f #t)
      (gen-node (abstract-atom 'filter (list (g 3) (a 2) (a 3))) 4 (gen 2 1) #f #t)
      (gen-node
       (multi
        (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
        #t
        (list
         (cons (a* 1 1 1) (abstract-function 'cons (list (g 4) (a 3)))))
        (list
         (cons (a* 1 'i+1 1) (a* 1 'i 2)))
        (list
         (cons (a* 1 'L 2) (a 4)))
        1)
       5
       (gen-range 3 'n 1 #t)
       #f
       #t)
      (gen-node (abstract-atom 'filter (list (g 5) (a 4) (a 5))) 6 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'sift (list (a 5) (a 6))) 7 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'len (list (a 6) (g 6))) 8 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 2 'i 2) (a* 2 'i 1) (a* 2 'i 2))))
     #t
     (list
      (cons (a* 2 1 1) (a 1)))
     (list
      (cons (a* 2 'i+1 1) (a* 2 'i 2)))
     (list
      (cons (a* 2 'L 2) (a 3)))
     1)
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (list
      (cons (a* 1 1 1) (abstract-function 'cons (list (g 4) (a 3)))))
     (list
      (cons (a* 1 'i+1 1) (a* 1 'i 2)))
     (list
      (cons (a* 1 'L 2) (a 4)))
     1)
    (abstract-atom 'filter (list (g 5) (a 4) (a 5)))
    (abstract-atom 'sift (list (a 5) (a 6)))
    (abstract-atom 'len (list (a 6) (g 6)))))
  (check-equal?
   (map
    gen-node-conjunct
    (generalize-level
     (list
      (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
      (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 3 (gen 1 1) #f #t)
      (gen-node
       (multi
        (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
        #t
        (list
         (cons (a* 1 1 1) (a 2)))
        (list
         (cons (a* 1 'i+1 1) (a* 1 'i 2)))
        (list
         (cons (a* 1 'L 2) (a 3)))
        1)
       4
       (gen-range 2 'n 1 #t)
       #f
       #t)
      (gen-node (abstract-atom 'filter (list (g 3) (a 3) (a 4))) 5 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'sift (list (a 4) (a 5))) 6 (gen (symsum 'n 1) 1) #f #t)
      (gen-node (abstract-atom 'len (list (a 5) (g 4))) 7 (gen 0 #f) #f #t))))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (list
      (cons (a* 1 1 1) (a 1)))
     (list
      (cons (a* 1 'i+1 1) (a* 1 'i 2)))
     (list
      (cons (a* 1 'L 2) (a 3)))
     1)
    (abstract-atom 'filter (list (g 3) (a 3) (a 4)))
    (abstract-atom 'sift (list (a 4) (a 5)))
    (abstract-atom 'len (list (a 5) (g 4))))))

(provide
 (proc-doc/names
  generalize-level
  (-> (listof gen-node?) (listof gen-node?))
  (lvl)
  @{Attempts to generalize the conjunction represented by @racket[lvl].}))

(define (infer-bb pre post)
  (define (grouping-proc gen-seq st)
    (let ([l (length gen-seq)])
      (cons (index-range st (+ st l)) (+ st l))))
  (define (conversion-proc rng Δ)
    (match rng
      [(index-range start end-before)
       (cons
        (cons
         (car
          (map-accumulatel
           grouping-proc
           start
           (group-by
            gen-node-range
            (take
             (drop pre start)
             (- end-before start)))))
         (- start Δ))
        (+ Δ (- end-before start 1)))]))
  (let ([rngs (generalized-ranges pre post)])
    (car (map-accumulatel conversion-proc 0 rngs))))
(module+ test
  (check-equal?
   (infer-bb
    (list
     (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
     (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 3 (gen 1 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 3) (a 2) (a 3))) 4 (gen 2 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (a 3) (abstract-function 'cons (list (g 5) (a 4))))) 5 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 5) (a 4) (a 5))) 6 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 6) (a 5) (a 6))) 7 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 7) (a 6) (a 7))) 8 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'sift (list (a 7) (a 8))) 9 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'length (list (a 8) (g 8))) 10 (gen 0 #f) #f #t))
    (list
     (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
     (gen-node
      (multi
       (list (abstract-atom* 'filter (list (g* 1 'i 2) (a* 1 'i 1) (a* 1 'i 2))))
       #t
       (list
        (cons (a* 1 1 1) (a 1)))
       (list
        (cons (a* 1 'i+1 1) (a* 1 'i 2)))
       (list
        (cons (a* 1 'L 2) (a 3)))
       1)
      11 (gen-range 1 'n 1 #t) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (a 3) (abstract-function 'cons (list (g 5) (a 4))))) 5 (gen (symsum 'n 1) 1) #f #t)
     (gen-node
      (multi
       (list (abstract-atom* 'filter (list (g* 2 'i 2) (a* 2 'i 1) (a* 2 'i 2))))
       #t
       (list
        (cons (a* 2 1 1) (a 4)))
       (list
        (cons (a* 2 'i+1 1) (a* 2 'i 2)))
       (list
        (cons (a* 2 'L 2) (a 5)))
       1)
      12 (gen-range (symsum 'n 2) 'm 1 #t) #f #t)
     (gen-node (abstract-atom 'filter (list (g 5) (a 5) (a 6))) 8 (gen (symsum 'm 1) 1) #f #t)
     (gen-node (abstract-atom 'sift (list (a 5) (a 6))) 9 (gen (symsum 'm 1) 1) #f #t)
     (gen-node (abstract-atom 'length (list (a 6) (g 6))) 10 (gen 0 #f) #f #t)))
   (list (cons (list (index-range 1 2) (index-range 2 3)) 1) (cons (list (index-range 4 5) (index-range 5 6)) 3))))

;; split this out into a function so I can more easily document it
(define (clustered-lvl/info br)
  (define gr (genealogical-graph-skeleton br))
  (define id->encoding (assign-prime-factor-ids gr))
  (define depth (length br))
  (define root
    (gen-node
     (first (tree-label-conjunction (car br))) 1 #f #t #t))
  (define lvl
    (sort
     (rdag-level gr root depth)
     <
     #:key gen-node-id))
  (define multi-encodings (sort (filter-map (λ (v) (and (gen-node-unfolded? v) (multi? (gen-node-conjunct v)) (hash-ref id->encoding (gen-node-id v)))) (get-vertices gr)) <))
  (define encoding->id
    (for/hash ([pair (hash->list id->encoding)])
      (values (cdr pair) (car pair))))
  (define id->conjunct
    (for/hash ([v (in-vertices gr)])
      (values (gen-node-id v) (gen-node-conjunct v))))
  (values
   (cluster lvl id->encoding multi-encodings id->conjunct encoding->id)
   gr
   id->encoding
   depth
   root
   lvl
   encoding->id
   id->conjunct))
(provide clustered-lvl/info)

(define (generalize/bu br)
  (define (sets->lists st)
    (for/list ([sst st])
      (if (set? sst)
          (sets->lists sst)
          sst)))
  (define-values
    (clustered-lvl gr id->encoding depth root lvl encoding->id id->conjunct)
    (clustered-lvl/info br))
  (define annotated-lvl
    (sort
     (set->list
      (flatten-clustering
       (car
        (annotate-cluster
         encoding->id
         id->conjunct
         clustered-lvl))))
     (λ (gn1 gn2)
       (< (gen-node-id gn1)
          (gen-node-id gn2)))))
  (define gen-lvl (generalize-level annotated-lvl))
  (list
   (map gen-node-conjunct gen-lvl)
   (generalized-ranges lvl gen-lvl)
   (infer-bb lvl gen-lvl)))
(provide
 (proc-doc/names
  generalize/bu
  (-> (listof (or/c tree-label? generalization?)) list)
  (candidate-branch)
  @{Applies generalization to the bottom level of @racket[candidate-branch] using a bottom-up recursion analysis.
 Returns a triple consisting of the generalized conjunction, the generalized index ranges and the assignment of building blocks to introduced multis.
 If generalization has no effect, the generalized conjunction is identical to the initial conjunction and the list of ranges is empty.
 This function is experimental, but will hopefully run much faster than the top-down approach.}))

