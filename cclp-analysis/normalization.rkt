#lang alpha-gamma racket
(require
 cclp-common/abstract-domain-ordering
 cclp-common/abstract-renaming
 cclp-common-data/abstract-substitution
 cclp-common/abstract-substitution-application
 cclp-common/abstract-unify
 cclp-common/abstraction-inspection-utils
 cclp-common/data-utils
 cclp-common/multi-folding-unfolding
 cclp-common/multi-unfolding
 cclp-analysis/generalize)
(require cclp-common-data/abstract-multi-domain)

; what does it mean to just wrap the subconjunction of corresponding length that follows?
; use a simple-multi with the same pattern as before
; set the init/final constraints

(define (re-identify m id)
        (match m
               [(multi/annotations sm asc? rta)
                (multi/annotations (re-identify sm id) asc? rta)]
               [(simple-multi patt i c f) ; note that we don't re-identify i/c/f!
                                          (simple-multi (re-identify patt id) i c f)]
               [(? list?) (map (λ (i) (re-identify i id)) m)]
               [(abstract-atom* sym args)
                (abstract-atom* sym (map (λ (i) (re-identify i id)) args))]
               [(abstract-function* sym args)
                (abstract-function* sym (map (λ (i) (re-identify i id)) args))]
               [(a* old-id sym-idx local-idx)
                (a* id sym-idx local-idx)]
               [(g* old-id sym-idx local-idx)
                (g* id sym-idx local-idx)]))

(define (normalize acon)
        (define (multi/annotations->simple-multi c)
                (match c
                       [(multi/annotations sm _ _) sm]
                       [_ c]))
        (define first-multi (findf multi? acon))
        (define first-multi-idx (index-where acon multi?))
        (define all-multis (filter multi? acon))
        (define fresh-id (add1 (apply max (cons 0 (map get-multi-id all-multis)))))
        (if (not first-multi)
            acon
            (match-let*-values
             ([(max-a max-g)
               (values
                (maximum-var-index acon a?)
                (maximum-var-index acon g?))]
              [(a-off g-off)
               (values
                (if (some? max-a) (some-v max-a) 1)
                (if (some? max-g) (some-v max-g) 1))]
              [(next-subconjunction)
               (and
                (> (length acon) (+ first-multi-idx 1))
                (take
                 (drop acon (add1 first-multi-idx))
                 (min
                  (length (multi-conjunction first-multi))
                  (- (length acon) first-multi-idx 1))))]
              [((cons case-one subst)) ; doing it like this and not with unfold-multi* because I want to get just this part later on
                                       (unfold-multi-bounded 1 first-multi a-off g-off)]
              [(spliced) (append (take acon first-multi-idx) case-one (drop acon (add1 first-multi-idx)))]
              [(subbed) (apply-substitution subst spliced)]
              ;; as in generalize.rkt...
              [(subscriptless-instance)
               (remove-multi-subscripts
                (multi-conjunction first-multi))]
              [(offset)
               (apply
                max
                (assemble-var-indices (const #t) subbed))]
              [(unifiable-instance)
               (offset-vars subscriptless-instance offset offset)]
              [(unification)
               (and
                next-subconjunction
                (renames? unifiable-instance next-subconjunction)
                ; additional offset 0 is okay here:
                ; won't unify g with something containing an a, because of >=-extension
                (some-v (abstract-unify (list (abstract-equality unifiable-instance next-subconjunction)) 0)))]
              [(context-vars) (extract-variables case-one)]
              [(new-init)
               (and
                unification
                (filter-map
                 (λ (ae)
                    (match ae
                           [(abstract-equality lhs rhs)
                            (and (member rhs context-vars) (cons (prefix-subscripts fresh-id 1 (offset-vars lhs (- offset) (- offset))) rhs))]))
                 unification))]
              [(new-final)
               (and
                unification
                (filter-map
                 (λ (ae)
                    (match ae
                           [(abstract-equality lhs rhs)
                            (and (not (member rhs context-vars)) (cons (prefix-subscripts fresh-id 'L (offset-vars lhs (- offset) (- offset))) rhs))]))
                 unification))]
              [(new-consecutive)
               (and
                unification
                (map
                 (λ (p)
                    (match p
                           [(cons (a* _ 'i+1 l1) (a* _ 'i l2))
                            (cons (a* fresh-id 'i+1 l1) (a* fresh-id 'i l2))]
                           [(cons (g* _ 'i+1 l1) (g* _ 'i l2))
                            (cons (g* fresh-id 'i+1 l1) (g* fresh-id 'i l2))]))
                 (multi-consecutive first-multi)))]
              [(new-pattern)
               (and
                unification
                (multi-conjunction (re-identify first-multi fresh-id)))]
              [(rewrapped)
               (and
                unification
                (simple-multi new-pattern new-init new-consecutive new-final))]
              [(next-acon)
               (and
                rewrapped
                (append
                 (take subbed (+ first-multi-idx (length subscriptless-instance)))
                 (list rewrapped)
                 (drop subbed (+ first-multi-idx (length subscriptless-instance) 1))))])
             (cond
              [(or (not next-subconjunction)
                   (ormap multi? (cdr next-subconjunction)))
               (append
                (map multi/annotations->simple-multi (take acon (add1 first-multi-idx)))
                (normalize (drop acon (add1 first-multi-idx))))]
              [(and
                rewrapped
                (renames?
                 next-acon
                 acon))
               (normalize next-acon)]
              [rewrapped
               (append
                (take acon (+ first-multi-idx 1))
                (normalize (drop acon (+ first-multi-idx 1))))]
              [(and
                (not rewrapped)
                (multi?
                 (car next-subconjunction))
                (let ([case-one (first (unfold-multi* (add1 first-multi-idx) acon))])
                     (and (renames? case-one acon) case-one)))
               =>
               normalize] ; second multi will be gone
              [(and
                (not rewrapped)
                (multi?
                 (car next-subconjunction)))
               (append
                (take acon (add1 first-multi-idx))
                (normalize (drop acon (add1 first-multi-idx))))]
              [else (map multi/annotations->simple-multi acon)]))))
(module+
 test
 (require rackunit)
 (check-equal?
  (normalize α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),filter(g4,a3,a4),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),filter(g4,a3,a4),sift(a4,a5),length(a5,g5)))
 (check-equal?
  (normalize α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),t,{a<1,1,1>=a3},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a4},1),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),{a<1,1,1>=a3},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a4}),sift(a4,a5),length(a5,g5)))
 (check-equal?
  (normalize α(integers(g1,a1),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),t,{a<1,1,1>=a1},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a2},1),filter(g2,a2,a3),filter(g3,a3,a4),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g6,a1,a2),filter(g2,a2,a3),multi(filter(g<3,i,1>,a<3,i,1>,a<3,i,2>),{a<3,1,1>=a3},{a<3,i+1,1>=a<3,i,2>},{g<3,l,1>=g3,a<3,l,2>=a4}),sift(a4,a5),length(a5,g5)))
 (check-equal?
  (normalize α(integers(g1,a1),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),t,{a<1,1,1>=a1},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a2},1),filter(g2,a2,a3),multi(filter(g<2,i,1>,a<2,i,1>,a<2,i,2>),t,{a<2,1,1>=a3},{a<2,i+1,1>=a<2,i,2>},{a<2,l,2>=a4},1),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g6,a1,a2),filter(g2,a2,a3),multi(filter(g<4,i,1>,a<4,i,1>,a<4,i,2>),{a<4,1,1>=a3},{a<4,i+1,1>=a<4,i,2>},{g<4,l,1>=g7,a<4,l,2>=a4}),sift(a4,a5),length(a5,g5)))
 (check-equal?
  (normalize α(integers(g1,a1),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),t,{a<1,1,1>=[g2|a1]},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a2},1),filter(g3,a2,a3),sift(a3,a4),length(a4,g4)))
  α(integers(g1,a1),filter(g5,[g2|a1],a2),multi(filter(g<2,i,1>,a<2,i,1>,a<2,i,2>),{a<2,1,1>=a2},{a<2,i+1,1>=a<2,i,2>},{g<2,l,1>=g3,a<2,l,2>=a3}),sift(a3,a4),length(a4,g4))))