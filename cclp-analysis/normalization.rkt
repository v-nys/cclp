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

(define (normalize acon)
        (define first-multi (findf multi? acon))
        (define first-multi-idx (index-where multi? acon))
        (define all-multis (filter multi? acon))
        (define fresh-id (add1 (apply max (map get-multi-id all-multis))))
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
                (> (length acon) (+ first-multi-idx (length (multi-conjunction first-multi))))
                (take (drop acon (add1 first-multi-idx)) (length (multi-conjunction first-multi))))]
              [(cons case-one subst)
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
                (assemble-var-indices (const #t) case-one))]
              [(unifiable-instance)
               (offset-vars subscriptless-instance offset offset)]
              [(unification)
               (and
                next-subconjunction
                (>=-extension unifiable-instance next-subconjunction)
                ; additional offset 0 is okay here:
                ; won't unify g with something containing an a, because of >=-extension
                (some-v (abstract-unify (list (abstract-equality unifiable-instance next-subconjunction)) 0)))]
              [(context) (and first-multi (append (take subbed (+ first-multi-idx (length case-one))) (drop subbed (+ first-multi-idx (* 2 (length case-one))))))]
              [(context-vars) (extract-variables context)]
              [(new-init)
               (map
                (λ (v) (cons (prefix-subscripts fresh-id 1 v) v))
                (filter
                 (λ (v) (member v context-vars))
                 (extract-variables subbed)))]
              ; just new-init, but with 'L instead of 1
              [(new-final)
               (map
                (λ (v) (cons (prefix-subscripts fresh-id 'L v) v))
                (filter
                 (λ (v) (member v context-vars))
                 (extract-variables subbed)))]
              [(rewrapped)
               (and
                next-subconjunction
                ; TODO: new simple multi
                (simple-multi (list) new-init (list) new-final))])
             (cond
              [(or (not next-subconjunction)
                   (ormap multi? (cdr next-subconjunction)))
               (append
                (take acon (add1 first-multi-idx))
                (normalize (drop acon (add1 first-multi-idx))))]
              [else acon]))))
;
;              ; TODO: conditions and recursive calls
;              [(and rewrapped)] ; a: rewrapping is possible and the result is equivalent with acon: keep normalizing
;                                     [rewrapped] ; b the result is not equivalent: only the part of acon after the multi is eligible for further normalization
;                                                      [(and (not rewrapped))] ; c) rewrapping is not possible, as the first conjunct in the following subconjunction is also a multi: another case split (c1 and c2)
;                                                                                   )
;
;; c1) unfold:one does affect denotation -> apply unfold:one and keep normalizing
;; c2) unfold:one affects denotation -> do not apply unfold:one and keep normalizing

(module+
 test
 (require rackunit)
 (check-equal?
  (normalize α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),filter(g4,a3,a4),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),filter(g4,a3,a4),sift(a4,a5),length(a5,g5)))
 (check-equal?
  (normalize α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),t,{a<1,1,1>=a3},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a4},1),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g2,a1,a2),filter(g3,a2,a3),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),{a<1,1,1>=a3},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a4}),sift(a4,a5),length(a5,g5)))
 ;; TODO: indices after normalization may differ
 (check-equal?
  (normalize α(integers(g1,a1),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),t,{a<1,1,1>=a1},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a2},1),filter(g2,a2,a3),filter(g3,a3,a4),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g6,a1,a2),filter(g3,a2,a3),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),{a<1,1,1>=a3},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a4}),sift(a4,a5),length(a5,g5)))
 ;; TODO: indices after normalization may differ
 (check-equal?
  (normalize α(integers(g1,a1),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),t,{a<1,1,1>=a1},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a2},1),filter(g2,a2,a3),multi(filter(g<2,i,1>,a<2,i,1>,a<2,i,2>),t,{a<2,1,1>=a3},{a<1,i+1,1>=a<1,i,2>},{a<2,l,2>=a4},1),sift(a4,a5),length(a5,g5)))
  α(integers(g1,a1),filter(g6,a1,a2),filter(g3,a2,a3),multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),{a<1,1,1>=a3},{a<1,i+1,1>=a<1,i,2>},{a<1,l,2>=a4}),sift(a4,a5),length(a5,g5))))