#lang alpha-gamma racket
(require cclp-common/abstraction-inspection-utils)
(require cclp-common-data/abstract-multi-domain)

(define (normalize acon)
        (define first-multi (findf multi? acon))
        (define-values
         (max-a max-g)
         (values
          (maximum-var-index acon a?)
          (maximum-var-index acon g?)))
        (if (not first-multi)
            acon
            acon))
; DONE: if there are no multis in acon, just return acon
; if there are any multis, replace the first with its case: one unfolding and attempt to wrap the subconjunction of corresponding length that follows
; a) this is possible and the result is equivalent with acon: keep normalizing
; b) this is possible, but the result is not equivalent: only the part of acon after the multi is eligible for further normalization
; c) this is not possible, as the first conjunct in the following subconjunction is also a multi: another case split
; c1) unfold:one does affect denotation -> apply unfold:one and keep normalizing
; c2) unfold:one affects denotation -> do not apply unfold:one and keep normalizing
; d) this is not possible, as another conjunct than the first in the following subconjunction is a multi: only the part of acon after the multi is eligible... (~b)


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