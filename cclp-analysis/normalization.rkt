#lang alpha-gamma racket
(require cclp-common-data/abstract-multi-domain)

(define (normalize acon) acon)

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