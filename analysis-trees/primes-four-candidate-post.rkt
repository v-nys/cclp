#lang reader "../at.rkt"
(5.*plus(g2,g3,a3)*,integers(a3,a4),sift([g2|a4],a1),len(a1,g1)
 {a2/[g2|a3]} integers(N,[N|I]) :- plus(N,1,M),integers(M,I).
 (integers(g43,a4),sift([g2|a4],a1),len(a1,g1)
  {} plus(g1,g2,a1) -> {a1/g3}.))