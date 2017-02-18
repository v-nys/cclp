#lang reader "../at.rkt"
(.5.*plus(g2,g3,a9)*,integers(a9,a8),sift([g2|a8],a5),length(a5,g1)
  {a7/g2, a6/[g2|a8]} integers(N,[N|I]) :- plus(N,1,M),integers(M,I).
  (.integers(g6,a8),sift([g4|a8],a5),length(a5,g1)
  {g2/g4, g3/g5, a9/g6} plus(g1,g2,a1) -> {a1/g3}))