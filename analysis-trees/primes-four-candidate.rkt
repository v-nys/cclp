#lang cclp/at
(.plus(g2,g3,a9),integers(a9,a8),sift([g2|a8],a5),length(a5,g1)
  {a7/g2, a6/[g2|a8]} integers(N,[N|I]) :- plus(N,1,M),integers(M,I).)