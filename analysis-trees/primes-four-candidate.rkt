#lang reader "../at.rkt"
(plus(g2,g3,a3),integers(a3,a4),sift([g2|a4],a1),len(a1,g1)
 {a2/[g2|a3]} integers(N,[N|I]) :- plus(N,1,M),integers(M,I).)