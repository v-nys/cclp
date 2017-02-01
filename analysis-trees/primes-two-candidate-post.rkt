#lang reader "../at.rkt"
(3.*sift([],a1)*,len(a1,g1) [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1)]
 {a2/[]} integers(N,[]).
 (len([],g1) [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1)]
  {} sift([],[]).))