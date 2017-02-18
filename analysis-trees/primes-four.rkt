#lang reader "../at.rkt"
(.1.*primes(g1,a1)*
 (.2.*integers(g2,a6)*,sift(a6,a5),length(a5,g1) [integers(g1,a1) < sift(a1,a2), integers(g1,a1) < length(a1,g1)]
 {a4/g1, a1/a5} primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N).
 (.3.*sift([],a5)*,length(a5,g1) [sift([],a1) < length(a1,g1)]
  {a7/g2, a6/[]} integers(N,[]).
  (.4.*length([],g1)*
   {a5/[]} sift([],[]).
   (.□
   {g1/g2} length([],0).)))
 (.plus(g2,g3,a9),integers(a9,a8),sift([g2|a8],a5),length(a5,g1)
  {a7/g2, a6/[g2|a8]} integers(N,[N|I]) :- plus(N,1,M),integers(M,I).)))