#lang reader "../at.rkt"
(1.*primes(g1,a1)* [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1) & ?]
 (2.*integers(g2,a2)*,sift(a2,a1),len(a1,g1) [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1) & ?]
  {} primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N).
  (3.*sift([],a1)*,len(a1,g1) [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1) & integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1)]
   {a2/[]} integers(N,[]).
   (4.*len([],g1)* [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1) & integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1)]
    {} sift([],[]).
    (□ [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1)]
     {} length([],0).)))
  (plus(g2,g3,a3),integers(a3,a4),sift([g2|a4],a1),len(a1,g1) [integers(g1,a1) > sift(a1,a2), integers(g1,a1) > len(a1,g1), sift([],a1) > len(a1,g1)]
   {a2/[g2|a3]} integers(N,[N|I]) :- plus(N,1,M),integers(M,I).)))