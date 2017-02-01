#lang reader "../at.rkt"
(1.*primes(g1,a1)*
 (integers(g2,a2),sift(a2,a1),len(a1,g1)
  {} primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N).))