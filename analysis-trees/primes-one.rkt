#lang reader "../at.rkt"
(.1.*primes(g1,a1)*
 (.integers(g2,a6),sift(a6,a5),length(a5,g1)
  {a4/g1, a1/a5} primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N).))