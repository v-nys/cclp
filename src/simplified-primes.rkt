#lang reader "cclp-reader.rkt"
% this is not the full primes program
% it is a test program which generates a single infinite branch
% this branch can be used to check (an initial version) of the implementation of s-similarity

{PROGRAM}
primes(N,Primes) :- integers(2,I),sift(I,Primes),newlength(Primes,N).

integers(N,[N|I]) :- plus(N,1,M), integers(M,I).
sift([N|Ints],[N|Primes]) :- filter(N,Ints,F), sift(F,Primes).
% omitting does_not_divide as I want everything to pass through all the filters for this single infinite branch
filter(N,[M|I],[M|F]) :- filter(N,I,F).
newlength([H|T],N) :- minus(N,1,M), newlength(T,M).

{FULL EVALUATION}
plus(γ1,γ2,α1) -> α1/γ3.
minus(γ1,γ2,α1) -> α1/γ3.