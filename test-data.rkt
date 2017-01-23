#lang racket
(require "cclp-interpreter.rkt")

(define primes-clauses
  (map interpret-concrete-rule
       (string-split
        #<<HERE
primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N)
integers(N,[])
integers(N,[N|I]) :- plus(N,1,M),integers(M,I)
sift([N|Ints],[N|Primes]) :- filter(N,Ints,F),sift(F,Primes)
sift([],[])
filter(N,[M|I],F) :- divides(N,M), filter(N,I,F)
filter(N,[M|I],[M|F]) :- does_not_divide(N,M), filter(N,I,F)
filter(N,[],[])
length([],0)
length([H|T],N) :- minus(N,1,M),length(T,M)
HERE
        "\n")))

(define primes-full-evals
  (interpret-full-eval-section
        #<<HERE
plus(γ1,γ2,α1) -> α1/γ3.
minus(γ1,γ2,α1) -> α1/γ3.
divides(γ1,γ2).
does_not_divide(γ1,γ2).
HERE
        "\n"))

(define primes-consts (list (abstract-function 'nil (list))))