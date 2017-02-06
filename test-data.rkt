#lang at-exp racket
(require graph
         "abstract-multi-domain.rkt"
         "cclp-interpreter.rkt"
         "concrete-domain.rkt"
         "concrete-knowledge.rkt"
         "preprior-graph.rkt")

(require scribble/srcdoc)
(require (for-doc scribble/manual))

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
(provide primes-clauses)

(define primes-full-evals
  (interpret-full-eval-section
   #<<HERE
plus(γ1,γ2,α1) -> α1/γ3.
minus(γ1,γ2,α1) -> α1/γ3.
divides(γ1,γ2).
does_not_divide(γ1,γ2).
HERE
   ))
(provide primes-full-evals)

(define primes-consts (list (function 'nil (list))))
(provide primes-consts)

(define permsort-clauses
  (map interpret-concrete-rule
       (string-split
        #<<HERE
sort(X,Y) :- perm(X,Y),ord(Y)
perm([],[])
perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V)
ord([])
ord([X])
ord([X,Y|Z]) :- lte(X,Y),ord([Y|Z])
HERE
        "\n")))
(provide permsort-clauses)

(define permsort-full-evals
  (interpret-full-eval-section
   #<<HERE
lte(γ1,γ2).
del(α1,[γ1|γ2],α2) -> α1/γ3,α2/γ4.
HERE
   ))
(provide permsort-full-evals)

(define permsort-consts (list (function 'nil (list))))
(provide permsort-consts)

(define permsort-prior (mk-preprior-graph))
(begin
  )
(provide permsort-prior)