#lang scribble/manual
@(require
   (for-label
    racket
    cclp/genealogical-graph
    cclp/genealogical-graph-visualization)
   scribble/example
   scribble/extract)

@(define gen-graph-illustration-eval (make-base-eval))
@(gen-graph-illustration-eval '(require graph pict pretty-graphs cclp/genealogical-graph-visualization cclp/analysis-trees/optimus-primes-branch-gen-graph))

@title{Recursion analysis using a genealogical graph}
@defmodule[cclp/genealogical-graph]

A genealogical graph is an auxiliary data structure used to detect similarity between abstract conjunctions, where one is not simply a renaming or an instance of the other. If conjunctions display this similarity appear during a computation, a multi abstraction is required in order to complete the program analysis.

@margin-note{It is easy to overlook this aspect, as many genealogical graphs are simply trees, which can lead to confusion.}
@emph{A genealogical graph is derived from a single branch of the abstract analysis tree.}
The genealogical graph not represent the path from one conjunction to another (as the analysis tree does), but rather the origin of every atom in a conjunction. The top-down perspective will make this clearer, but a direct implementation is inefficient.

@section{Top-down perspective}
Consider the following abstract analysis of the sieve of Eratosthenes, in which we try to move integers through the filters as soon as possible using a non-standard selection rule.
@codeblock|{
#lang cclp
{PROGRAM}
primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N).

integers(N,[]).
integers(N,[N|I]) :- plus(N,1,M),integers(M,I).
            
sift([N|Ints],[N|Primes]) :- filter(N,Ints,F),sift(F,Primes).
sift([],[]).
               
filter(N,[M|I],F) :- divides(N,M), filter(N,I,F).
filter(N,[M|I],[M|F]) :- does_not_divide(N,M), filter(N,I,F).
filter(N,[],[]).

length([],0).
length([H|T],N) :- minus(N,1,M),length(T,M).

{FULL EVALUATION}
plus(γ1,γ2,α1) -> α1/γ3.
minus(γ1,γ2,α1) -> α1/γ3.
divides(γ1,γ2).
does_not_divide(γ1,γ2).

{CONCRETE CONSTANTS}
nil

{QUERY}
primes(γ1,α1)
}|

@margin-note{Optionally include the partial order in the file.}
Consider a tree branch on which we find the abstract conjunctions (modulo variable renaming): @itemlist{
 @item{integers(g1,a1), filter(g2,a1,a2), sift(a2,a3), length(a3,g3)}@item{integers(g1,a1), filter(g2,a1,a2), filter(g3,a2,a3), sift(a3,a4), length(a4,g4)}@item{integers(g1,a1), filter(g2,a1,a2), filter(g3,a2,a3), filter(g4,a3,a4), sift(a4,a5), length(a5,g5)}
}

The analyses of these three conjunctions essentially consist of the same steps. Some of those steps are simply repeated more often in the longer conjunctions. Nonetheless, the fact that an unbounded number of "similar, but not equivalent" conjunctions appears means that the analysis does not terminate.

By construction of our abstract domain, as well as our use of depth-k abstraction, the number of different abstract atoms (modulo renaming) that can appear during program analysis is finite. This means that a program analysis can only be non-terminating if the length of abstract conjunctions is not finitely bounded.

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (dag->pict
           val
           gen-node->pict)]

@section{Bottom-up computation}
The top-down computation procedure can be derived in a straightforward way from the description of the genealogical graph.

@section{API}
@(include-extracted cclp/genealogical-graph)
