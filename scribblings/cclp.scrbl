#lang scribble/manual
@(require (for-label racket))@;so that racket bindings are hyperlinked

@title{The CCLP language}
@author{Vincent Nys}
@defmodulelang{cclp}

CCLP is a language for the implementation of pure logic programs which rely on instantiation-based coroutining.
A program written in the CCLP language can be executed to generate --- at the current time --- an abstract interpretation of a pure logic program.
From this abstract interpretation, a logic program which does not require special coroutining constructs but which does have the desired control flow can be derived.

@section{A simple example}
@verbatim{
#lang cclp
{PROGRAM}
sort(X,Y) :- perm(X,Y),ord(Y).

perm([],[]).
perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V).

ord([]).
ord([X]).
ord([X,Y|Z]) :- lte(X,Y),ord([Y|Z]).

{FULL EVALUATION}
lte(g1,g2).
del(a1,[g1|g2],a2) -> a1/g3,a2/g4.

{PREPRIOR}
perm(g1,a1),ord(a1)
perm(g1,a1),ord([g1|a1])
ord([g1,g2|a1]),perm(g1,a1)

{QUERY}
sort(g1,a1)
}

@;include-section["interaction.scrbl"]
@;include-section["abstract-substitution.scrbl"]
@;include-section["abstract-domain-ordering.scrbl"]
@include-section["abstract-multi-domain.scrbl"]
@include-section["genealogical-graph.scrbl"]
@;include-section["abstract-analysis.scrbl"]

@section{Limitations}

@section{Plans for the future}
The current implementation allows the user to build an abstract analysis tree of the logic program.
From this analysis tree, a concrete Prolog program or CHR program can be synthesized, as described in @;TODO
Once the completeness properties of the abstract analysis have been proven, the goal will be to automate both synthesis procedures, as well.
