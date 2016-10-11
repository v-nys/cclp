#lang scribble/manual
@(require (for-label racket))@;so that racket bindings are hyperlinked

@title{The CCLP language}

The CCLP language is a language for the implementation of pure logic programs which rely on instantiation-based coroutining.
A program written in the CCLP language can be executed to generate --- at the current time --- an abstract interpretation of a pure logic program.
From this abstract interpretation, a logic program which does not require special coroutining constructs but which does have the desired control flow can be derived.


@section{A simple example}
@verbatim{
#lang reader "cclp-reader.rkt"
{PROGRAM}
sort(X,Y) :- perm(X,Y),ord(Y).

perm([],[]).
perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V).

ord([]).
ord([X]).
ord([X,Y|Z]) :- lte(X,Y),ord([Y|Z]).

{FULL EVALUATION}
lte(γ1,γ2).
del(α1,[γ1|γ2],α2) -> α1/γ3,α2/γ4.

{PREPRIOR}
perm(γ1,α1),ord(α1)
perm(γ1,α1),ord([γ1|α1])
ord([γ1,γ2|α1]),perm(γ1,α1)

{QUERY}
sort(γ1,α1)
}

@include-section["interaction.scrbl"]
@include-section["abstract-substitution.scrbl"]

@section{Limitations}

@section{Plans for the future}
The current implementation allows the user to build an abstract analysis tree of the logic program.
From this analysis tree, a concrete Prolog program or CHR program can be synthesized, as described in @;TODO
Once the completeness properties of the abstract analysis have been proven, the goal will be to automate both synthesis procedures, as well.