#lang scribble/manual
@(require (for-label "../src/prior-graph.rkt"))
@(require (for-label "../src/abstract-renaming.rkt"))

@title{The prior graph}
A graph constructed using @racket[mk-preprior-graph] can be used to maintain
and check the partial order relation used to select atoms for expansion.

Whenever there are multiple candidates for expansion,
the user is expected to add them to the prior graph using @racket[add-vertex!].

When a candidate is added, it is normalized via @racket[normalize-abstract-atom], and the following happens:

@itemlist[@item{A directed edge to any more general instances in the graph is added.}
          @item{A directed edge from any more specific instances in the graph is added.}
          @item{An edge to an equivalent instance is never added.}]

When a candidate is selected manually, an edge from the (normalized) selected candidate to every (normalized) unselected candidate,
unless the unselected candidate is equivalent to the selected one.

Under these rules, the transitive closure @italic{G′} of a @racket[preprior-graph?] @italic{G} is not a strict partial order
if and only if @italic{G} contains a loop.

A strict partial order @italic{<} has the following properties:
@itemlist[@item{irreflexivity: A ≮ A}
          @item{transitivity: A < B ∧ B < C ⇒ A < C}
          @item{asymmetry: A < B ⇒ B ≮ A}]

Now, the equivalence, from right to left:
@itemlist[@item{If there is a loop from some atom @italic{A} to @italic{A} in @italic{G}, irreflexivity does not hold in @italic{G′}.}
          @item{If there is a loop from some atom @italic{A} to another atom @italic{B} in @italic{G}, asymmetry does not hold in @italic{G′}.}]
Therefore, any kind of loop in @italic{G} means that @italic{G′} does not represent a strict partial order.

And the equivalence, from left to right:
@itemlist[@item{Transitivity holds, because we are considering the transitive closure of @italic{G}.}
          @item{If @italic{G′} is not a strict partial order, it is either not irreflexive, or it is not asymmetric.}
          @item{If @italic{G′} is not irreflexive, there is a loop from some atom @italic{A} to itself in @italic{G′}.
           The transitive closure (i.e. reachability relation) cannot contain a self-loop if there is no loop in @italic{G}.}
          @item{If @italic{G′} is not asymmetric, then there is a loop between at least two atoms in @italic{G′}.
           Then, @italic{G′} is also not reflexive.
           See the previous bullet.}]