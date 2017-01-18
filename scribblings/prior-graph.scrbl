#lang scribble/manual

@title{Prior}
A graph constructed using @racket{mk-preprior-graph} makes it possible to apply
the additional contraints imposed by the (strict) partial order relation.

The proof is as follows:

Assume we add each atom which is compared to other atoms to a directed graph G.
Assume that, if an atom A is preferred over a non-equivalent atom B,
an edge from A to B is added.
Assume that edges from A to an equivalent atom B are never added.
(If A and B are equivalent, a syntax-based rule determines which atom is preferred.)
Assume that, if an atom A is added to G, and there are atoms B which generalize A,
edges from A to each B are added.
Finally, assume that, if an atom A is added to G, and there are atoms B which specify A,
an edge is added from each A to B.

Then, the transitive closure G' of G is not a strict partial order
if and only if G contains a loop.

A strict partial order is irreflexive, transitive and asymmetric.

Now, from right to left:
- if there is a loop from some atom A to A in G, irreflexivity does not hold in G'.
- if there is a loop from some atom A to another atom B in G, asymmetry does not hold in G'.
Therefore, any kind of loop in G means that G' is not a strict partial order.

From left to right:
- we know that transitivity holds, because we are considering the transitive closure of G.
- if G' is not a strict partial order, it is either not irreflexive, or it is not asymmetric.
- if G' is not irreflexive, there is a loop from some atom A to itself in G'.
  the transitive closure (i.e. reachability relation) cannot contain a self-loop if there is no loop in G.
- if G' is not asymmetric, then there is a loop between at least two atoms in G'.
  then, G' is also not reflexive.
  see previous bullet.