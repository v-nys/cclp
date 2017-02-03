#lang scribble/manual
@require[scribble-math]
@; still need to figure out how to render math properly

@title{Abstract analysis tree}
The purpose of the abstract analysis tree module is to represent the abstract analysis phase for compiling control.

@section{Design decisions}
None of the publications thus far have pointed out why, if there is a loop, a predecessor must be @emph{equivalent}, rather than more or equally general.
Consider an analysis of a variant of the sameleaves problem.
Suppose the top-level query is @$["sameleaves(a_1,a_2)"].
Suppose there is a clause @$["sameleaves(X,Y) :- X \\= g_1, Y \\= g_2, sameleaves(g_1,g_2)"], where @$["g_1"] and @$["g_2"] are concrete, ground trees.
Then, the resolvent @$["sameleaves(g_1,g_2)"] should not cycle back to its parent.
This would have the same effect as premature widening, i.e. it would hide part of the control flow from the analysis and, thus, from the transformation.