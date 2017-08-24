#lang scribble/manual
@(require scribble/html/html)
@title{Automated generalization}
Generalization using the multi abstraction is necessary to obtain a closed analysis of certain coroutining programs. So far, we have assumed that some type of "oracle" provides information on which subconjunctions to generalize and when to do so. In our implementation, we use a technique which has not yet been formally proved correct, but which can be used to analyze certain example programs in an automated way. Note that counter-examples for this technique are known (e.g. sieve algorithms where the program clauses are such that different subsequences of filters may be added as building blocks), but it seems plausible that these programs can also be handled if the technique is extended.

The starting point is as follows: it is only necessary to apply generalization if the length of abstract conjunctions which occur during naive program analysis is unbounded. Otherwise, due to the finite nature of the abstract domain and due to the assumption of depth-k abstraction, the number of abstract conjuncts encountered must necessarily be finite (though, if depth-k abstraction is required, the desired control flow may not be compiled correctly). Each resolution step can, at most, increase the number of conjuncts in the conjunction to which it is applied by a bounded number. Therefore, an unbounded number of resolution steps must be able to take place. As the number of clauses in a program is finite and fixed and, as the abstract domain is finite, certain computations must be repeated in an infinite loop during the abstract analysis and performing them must yield a longer conjunction than the one in which they were performed.

This reasoning leads to a specific recursion analysis technique.




















Once the first multi abstraction is introduced based on the analysis outlined above, a new issue arises: multi abstractions are not the result of resolution. Therefore, they cannot be assigned generations in the usual way, by looking at the conjuncts from which they were derived. However, they need a similar construct if they are to be combined with new building blocks which either precede or follow them syntactically and which share the same relevant target atom as an origin. Therefore, multi abstractions are assigned a generation range, which captures the generations of all the building blocks.

Again, this raises an issue.
...
Therefore, generation ranges must be at least partly symbolic.

Once generations have been assigned, generalization is applied by folding over the bottom level of the genealogical graph, considering each node at this level in an order which reflects the syntactic order of the atoms contained by these nodes. This folding operation requires the following three data structures:

@itemlist{
 @item{A completed list of nodes. This is only ever extended and never modified in any other way.}
 @item{An optional list of nodes that make up a potential abstraction. This contains either atoms of a generation which directly precedes or follows the atoms in the current generation being processed, or a single multi abstraction to which the current generation being processed could still be added depending on the nodes that follow. @strong{We only check whether the current generation being processed can still be added to an existing potential abstraction once the current generation ends, so the potential abstraction may in some cases be a list with a multi that cannot represent the current generation being processed.}}
}
}