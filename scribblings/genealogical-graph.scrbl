#lang scribble/manual
@(require
   (for-label
    racket
    cclp/genealogical-graph
    cclp/genealogical-graph-visualization)
   (only-in racket/file file->string)
   cclp/interaction
   scribble/example
   scribble/extract)

@(define gen-graph-illustration-eval (make-base-eval))
@(gen-graph-illustration-eval
  '(require graph pict pretty-graphs cclp/genealogical-graph cclp/genealogical-graph-visualization cclp/generalize cclp/cluster-visualization cclp/clustering cclp/interaction repeated-application (prefix-in primes: cclp-programs/primes) (prefix-in graph-coloring: cclp-programs/graph-coloring) (prefix-in sameleaves: cclp-programs/sameleaves)))
@(gen-graph-illustration-eval
  '(define primes-15-applications (apply↑ 15 proceed primes:initial-program-analysis)))
@(gen-graph-illustration-eval
  '(define primes-17-applications (apply↑ 2 proceed primes-15-applications)))
@(gen-graph-illustration-eval
  '(define primes-32-applications (apply↑ 15 proceed primes-17-applications)))
@(gen-graph-illustration-eval
  '(define primes-37-applications (apply↑ 5 proceed primes-32-applications)))
@(gen-graph-illustration-eval
  '(define primes-68-applications (apply↑ 31 proceed primes-37-applications)))
@(gen-graph-illustration-eval
  '(define graphcol-25-applications (apply↑ 25 proceed graph-coloring:initial-program-analysis)))
@(gen-graph-illustration-eval
  '(define graphcol-29-applications (apply↑ 4 proceed graphcol-25-applications)))
@(gen-graph-illustration-eval
  '(define graphcol-35-applications (apply↑ 6 proceed graphcol-29-applications)))
@(gen-graph-illustration-eval
  '(define graphcol-43-applications (apply↑ 8 proceed graphcol-35-applications)))
@(gen-graph-illustration-eval
  '(define graphcol-44-applications (proceed graphcol-43-applications)))
@(gen-graph-illustration-eval
  '(define sameleaves-48-applications (apply↑ 48 proceed sameleaves:initial-program-analysis)))
@(gen-graph-illustration-eval
  '(define sameleaves-49-applications (proceed sameleaves-48-applications)))

@title{Recursion analysis using a genealogical graph}
@defmodule[cclp/genealogical-graph]

A genealogical graph is an auxiliary data structure used to detect similarity between abstract conjunctions, where one is not simply a renaming or an instance of the other. If conjunctions display this similarity appear during a computation, a multi abstraction is required in order to complete the program analysis.

@margin-note{It is easy to overlook this aspect, as many genealogical graphs are simply trees, which can lead to confusion.}
@emph{A genealogical graph is derived from a single branch of the abstract analysis tree.}
The genealogical graph not represent the path from one conjunction to another (as the analysis tree does), but rather the origin of every atom in a conjunction. The two perspectives on how to number generations in the graph will make this clearer.

@section{Graph skeleton}
Consider the following abstract analysis of the sieve of Eratosthenes, in which we try to move integers through the filters as soon as possible using a non-standard selection rule.
@codeblock{@file->string["example-programs/primes.rkt"]}

Consider the tree branch on which we find the abstract conjunctions (modulo variable renaming): @itemlist{
 @item{integers(g1,a1), filter(g2,a1,a2), sift(a2,a3), length(a3,g3)}@item{integers(g1,a1), filter(g2,a1,a2), filter(g3,a2,a3), sift(a3,a4), length(a4,g4)}@item{integers(g1,a1), filter(g2,a1,a2), filter(g3,a2,a3), filter(g4,a3,a4), sift(a4,a5), length(a5,g5)}
}

The analyses of these three conjunctions essentially consist of the same steps. Some of those steps are simply repeated more often in the longer conjunctions. Nonetheless, the fact that an unbounded number of "similar, but not equivalent" conjunctions appears means that the analysis does not terminate.

By construction of our abstract domain, as well as our use of depth-k abstraction, the number of different abstract atoms (modulo renaming) that can appear during program analysis is finite. This means that a program analysis can only be non-terminating if the length of abstract conjunctions is not finitely bounded.

TODO make sure to point out RDAG.

@section{Top-down computation}
TODO

@section{Top-down bottom-up computation}
@subsection{Simplified perspective without multi}
A purely top-down procedure for assigning generations is extremely slow. A much faster approach is possible. This approach is a combination of top-down and bottom-up traversal of the generational graph skeleton. It does not assign a generation to every node in the graph, but only to the nodes at the bottom level. This is done by considering common ancestors. Consider the following genealogical graph skeleton:

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (dag->pict
           (analysis->current-genealogical-graph-skeleton
            primes-15-applications)
           gen-node->pict)]

Consider node 34. Its only shared ancestor with nodes 35 through 38 is the top-level atom, which is entirely non-recursive. Therefore, we can conclude that the generation assigned to node 34 should be @racket[(gen 0 #f)]. The same reasoning applies to node 38, which has no shared ancestors (other than the top-level atom) with nodes 34 through 37.

Nodes 35 through 37, however, do share a common ancestor, i.e. node 10. Furthermore, this common ancestor has an equivalent descendant whose shared variables occur in an identical position. Therefore, these nodes are said to be of the first generation of the family of node 10.

Next, consider the following skeleton, obtained by further unfolding:

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (dag->pict
           (analysis->current-genealogical-graph-skeleton
            primes-17-applications)
           gen-node->pict)]

Here, nodes 44 through 46 share an ancestor. However, they belong to different generations: node 44 belongs to the first generation, as it was obtained by unfolding node 10. Nodes 45 and 46 were obtained by unfolding a descendant of node 10, equivalent with node 10 and with shared arguments in the same position.

A fast way to find shared ancestors is by using prime encodings: each node is assigned a number which encodes its sequence of ancestors. If we disregard the multi abstraction (so that the generational graph skeleton is always a tree, not a rooted DAG), this is done as follows:
@itemlist{@item{The top-level atom is encoded as @racket[2], the first prime.}@item{The first second-level atom is encoded as @racket[6], i.e. @racket[(* 2 3)], the product of its parent's encoding and the next prime.}@item{The second second-level atom is encoded as @racket[10], etc.}
}

Now, to find the most recent common ancestor of a set of nodes, we need only compute their greatest common divisor. This permits us to easily cluster abstract atoms by origin: First, all abstract atoms are joined in a single, cluster representing all descendants of the top-level atom. Then, smaller clusters are computed by grouping abstract atoms whose GCD is greater than that for all atoms in the cluster. This process is applied repeatedly until each abstract atom makes up its own singleton cluster.@margin-note{We could also forego the use of prime encodings and simply associate a list of ID's with each node. However, execution speed was a concern in developing this technique and our estimate was the the encoding would be faster.}

For the previous example, this yields the following clustering:
@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                        (clustered-lvl/info
                         (active-branch
                          (analysis-tree
                           primes-17-applications)))])
            (cluster->pict clustered encoding->id id->conjunct))]

These clusterings make it possible to define a notion of depth in the aforementiond "similar, but not equivalent" abstract conjunctions. The rules for establishing this depth are as follows:

@emph{If an abstract atom, which is the shared ancestor of at least two subclusters, is renamed by the shared ancestor of one of its subclusters, it is a "target atom". Its unfoldings have a generation number which is one higher.}
The following example shows how a non-zero generation is assigned for the first time in the graph coloring example.
The generation increment is due to the fact that node 64 is a renaming of node 37, such that all common abstract variables occur in the exact same position.
@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            graphcol-25-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

Note that the next generation increment occurs as soon as said renaming is unfolded. It does not immediately need to produce another renaming. That is, in the following example, node 82 does not rename node 77.
@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            graphcol-29-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            graphcol-35-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

Once there are at least two sequential generations which are identical (modulo renaming), these generations can be grouped using the multi abstraction.
Thus, the following clustering can be used to apply generalization:

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            graphcol-43-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

The same applies here:
@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            primes-32-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

It also applies to the sameleaves problem. Note, however, that the syntactic order is reversed with respect to the "age" of the atoms.

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            sameleaves-48-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

@subsection{With multi}
The introduction of the multi abstraction raises several new issues. A multi abstraction represents conjunctions of different generations. Hence, it cannot be assigned a single generation. It can, however, be assigned a range of generations representing the generations of the abstracted conjunctions, say @racket[g1] through @racket[g2], where @racket[g1] may be either higher or lower than @racket[g1]. Whichever generation is greater must be represented symbolically.

This also entails the introduction of "symbolic sums". If, say, generations 1 through 2 are generalized to a range from 1 through l, atoms corresponding to those in generation 3 should have corresponding atoms of generation l+1.

For the grap coloring problem, @racket[g1] is lower than @racket[g2].

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            graphcol-44-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

For sameleaves, @racket[g1] is higher than @racket[g2].
@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            sameleaves-49-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

Ranges become single generations again when the "case:one" unfold of multi is applied.
Note that multi occurs as a GCD even when it has only one descendant.
This is intentional, to avoid ambiguity when annotating the result of one or more case:many unfoldings followed by a case:one unfolding. (notes: 11/10)

@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            primes-37-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]

TODO: point out sibling multis
@examples[#:eval gen-graph-illustration-eval
          #:result-only
          (let*-values ([(clustered _2 _3 _4 _5 _6 encoding->id id->conjunct)
                         (clustered-lvl/info
                          (active-branch
                           (analysis-tree
                            primes-68-applications)))]
                        [(annotated-clustering) (car (annotate-cluster encoding->id id->conjunct clustered))])
            (cluster->pict annotated-clustering encoding->id id->conjunct))]



@section{API}
@(include-extracted cclp/genealogical-graph)
@(include-extracted cclp/clustering)
