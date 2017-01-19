#lang scribble/manual
@(require (for-label racket))@;so that racket bindings are hyperlinked
@(require scribble/extract)

@title{Generational trees}
A generational tree is an auxiliary data structure used to detect similarity between abstract conjunctions,
where one is not simply a renaming or an instance of the other.

@section{API}
@(include-extracted "../generational-tree.rkt")
