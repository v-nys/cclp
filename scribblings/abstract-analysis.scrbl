#lang scribble/manual
@(require (for-label racket))@;so that racket bindings are hyperlinked
@(require scribble/extract)

@title{Abstract analysis}
Abstract analysis encompasses the artefacts generated during the analysis phase of Compiling Control,
except for the tree data structure, which is generic.
That is, this module contains the node contents for the analysis phase.
The most common and largest of these is @racket[tree-label], i.e. the type of node in which unfolding or full evaluation occurs.

@section{API}
@(include-extracted "../abstract-analysis.rkt")
