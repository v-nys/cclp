#lang scribble/manual
@(require (for-label racket))@;so that racket bindings are hyperlinked
@(require scribble/extract)

@title{Abstract analysis}
Abstract analysis encompasses the artefacts generated during the analysis phase of Compiling Control,
except for the tree data structure, which is generic.
The most common and largest of these is @racket[tree-label].

@section{API}
@(include-extracted "../src/abstract-analysis.rkt")