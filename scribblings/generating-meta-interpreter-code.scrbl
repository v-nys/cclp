#lang scribble/manual
@(require scribble/extract)

@title{Generating code for the concrete meta-interpreter.}
In addition to a source program, the meta-interpreter in the companion Prolog code needs a specification of the compiled selection rule in the form of @code{precedence/2} clauses, as well as @code{generalization/2} clauses which it can use to wrap certain concrete atoms inside a concrete multi abstraction. Both can be generated when an interactive abstract analysis has been performed.

@include-extracted["../cclp-analysis/mi-map.rkt"]