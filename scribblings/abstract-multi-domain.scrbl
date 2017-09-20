#lang scribble/manual
@(require
   (for-label
    racket/base
    cclp/abstract-multi-domain)
   scribble/extract
   scriblib/bibtex
   scriblib/footnote)

@; this has to come before any defmodules or they show up wrong, oddly
@title{Abstract domain, including multi}

@; this is needed to get hyperlinks for the different struct id's
@defmodule[cclp/abstract-multi-domain]

@(define-bibtex-cite @(path->string (simplify-path (build-path (syntax-source #'here) "../bibliography.bib"))) ~cite citet generate-bibliography)

The abstract-multi domain module encodes the elements described in Definition 20 in @citet["nys2017abstract"]. Several predicate functions are provided, e.g. to check whether a particular value is in the abstract-multi domain or some subdomain thereof. Some inspection functions are also provided for convenient access.

@section{Caveats}
@subsection{Parameterized namings}
Note that @racket[abstract-atom*], @racket[abstract-function*], @racket[a*] and @racket[g*] are separate constructors here. That is, parameterized namings (see Definition 18 in @citet["nys2017abstract"]) of domain elements are encoded using different struct types than their non-parameterized counterparts.

@subsection{Extra information in the @racket[multi] struct type}
At the time of writing, the @racket[multi] struct type contains an accessor which is not mentioned in @citet["nys2017abstract"], i.e. @racket[multi-ascending?]. The field accessed by this function indicates whether the represented abstract conjunction consists of conjunctions which, roughly, increase in length on the right (if @racket[multi-ascending?] returns @racket[#t]) or on the left (if @racket[multi-ascending?] returns @racket[#f]). For more details, see the description of the genealogical tree data structure. @note{This description has not actually been written yet.}. The field may still be removed in the future, as it is not actually a property of the abstraction itself, but an extra piece of information which helps to correctly apply the abstraction and which could be inferred.

Also, the @racket[init], @racket[consecutive] and @racket[final] structs may appear redundant. This is, in fact, true. They will be removed in a future version of the code.

@section{API}
@(include-extracted cclp/abstract-multi-domain)

@(generate-bibliography)