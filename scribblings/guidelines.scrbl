#lang scribble/manual
@title{Guidelines for the CCLP codebase}
The following are conventions I try to stick to for the CCLP code base.
I have not always been consistent, which is part of the reason why I'm writing them down.
They may contradict some more common Racket guidelines that I am not aware of.

@section{Ordering @racket[require] forms}
Forms that involve @racket[require] are organized as follows:
@itemlist[
 @item{By phase: expansion before runtime before documentation time}
 @item{By origin: libraries included in the Racket distribution, then external dependencies, then files in the project}
 @item{Alphabetically, by collection}]

All @racket[require] forms relating to the same phase are grouped in a single @racket[require] form.