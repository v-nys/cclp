#lang scribble/manual
@(require (for-label racket syntax/parse))
@title{Syntax parsing}
@section{Dealing with optional parts of a grammar}
@subsection{Problem description}
A program in the CCLP language consists of two required sections (a logic program and a top-level query), three optional components: abstract substitutions for fully evaluated calls, concrete constants included in the abstract domain and a partial order on the abstract atoms that will appear during the analysis. Originally, I did this with 8 clauses in @racket[syntax-parse].  Clearly, this is not scalable.

@subsection{Solution outline}
The basic idea was to replace the different clauses with just this:
@racketblock[
 (define-syntax (cclp-program stx)
   (syntax-parse stx
     [(_ "{PROGRAM}" _PROGRAM-SECTION
         OPTIONAL-SECTION ...
         "{QUERY}" _QUERY-SECTION)
      (with-syntax ([??? (optional-cclp-sections OPTIONAL-SECTION ...)])
        (syntax/loc stx
          (cclp
           _PROGRAM-SECTION
           _FULL-EVALUATION-SECTION
           _CONCRETE-CONSTANTS-SECTION
           _PARTIAL-ORDER-SECTION
           _QUERY-SECTION)))]))]

@subsection{Challenge}
This is where I drew a blank. I wanted to be able to parse the optional program components in a linear way, i.e. with one macro or function per component. Each component is identified by a string which precedes it, e.g. @racket["{CONCRETE CONSTANTS}"]. If the expected string is missing, we know that the component takes a default value.

My first attempt (which contains several mistakes!) was the following:
@racketblock[
 (define-syntax (cclp-program stx)
   (syntax-parse stx
     [(_ "{PROGRAM}" _PROGRAM-SECTION
         OPTIONAL-SECTION ...
         "{QUERY}" _QUERY-SECTION)
      (with-syntax ([(_FULL-EVALUATION-SECTION
                      _CONCRETE-CONSTANTS-SECTION
                      _PARTIAL-ORDER-SECTION)
                     (syntax->list
                      (optional-cclp-sections/full-evaluation
                       OPTIONAL-SECTION ...))])
        (syntax/loc stx
          (cclp
           _PROGRAM-SECTION
           _FULL-EVALUATION-SECTION
           _CONCRETE-CONSTANTS-SECTION
           _PARTIAL-ORDER-SECTION
           _QUERY-SECTION)))]))

 (define-syntax (optional-cclp-sections/full-evaluation stx)
   (syntax-parse stx
     [(_ "{FULL EVALUATION}" _FULL-EVALUATION-SECTION REST ...)
      (with-syntax ([(_CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)
                     (optional-cclp-sections/concrete-constants REST ...)])
        (syntax/loc stx
          (_FULL-EVALUATION-SECTION
           _CONCRETE-CONSTANTS-SECTION
           _PARTIAL-ORDER-SECTION)))]
     [(_ REST ...)
      (with-syntax ([(_CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)
                     (optional-cclp-sections/concrete-constants REST ...)])
        (syntax/loc stx
          ((list) _CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)))]))

 (define-syntax (optional-cclp-sections/concrete-constants stx)
   (syntax-parse stx
     [(_ "{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION REST ...)
      (with-syntax ([_PARTIAL-ORDER-SECTION
                     (optional-cclp-sections/partial-order REST ...)])
        (syntax/loc stx (_CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)))]
     [(_ REST ...)
      (with-syntax ([_PARTIAL-ORDER-SECTION
                     (optional-cclp-sections/partial-order REST ...)])
        (syntax/loc stx ((list) _PARTIAL-ORDER-SECTION)))]))

 (define-syntax (optional-cclp-sections/concrete-constants stx)
   (syntax-parse stx
     [(_ "{PARTIAL ORDER}" _PARTIAL-ORDER-SECTION)
      (syntax/loc stx _PARTIAL-ORDER-SECTION)]
     [(_)
      (syntax/loc stx (mk-preprior-graph))]))]

First off, @racket[OPTIONAL-SECTION] cannot occur where it does in @racket[with-syntax].

@bold{Q: Why not?}

@bold{A: Because it is a template variable. It cannot just be used inside other syntax such as @racket[optional-cclp-sections/full-evaluation], it needs to be wrapped explicitly inside @racket[syntax], along with the associated ellipsis.}

With that in mind, @racket[optional-cclp-sections/full-evaluation] does not appear as syntax with the desired arguments.

@bold{Q: How do we fix this?}

@bold{A: By turning @racket[optional-cclp-sections/full-evaluation] into a function which takes a single syntax @emph{object} and produces a syntax object.}

So the corrected version is:

@racketblock[
 (define-syntax (cclp-program stx)
   (syntax-parse stx
     [(_ "{PROGRAM}" _PROGRAM-SECTION
         OPTIONAL-SECTION ...
         "{QUERY}" _QUERY-SECTION)
      (with-syntax ([(_FULL-EVALUATION-SECTION _CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)
                     (syntax->list (optional-cclp-sections/full-evaluation #'(OPTIONAL-SECTION ...)))])
        (syntax/loc
            stx
          (cclp _PROGRAM-SECTION _FULL-EVALUATION-SECTION _CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION _QUERY-SECTION)))]))

 (define-for-syntax (optional-cclp-sections/full-evaluation stx)
   (syntax-parse stx
     [("{FULL EVALUATION}" _FULL-EVALUATION-SECTION REST ...)
      (with-syntax ([(_CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)
                     (optional-cclp-sections/concrete-constants #'(REST ...))])
        (syntax/loc stx (_FULL-EVALUATION-SECTION _CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)))]
     [(REST ...)
      (with-syntax ([(_CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)
                     (optional-cclp-sections/concrete-constants #'(REST ...))])
        (syntax/loc stx ((list) _CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)))]))

 (define-for-syntax (optional-cclp-sections/concrete-constants stx)
   (syntax-parse stx
     [("{CONCRETE CONSTANTS}" _CONCRETE-CONSTANTS-SECTION REST ...)
      (with-syntax ([_PARTIAL-ORDER-SECTION (optional-cclp-sections/partial-order #'(REST ...))])
        (syntax/loc stx (_CONCRETE-CONSTANTS-SECTION _PARTIAL-ORDER-SECTION)))]
     [(REST ...)
      (with-syntax ([_PARTIAL-ORDER-SECTION (optional-cclp-sections/partial-order #'(REST ...))])
        (syntax/loc stx ((list) _PARTIAL-ORDER-SECTION)))]))

 (define-for-syntax (optional-cclp-sections/partial-order stx)
   (syntax-parse stx
     [("{PARTIAL ORDER}" _PARTIAL-ORDER-SECTION)
      (syntax/loc stx _PARTIAL-ORDER-SECTION)]
     [() (syntax/loc stx (mk-preprior-graph))]))]