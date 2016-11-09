#lang at-exp racket

; cannot define these functions in the domain module, would cause circular import between domain module and unification module
(require "abstract-multi-domain.rkt")
(require "abstract-unify.rkt")
(require "data-utils.rkt")
(require "abstract-substitution.rkt")
(require "abstract-renaming.rkt")

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (>=-extension domain-elem1 domain-elem2)
  (let* ([renamed-domain-elem2
          (rename-apart domain-elem2 domain-elem1)]
         [unifier
          (abstract-unify
           (list (abstract-equality domain-elem1 renamed-domain-elem2)) 0)])
    (begin
      (and
       (some? unifier)
       (equal?
        (apply-substitution (some-v unifier) domain-elem1)
        renamed-domain-elem2)))))
(provide
 (proc-doc/names
  >=-extension
  (-> abstract-domain-elem? abstract-domain-elem? boolean?)
  (domain-elem1 domain-elem2)
  @{Checks whether @racket[domain-elem1] is at least as general as @racket[domain-elem2].}))

(define (renames? domain-elem1 domain-elem2)
  (and (>=-extension domain-elem1 domain-elem2)
       (>=-extension domain-elem2 domain-elem1)))
(provide
 (proc-doc/names
  renames?
  (-> abstract-domain-elem? abstract-domain-elem? boolean?)
  (domain-elem1 domain-elem2)
  @{Checks whether @racket[domain-elem1] is equivalent to @racket[domain-elem2].}))