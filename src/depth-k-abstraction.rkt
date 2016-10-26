#lang at-exp racket
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(require "abstract-multi-domain.rkt")
(require (only-in "abstraction-inspection-utils.rkt" maximum-var-index))

(define (abstract elem k)
; TODO take into account maybe...
  (define fresh-g (+ (maximum-var-index elem g?) 1))
  (define fresh-a (+ (maximum-var-index elem a?) 1))
  elem)
(provide
 (proc-doc/names
  abstract
  (-> abstract-domain-elem? exact-positive-integer? abstract-domain-elem?)
  (abstract-domain-element k)
  @{Return the most specific abstraction of @racket[elem] whose depth is at most @racket[k].
 Depth is defined as follows: for abstract variables and constants in the abstract domain, it is 1.
 For function terms, it is the depth of the argument with the greatest depth, increased by one.
 For abstract atoms and conjunctions, it is identical to the depth of the argument with the greatest depth.
 This definition is based on the assumption that we only use first-order logic and thus cannot nest atoms.}))