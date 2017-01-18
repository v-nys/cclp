#lang at-exp racket

; cannot define these functions in the domain module, would cause circular import between domain module and unification module
(require "abstract-multi-domain.rkt")
(require "abstract-unify.rkt")
(require "data-utils.rkt")
(require "abstract-substitution.rkt")
(require "abstract-renaming.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/manual))
(require (only-in "depth-k-abstraction.rkt" max-depth))
(module+ test
  (require rackunit)
  (require "cclp-interpreter.rkt"))

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
(module+ test
  (check-true (>=-extension (interpret-abstract-term "α1") (interpret-abstract-term "γ1")))
  (check-true (>=-extension (interpret-abstract-term "α1") (interpret-abstract-term "α2")))
  (check-false (>=-extension (interpret-abstract-term "γ1") (interpret-abstract-term "α1")))
  (check-true (>=-extension (interpret-abstract-term "γ1") (interpret-abstract-term "γ1")))
  (check-true (>=-extension (interpret-abstract-term "nil") (interpret-abstract-term "nil")))
  (check-false (>=-extension (interpret-abstract-term "nil") (interpret-abstract-term "nonnil")))
  (check-true
   (>=-extension
    (interpret-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
    (interpret-abstract-term "foo(nil,bar(nonnil,γ3,nil))")))
  (check-false
   (>=-extension
    (interpret-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
    (interpret-abstract-term "foo(nil,bar(nonnil,γ3,nonnil))")))
  (check-true
   (>=-extension
    (interpret-abstract-atom "foo(γ1,bar(γ2,α1,γ1))")
    (interpret-abstract-atom "foo(nil,bar(nonnil,γ3,nil))")))
  (check-false
   (>=-extension
    (interpret-abstract-atom "foo(γ1,bar(γ2,α1,γ1))")
    (interpret-abstract-atom "foo(nil,bar(nonnil,γ3,nonnil))")))
  (check-false
   (>=-extension
    (interpret-abstract-term "foo(γ1,bar(γ2,α1,γ1))")
    (interpret-abstract-atom "foo(nil,bar(nonnil,γ3,nil))")))
  (check-true
   (>=-extension
    (interpret-abstract-atom "sift([γ1|α1],α2)")
    (interpret-abstract-atom "sift([γ2|α4],α1)"))) ; renaming should be implicit
  (check-true
   (>=-extension
    (list (interpret-abstract-atom "sift([γ1|α1],α2)"))
    (list (interpret-abstract-atom "sift([γ2|α4],α1)"))))
  (check-false
   (>=-extension
    (list (interpret-abstract-atom "sift([γ1|γ2],α2)"))
    (list (interpret-abstract-atom "sift([γ3|α4],α1)"))))
  (check-true
   (>=-extension
    (list
     (interpret-abstract-atom "sift([γ1|α1],α2)")
     (interpret-abstract-atom "filter(α2,α3)"))
    (list
     (interpret-abstract-atom "sift([γ2|α4],α1)")
     (interpret-abstract-atom "filter(α1,α5)"))))
  (check-false
   (>=-extension
    (list
     (interpret-abstract-atom "sift([γ1|α1],α2)")
     (interpret-abstract-atom "filter(α2,α3)"))
    (list
     (interpret-abstract-atom "sift([γ2|α4],α1)")
     (interpret-abstract-atom "filter(α6,α5)")))))

(define (renames? domain-elem1 domain-elem2)
  (and (>=-extension domain-elem1 domain-elem2)
       (>=-extension domain-elem2 domain-elem1)))
(provide
 (proc-doc/names
  renames?
  (-> abstract-domain-elem? abstract-domain-elem? boolean?)
  (domain-elem1 domain-elem2)
  @{Checks whether @racket[domain-elem1] is equivalent to @racket[domain-elem2].}))

; note: generates direct and indirect generalizations
; i.e. this generalizes nil to g1 but also to a1
; could also just generalize to g1, as g1 itself generalizes to a1
; on the other hand, that would make add-vertex trickier
(define (>-instances aa) (set))
(define (<-instances aa) (set))
(module+ test
  (parameterize
      ([max-depth 2]
       [domain-symbols
        (list
         (abstract-atom 'foo '()) ; foo/0
         (abstract-atom 'foo (list (a 1))) ; foo/1
         (abstract-atom 'foo (list (a 1) (a 2))) ; foo/2
         (abstract-function 'cons (list (a 1) (a 2)))
         (abstract-function 'nil '()))])
    (check-equal?
     (>-instances (abstract-atom 'foo '()))
     (set))
    (check-equal?
     (>-instances (abstract-atom 'foo (list (g 1))))
     (set (abstract-atom 'foo (list (a 1)))))
    (check-equal?
     (>-instances (abstract-atom 'foo (list (abstract-function 'nil '()))))
     (set
      (abstract-atom 'foo (list (g 1)))
      (abstract-atom 'foo (list (a 1)))))
    (check-equal?
     (>-instances
      (abstract-atom
       'foo
       (list
        (abstract-function 'nil '())
        (abstract-function 'nil '()))))
     (set
      (abstract-atom 'foo (list (g 1) (g 1)))
      (abstract-atom 'foo (list (g 1) (g 2)))
      (abstract-atom 'foo (list (g 1) (a 1)))
      (abstract-atom 'foo (list (a 1) (g 1)))
      (abstract-atom 'foo (list (a 1) (a 1)))
      (abstract-atom 'foo (list (a 1) (a 2)))))))