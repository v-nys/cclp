#lang at-exp racket

(require (only-in racket-list-utils/utils map-accumulater))

; cannot define these functions in the domain module, would cause circular import between domain module and unification module
(require "abstract-multi-domain.rkt")
(require "abstract-unify.rkt")
(require "data-utils.rkt")
(require "abstract-substitution.rkt")
(require "abstract-renaming.rkt")
(require (only-in "abstraction-inspection-utils.rkt" assemble-var-indices))
(require "multi-unfolding.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/manual))
(module+ test
  (require rackunit)
  (require "cclp-interpreter.rkt"))

;; TODO: double check any potential renaming issues
(define (>=-extension domain-elem1 domain-elem2)
  (define (represent c n off)
    (match c
      [(? abstract-atom?) (cons c off)]
      [(? multi?)
       (let* ([unf (unfold-multi-bounded n c off off)]
              [new-off (apply max (assemble-var-indices (λ (_) #t) unf))])
         (cons unf new-off))]))
  (match* (domain-elem1 domain-elem2)
    [((? abstract-domain-elem?) (? abstract-domain-elem?))
     (let* ([renamed-domain-elem2
             (rename-apart domain-elem2 domain-elem1)]
            [unifier
             (abstract-unify
              (list (abstract-equality domain-elem1 renamed-domain-elem2)) 0)])
       (and
        (some? unifier)
        (equal?
         (apply-substitution (some-v unifier) domain-elem1)
         renamed-domain-elem2)))]
    [((? multi?) (? (listof abstract-atom?)))
     (if (<= (length (multi-conjunction domain-elem1)) (length domain-elem2))
         (let* ([offset (apply max (assemble-var-indices (λ (_) #t) domain-elem1))]
                [unf-one (unfold-multi-bounded 1 domain-elem1 offset offset)]
                [unf-many (unfold-multi-many domain-elem1 offset offset)])
           (or (>=-extension unf-one domain-elem2)
               (>=-extension unf-many domain-elem2)))
         #f)]
    [((? list?) (? list?))
     #:when (and (ormap multi? domain-elem1) (ormap multi? domain-elem2))
     (let* ([off (apply max (assemble-var-indices (λ (_) #t) domain-elem2))]
            [repr-1 (car (map-accumulater (λ (c off) (represent c 1 off)) off domain-elem2))]
            [repr-2 (car (map-accumulater (λ (c off) (represent c 2 off)) off domain-elem2))])
       (and (>=-extension domain-elem1 repr-1)
            (>=-extension domain-elem1 repr-2)))]
    [((? list?) (? list?))
     #:when (and (ormap multi? domain-elem1) (not (ormap multi? domain-elem2)))
     (if (> (length domain-elem1) (length domain-elem2))
         #f
         (let* ([off (apply max (assemble-var-indices (λ (_) #t) domain-elem1))]
                [multis (filter multi? domain-elem1)]
                [one-unfs (map (λ (m) (unfold-multi-bounded 1 m off off)) multis)]
                [many-unfs (map (λ (m) (unfold-multi-many m off off)) multis)]
                [replacer (λ (m u) (append (takef domain-elem1 (compose not (curry equal? m))) (append u (drop (dropf domain-elem1 (compose not (curry equal? m))) 1))))]
                [resulting-conjunctions (append (map replacer multis one-unfs) (map replacer multis many-unfs))])
           (ormap (λ (c) (>=-extension c domain-elem2)) resulting-conjunctions)))]
    [((? multi?) (? multi?))
     (let* ([offset (apply max (assemble-var-indices (λ (_) #t) (list domain-elem1 domain-elem2)))]
            [unf-1 (unfold-multi-bounded 2 domain-elem1 offset offset)]
            [unf-2 (unfold-multi-bounded 2 domain-elem2 offset offset)])
       (>=-extension unf-1 unf-2))]))
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
     (interpret-abstract-atom "filter(α6,α5)"))))
  (check-true
   (>=-extension
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 1))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 2))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 1))
       (cons (a* 1 1 1) (abstract-function 'cons (list (g 3) (a 1))))
       (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 2))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))))
  (check-false
   (>=-extension
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 1))
       (cons (a* 1 1 1) (abstract-function 'cons (list (g 3) (a 1))))
       (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 2))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 1))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 2))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))))
  ; needs more tests for the abstract multi domain
  ; specifically:
  ; multi vs list (without multis)
  ; list with multi vs list without multi
  ; two lists with multis
  ; note that multi vs list containing multis is not implemented
  ; this is like an atom not being considered more general than a single-atom conjunction
  (check-true
   (>=-extension
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 1))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 2))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (list (abstract-atom 'filter (list (g 1) (a 1) (a 2))))))
  (check-true
   (>=-extension
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 1))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 2))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (list (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
          (abstract-atom 'filter (list (g 2) (a 2) (a 3))))))
  (check-true
   (>=-extension
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 1))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 2))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (list (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
          (abstract-atom 'filter (list (g 2) (a 2) (a 3)))
          (abstract-atom 'filter (list (g 3) (a 3) (a 4)))))))
(provide
 (proc-doc/names
  >=-extension
  (-> abstract-domain-elem? abstract-domain-elem? boolean?)
  (domain-elem1 domain-elem2)
  @{Checks whether @racket[domain-elem1] is at least as general as @racket[domain-elem2].
 Note that this does not currently support the abstract multi domain, only the abstract domain.}))

(define (renames? domain-elem1 domain-elem2)
  (and (>=-extension domain-elem1 domain-elem2)
       (>=-extension domain-elem2 domain-elem1)))
; TODO needs tests for multi equivalence (and equivalence of lists containing multi)
(module+ test
  (check-true #f))
(provide
 (proc-doc/names
  renames?
  (-> abstract-domain-elem*? abstract-domain-elem*? boolean?)
  (domain-elem1 domain-elem2)
  @{Checks whether @racket[domain-elem1] is equivalent to @racket[domain-elem2].}))