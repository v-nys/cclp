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

(define (>=-extension domain-elem1 domain-elem2)
  (define (replacer de m u*)
    (define s (cdr u*))
    (define u (car u*))
    (apply-substitution s (append (takef de (compose not (curry equal? m))) (append u (drop (dropf de (compose not (curry equal? m))) 1)))))
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
                ; don't need substitution here
                [unf-one (car (unfold-multi-bounded 1 domain-elem1 offset offset))]
                [unf-many (unfold-multi-many domain-elem1 offset offset)])
           (or (>=-extension unf-one domain-elem2)
               (>=-extension unf-many domain-elem2)))
         #f)]
    [((? list?) (? list?))
     #:when (and (ormap multi? domain-elem1) (ormap multi? domain-elem2))
     (let* ([off (apply max (assemble-var-indices (λ (_) #t) domain-elem2))]
            [first-multi (findf multi? domain-elem2)]
            [one-unf (unfold-multi-bounded 1 first-multi off off)]
            [two-unf (unfold-multi-bounded 2 first-multi off off)]
            [repr-1 (replacer domain-elem2 first-multi one-unf)]
            [repr-2 (replacer domain-elem2 first-multi two-unf)])
       (and (>=-extension domain-elem1 repr-1)
            (>=-extension domain-elem1 repr-2)))]
    [((? list?) (? list?))
     #:when (and (ormap multi? domain-elem1) (not (ormap multi? domain-elem2)))
     (if (> (length domain-elem1) (length domain-elem2))
         #f
         (let* ([off (apply max (assemble-var-indices (λ (_) #t) domain-elem1))]
                [multis (filter multi? domain-elem1)]
                [one-unfs (map (λ (m) (unfold-multi-bounded 1 m off off)) multis)]
                [many-unfs (map (λ (m) (cons (unfold-multi-many m off off) (list))) multis)]
                [resulting-conjunctions (append (map (curry replacer domain-elem1) multis one-unfs) (map (curry replacer domain-elem1) multis many-unfs))])
           (ormap (λ (c) (>=-extension c domain-elem2)) resulting-conjunctions)))]
    [((? list?) (? list?))
     #:when (and (not (ormap multi? domain-elem1)) (ormap multi? domain-elem2))
     #f]
    [((? multi?) (? multi?))
     (let* ([offset (apply max (assemble-var-indices (λ (_) #t) (list domain-elem1 domain-elem2)))]
            ; don't need substitution here
            [unf-1 (car (unfold-multi-bounded 2 domain-elem1 offset offset))]
            [unf-2 (car (unfold-multi-bounded 2 domain-elem2 offset offset))])
       (>=-extension unf-1 unf-2))]
    [((? abstract-domain-elem?) (? multi?)) #f]))
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
       (cons (a* 1 'L 1) (a 3))
       (cons (a* 1 'L 2) (a 4)))))
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
       (cons (a* 1 'L 1) (a 3))
       (cons (a* 1 'L 2) (a 4)))))
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
       (cons (a* 1 'L 1) (a 3))
       (cons (a* 1 'L 2) (a 4)))))
    (list (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
          (abstract-atom 'filter (list (g 2) (a 2) (a 3)))
          (abstract-atom 'filter (list (g 3) (a 3) (a 4))))))
  (check-true
   (>=-extension
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
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
        (cons (a* 1 'L 1) (a 3))
        (cons (a* 1 'L 2) (a 4)))))
     (abstract-atom 'sift (list (a 4) (a 5))))
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
     (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
     (abstract-atom 'sift (list (a 2) (a 3))))))
  (check-true
   (>=-extension
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
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
        (cons (a* 1 'L 1) (a 3))
        (cons (a* 1 'L 2) (a 4)))))
     (abstract-atom 'sift (list (a 4) (a 5))))
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
     (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
     (abstract-atom 'filter (list (g 2) (a 2) (a 3)))
     (abstract-atom 'sift (list (a 3) (a 4))))))
  (check-true
   (>=-extension
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
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
        (cons (a* 1 'L 1) (a 3))
        (cons (a* 1 'L 2) (a 4)))))
     (abstract-atom 'sift (list (a 4) (a 5))))
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
     (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
     (abstract-atom 'filter (list (g 2) (a 2) (a 3)))
     (abstract-atom 'filter (list (g 3) (a 3) (a 4)))
     (abstract-atom 'sift (list (a 4) (a 5))))))
  (check-true
   (>=-extension
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
     (multi
      (list (abstract-atom* 'filterA (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
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
        (cons (a* 1 'L 1) (a 3))
        (cons (a* 1 'L 2) (a 4)))))
     (multi
      (list (abstract-atom* 'filterB (list (g* 2 'i 1) (a* 2 'i 1) (a* 2 'i 2))))
      #t
      (init
       (list
        (cons (g* 2 1 1) (g 4))
        (cons (a* 2 1 1) (a 4))
        (cons (a* 2 1 2) (a 5))))
      (consecutive (list (cons (a* 2 'i+1 1) (a* 2 'i 2))))
      (final
       (list
        (cons (g* 2 'L 1) (g 5))
        (cons (a* 2 'L 1) (a 6))
        (cons (a* 2 'L 2) (a 7))))))
    (list
     (abstract-atom 'integers (list (g 10) (a 1)))
     (abstract-atom 'filterA (list (g 1) (a 1) (a 2)))
     (abstract-atom 'filterA (list (g 2) (a 2) (a 3)))
     (abstract-atom 'filterA (list (g 3) (a 3) (a 4)))
     (multi
      (list (abstract-atom* 'filterB (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init
       (list
        (cons (g* 1 1 1) (g 4))
        (cons (a* 1 1 1) (a 4))
        (cons (a* 1 1 2) (a 5))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final
       (list
        (cons (g* 1 'L 1) (g 6))
        (cons (a* 1 'L 1) (a 6))
        (cons (a* 1 'L 2) (a 7)))))))))
(provide
 (proc-doc/names
  >=-extension
  (-> (or/c abstract-domain-elem? multi?) (or/c abstract-domain-elem? multi?) boolean?)
  (domain-elem1 domain-elem2)
  @{Checks whether @racket[domain-elem1] is at least as general as @racket[domain-elem2].
 Note that this does not currently support the abstract multi domain, only the abstract domain.}))

(define (renames? domain-elem1 domain-elem2)
  (and (>=-extension domain-elem1 domain-elem2)
       (>=-extension domain-elem2 domain-elem1)))
(provide
 (proc-doc/names
  renames?
  (-> abstract-domain-elem*? abstract-domain-elem*? boolean?)
  (domain-elem1 domain-elem2)
  @{Checks whether @racket[domain-elem1] is equivalent to @racket[domain-elem2].}))