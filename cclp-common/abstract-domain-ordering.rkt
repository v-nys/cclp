#lang alpha-gamma at-exp racket

; cannot define these functions in the domain module, would cause circular import between domain module and unification module
(require cclp-common-data/abstract-multi-domain)
(require "abstract-unify.rkt")
(require "data-utils.rkt")
(require cclp-common-data/abstract-substitution "abstract-substitution-application.rkt")
(require "abstract-renaming.rkt")
(require (only-in "abstraction-inspection-utils.rkt" assemble-var-indices))
(require "multi-unfolding.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/manual))
(module+ test
         (require rackunit))

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
                     (let* ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) domain-elem1)))]
                            ; don't need substitution here
                            [unf-one (car (unfold-multi-bounded 1 domain-elem1 offset offset))]
                            [unf-many (unfold-multi-many domain-elem1 offset offset)])
                           (or (>=-extension unf-one domain-elem2)
                               (>=-extension unf-many domain-elem2)))
                     #f)]
                [((? list?) (? list?))
                 #:when (and (ormap multi? domain-elem1) (ormap multi? domain-elem2))
                 (let* ([off (apply max (cons 0 (assemble-var-indices (λ (_) #t) domain-elem2)))]
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
                     (let* ([off (apply max (cons 0 (assemble-var-indices (λ (_) #t) domain-elem1)))]
                            [multis (filter multi? domain-elem1)]
                            [one-unfs (map (λ (m) (unfold-multi-bounded 1 m off off)) multis)]
                            [many-unfs (map (λ (m) (cons (unfold-multi-many m off off) (list))) multis)]
                            [resulting-conjunctions (append (map (curry replacer domain-elem1) multis one-unfs) (map (curry replacer domain-elem1) multis many-unfs))])
                           (ormap (λ (c) (>=-extension c domain-elem2)) resulting-conjunctions)))]
                [((? list?) (? list?))
                 #:when (and (not (ormap multi? domain-elem1)) (ormap multi? domain-elem2))
                 #f]
                [((? multi?) (? multi?))
                 (let* ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) (list domain-elem1 domain-elem2))))]
                        ; don't need substitution here
                        [unf-1 (car (unfold-multi-bounded 2 domain-elem1 offset offset))]
                        [unf-2 (car (unfold-multi-bounded 2 domain-elem2 offset offset))])
                       (>=-extension unf-1 unf-2))]
                [((? abstract-domain-elem?) (? multi?)) #f]
                [((? multi?) (? abstract-atom?))
                 (>=-extension domain-elem1 (list domain-elem2))]))
(module+ test
         (check-true (>=-extension (a 1) (g 1)))
         (check-true (>=-extension (a 1) (a 2)))
         (check-false (>=-extension (g 1) (a 1)))
         (check-true (>=-extension (g 1) (g 1)))
         (check-true (>=-extension β(nil) β(nil)))
         (check-false (>=-extension β(nil) β(nonnil)))
         (check-true
          (>=-extension
           β(foo(g1,bar(g2,a1,g1)))
           β(foo(nil,bar(nonnil,g3,nil)))))
         (check-false
          (>=-extension
           β(foo(g1,bar(g2,a1,g1)))
           β(foo(nil,bar(nonnil,g3,nonnil)))))
         (check-true
          (>=-extension
           α(foo(g1,bar(g2,a1,g1)))
           α(foo(nil,bar(nonnil,g3,nil)))))
         (check-false
          (>=-extension
           α(foo(g1,bar(g2,a1,g1)))
           α(foo(nil,bar(nonnil,g3,nonnil)))))
         (check-true
          (>=-extension
           β(foo(g1,bar(g2,a1,g1)))
           β(foo(nil,bar(nonnil,g3,nil)))))
         (check-true
          (>=-extension
           α(sift([g1|a1],a2))
           α(sift([g2|a4],a1)))) ; renaming should be implicit
                                 (check-true
                                  (>=-extension
                                   (list α(sift([g1|a1],a2)))
                                   (list α(sift([g2|a4],a1)))))
                                 (check-false
                                  (>=-extension
                                   (list α(sift([g1|g2],a2)))
                                   (list α(sift([g3|a4],a1)))))
                                 (check-true
                                  (>=-extension
                                   (list
                                    α(sift([g1|a1],a2))
                                    α(filter(a2,a3)))
                                   (list
                                    α(sift([g2|a4],a1))
                                    α(filter(a1,a5)))))
                                 (check-false
                                  (>=-extension
                                   (list
                                    α(sift([g1|a1],a2))
                                    α(filter(a2,a3)))
                                   (list
                                    α(sift([g2|a4],a1))
                                    α(filter(a6,a5)))))
                                 (check-true
                                  (>=-extension
                                   α(multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),{g<1,1,1>=g1,a<1,1,1>=a1,a<1,1,2>=a2},{a<1,i+1,1>=a<1,i,2>},{g<1,l,1>=g2,a<1,l,1>=a2,a<1,l,2>=a3}))
                                   α(multi(filter(g<1,i,1>,a<1,i,1>,a<1,i,2>),{g<1,1,1>=g1,a<1,1,1>=[g3|a1],a<1,1,2>=a2},{a<1,i+1,1>=a<1,i,2>},{g<1,l,1>=g2,a<1,l,1>=a2,a<1,l,2>=a3}))))
                                 (check-false
                                  (>=-extension
                                   (simple-multi
                                    (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                    (list
                                     (cons (g* 1 1 1) (g 1))
                                     (cons (a* 1 1 1) (abstract-function 'cons (list (g 3) (a 1))))
                                     (cons (a* 1 1 2) (a 2)))
                                    (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                    (list
                                     (cons (g* 1 'L 1) (g 2))
                                     (cons (a* 1 'L 1) (a 2))
                                     (cons (a* 1 'L 2) (a 3))))
                                   (simple-multi
                                    (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                    (list
                                     (cons (g* 1 1 1) (g 1))
                                     (cons (a* 1 1 1) (a 1))
                                     (cons (a* 1 1 2) (a 2)))
                                    (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                    (list
                                     (cons (g* 1 'L 1) (g 2))
                                     (cons (a* 1 'L 1) (a 2))
                                     (cons (a* 1 'L 2) (a 3))))))
                                 (check-true
                                  (>=-extension
                                   (simple-multi
                                    (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                    (list
                                     (cons (g* 1 1 1) (g 1))
                                     (cons (a* 1 1 1) (a 1))
                                     (cons (a* 1 1 2) (a 2)))
                                    (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                    (list
                                     (cons (g* 1 'L 1) (g 2))
                                     (cons (a* 1 'L 1) (a 3))
                                     (cons (a* 1 'L 2) (a 4))))
                                   (list (abstract-atom 'filter (list (g 1) (a 1) (a 2))))))
                                 (check-true
                                  (>=-extension
                                   (simple-multi
                                    (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                    (list
                                     (cons (g* 1 1 1) (g 1))
                                     (cons (a* 1 1 1) (a 1))
                                     (cons (a* 1 1 2) (a 2)))
                                    (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                    (list
                                     (cons (g* 1 'L 1) (g 2))
                                     (cons (a* 1 'L 1) (a 3))
                                     (cons (a* 1 'L 2) (a 4))))
                                   (list (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
                                         (abstract-atom 'filter (list (g 2) (a 2) (a 3))))))
                                 (check-true
                                  (>=-extension
                                   (simple-multi
                                    (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                    (list
                                     (cons (g* 1 1 1) (g 1))
                                     (cons (a* 1 1 1) (a 1))
                                     (cons (a* 1 1 2) (a 2)))
                                    (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                    (list
                                     (cons (g* 1 'L 1) (g 2))
                                     (cons (a* 1 'L 1) (a 3))
                                     (cons (a* 1 'L 2) (a 4))))
                                   (list (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
                                         (abstract-atom 'filter (list (g 2) (a 2) (a 3)))
                                         (abstract-atom 'filter (list (g 3) (a 3) (a 4))))))
                                 (check-true
                                  (>=-extension
                                   (list
                                    (abstract-atom 'integers (list (g 10) (a 1)))
                                    (simple-multi
                                     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                     (list
                                      (cons (g* 1 1 1) (g 1))
                                      (cons (a* 1 1 1) (a 1))
                                      (cons (a* 1 1 2) (a 2)))
                                     (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                     (list
                                      (cons (g* 1 'L 1) (g 2))
                                      (cons (a* 1 'L 1) (a 3))
                                      (cons (a* 1 'L 2) (a 4))))
                                    (abstract-atom 'sift (list (a 4) (a 5))))
                                   (list
                                    (abstract-atom 'integers (list (g 10) (a 1)))
                                    (abstract-atom 'filter (list (g 1) (a 1) (a 2)))
                                    (abstract-atom 'sift (list (a 2) (a 3))))))
                                 (check-true
                                  (>=-extension
                                   (list
                                    (abstract-atom 'integers (list (g 10) (a 1)))
                                    (simple-multi
                                     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                     (list
                                      (cons (g* 1 1 1) (g 1))
                                      (cons (a* 1 1 1) (a 1))
                                      (cons (a* 1 1 2) (a 2)))
                                     (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                     (list
                                      (cons (g* 1 'L 1) (g 2))
                                      (cons (a* 1 'L 1) (a 3))
                                      (cons (a* 1 'L 2) (a 4))))
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
                                    (simple-multi
                                     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                     (list
                                      (cons (g* 1 1 1) (g 1))
                                      (cons (a* 1 1 1) (a 1))
                                      (cons (a* 1 1 2) (a 2)))
                                     (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                     (list
                                      (cons (g* 1 'L 1) (g 2))
                                      (cons (a* 1 'L 1) (a 3))
                                      (cons (a* 1 'L 2) (a 4))))
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
                                    (simple-multi
                                     (list (abstract-atom* 'filterA (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                     (list
                                      (cons (g* 1 1 1) (g 1))
                                      (cons (a* 1 1 1) (a 1))
                                      (cons (a* 1 1 2) (a 2)))
                                     (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                     (list
                                      (cons (g* 1 'L 1) (g 2))
                                      (cons (a* 1 'L 1) (a 3))
                                      (cons (a* 1 'L 2) (a 4))))
                                    (simple-multi
                                     (list (abstract-atom* 'filterB (list (g* 2 'i 1) (a* 2 'i 1) (a* 2 'i 2))))
                                     (list
                                      (cons (g* 2 1 1) (g 4))
                                      (cons (a* 2 1 1) (a 4))
                                      (cons (a* 2 1 2) (a 5)))
                                     (list (cons (a* 2 'i+1 1) (a* 2 'i 2)))
                                     (list
                                      (cons (g* 2 'L 1) (g 5))
                                      (cons (a* 2 'L 1) (a 6))
                                      (cons (a* 2 'L 2) (a 7)))))
                                   (list
                                    (abstract-atom 'integers (list (g 10) (a 1)))
                                    (abstract-atom 'filterA (list (g 1) (a 1) (a 2)))
                                    (abstract-atom 'filterA (list (g 2) (a 2) (a 3)))
                                    (abstract-atom 'filterA (list (g 3) (a 3) (a 4)))
                                    (simple-multi
                                     (list (abstract-atom* 'filterB (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
                                     (list
                                      (cons (g* 1 1 1) (g 4))
                                      (cons (a* 1 1 1) (a 4))
                                      (cons (a* 1 1 2) (a 5)))
                                     (list (cons (a* 1 'i+1 1) (a* 1 'i 2)))
                                     (list
                                      (cons (g* 1 'L 1) (g 6))
                                      (cons (a* 1 'L 1) (a 6))
                                      (cons (a* 1 'L 2) (a 7))))))))
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
