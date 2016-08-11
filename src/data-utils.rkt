#lang typed/racket

(struct none () #:transparent)
(provide (struct-out none))
(struct (a) some ([v : a]) #:transparent)
(provide (struct-out some))
(define-type (Opt a) (U none (some a)))
(provide Opt)

; 2-tuples are useful because consing with an empty list can change something's type to list when we want it to be a pair
(struct (X Y) 2-tuple ([first : X] [second : Y]) #:transparent)
(provide (struct-out 2-tuple))

; may want to create a set utils module?
(: optional-set-union (All (A) (-> (Setof A) * (Setof A))))
(define (optional-set-union . sets)
  (foldl (λ ([el : (Setof A)] [acc : (Setof A)]) (set-union el acc)) (ann (set) (Setof A)) sets))
(provide optional-set-union)