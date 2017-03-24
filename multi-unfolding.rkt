#lang at-exp racket
(require
  scribble/srcdoc
  "abstract-multi-domain.rkt")
(require (for-doc scribble/manual))

(define (unfold-multi m a-off g-off)
  (list))
(module+ test
  (require rackunit)
  (check-equal?
   (unfold-multi
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init (list (cons (a* 1 1 1) (a 1))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final (list (cons (a* 1 'L 2) (a 2)))))
    10
    15)
   (list
    (list (abstract-atom 'filter (list (g 16) (a 1) (a 2))))
    (list
     (abstract-atom 'filter (list (g 16) (a 1) (a 12)))
     (multi
      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init (list (cons (a* 1 1 1) (a 12))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final (list (cons (a* 1 'L 2) (a 2))))))))
  (check-equal?
   (unfold-multi
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init (list (cons (a* 1 1 1) (abstract-function 'cons (list (g 1) (a 1))))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final (list (cons (a* 1 'L 2) (a 2)))))
    10
    15)
   (list
    (list (abstract-atom 'filter (list (g 16) (abstract-function 'cons (list (g 1) (a 1))) (a 2))))
    (list
     (abstract-atom 'filter (list (g 16) (abstract-function 'cons (list (g 1) (a 1))) (a 12)))
     (multi
      (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
      #t
      (init (list (cons (a* 1 1 1) (a 12))))
      (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
      (final (list (cons (a* 1 'L 2) (a 2)))))))))
(provide
 (proc-doc/names
  unfold-multi
  (-> multi? exact-positive-integer? exact-positive-integer? (listof (listof abstract-conjunct?)))
  (m a-off g-off)
  @{Returns the "case: one" and "case: many" unfoldings of a @racket[multi] struct.
 The value @racket[a-off] specifies a safe offset value for "any"-type variables which
 are not constrained by the bindings in the multi abstraction and the @racket[g-off]
 serves the same purpose for "ground"-type variables.}))