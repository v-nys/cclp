#lang at-exp racket
(require
  scribble/srcdoc
  "abstract-multi-domain.rkt"
  (only-in "multi-folding-unfolding.rkt" remove-multi-subscripts)
  "abstract-substitution.rkt"
  (only-in "abstraction-inspection-utils.rkt" extract-subscripted-variables))
(require (for-doc scribble/manual))

; TODO: make sure substitution is returned, as well

(define (unfold-multi-one m a-off g-off)
  (define subscript-mapping
    (foldl
     (λ (el acc)
       (match el
         [(a* id 'i j) (hash-set acc el (a (+ a-off j)))]
         [(g* id 'i j) (hash-set acc el (g (+ g-off j)))]))
     (make-immutable-hash)
     (extract-subscripted-variables m)))
  (define single-subscript-conjunction
    (remove-multi-subscripts (multi-conjunction m) subscript-mapping))
  (define single-subscript-init
    (map
     (match-lambda
       [(cons (a* id 1 j) val) (abstract-equality (hash-ref subscript-mapping (a* id 'i j)) val)]
       [(cons (g* id 1 j) val) (abstract-equality (hash-ref subscript-mapping (g* id 'i j)) val)])
     (init-constraints (multi-init m))))
  (define single-subscript-final
    (map
     (match-lambda
       [(cons (a* id 'L j) val) (abstract-equality (hash-ref subscript-mapping (a* id 'i j)) val)]
       [(cons (g* id 'L j) val) (abstract-equality (hash-ref subscript-mapping (g* id 'i j)) val)])
     (final-constraints (multi-final m))))
  (apply-substitution-to-conjunction
   (append single-subscript-init single-subscript-final)
   single-subscript-conjunction))
(provide unfold-multi-one)

(define (unfold-multi-many m a-off g-off)
  (define subscript-mapping
    (foldl
     (λ (el acc) (match el [(a* id i j) (hash-set acc el (a (+ a-off j)))] [(g* id i j) (hash-set acc el (g (+ g-off j)))]))
     (make-immutable-hash)
     (extract-subscripted-variables m)))
  (define single-subscript-conjunction (remove-multi-subscripts (multi-conjunction m) subscript-mapping))
  (define single-subscript-init
    (map
     (match-lambda
       [(cons (a* id 1 j) val) (abstract-equality (hash-ref subscript-mapping (a* id 'i j)) val)]
       [(cons (g* id 1 j) val) (abstract-equality (hash-ref subscript-mapping (g* id 'i j)) val)])
     (init-constraints (multi-init m))))
  (define initial-conjunction (apply-substitution-to-conjunction single-subscript-init single-subscript-conjunction))
  (define (consecutive-shift pair)
    (match pair
      [(cons (a* id 'i+1 j) (a* id 'i k)) (cons (a* id 1 j) (apply-substitution single-subscript-init (hash-ref subscript-mapping (a* id 'i k))))]
      [(cons (g* id 'i+1 j) (g* id 'i k)) (cons (g* id 1 j) (apply-substitution single-subscript-init (hash-ref subscript-mapping (g* id 'i k))))]))
  (append initial-conjunction
          (list (struct-copy multi m [init (init (map consecutive-shift (consecutive-constraints (multi-consecutive m))))]))))

(define (unfold-multi m a-off g-off)
  (list
   (unfold-multi-one m a-off g-off)
   (unfold-multi-many m a-off g-off)))
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
      (final (list (cons (a* 1 'L 2) (a 2))))))))
  (check-true #f)) ; TODO: substitution in case init and final are merged
(provide
 (proc-doc/names
  unfold-multi
  (-> multi? exact-positive-integer? exact-positive-integer? (listof (listof abstract-conjunct?)))
  (m a-off g-off)
  @{Returns the "case: one" and "case: many" unfoldings of a @racket[multi] struct.
 The value @racket[a-off] specifies a safe offset value for "any"-type variables which
 are not constrained by the bindings in the multi abstraction and the @racket[g-off]
 serves the same purpose for "ground"-type variables.}))