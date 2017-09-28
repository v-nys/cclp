#lang at-exp racket
(require
  scribble/srcdoc
  "abstract-multi-domain.rkt"
  (only-in "multi-folding-unfolding.rkt" remove-multi-subscripts)
  "abstract-substitution.rkt"
  (only-in "abstract-unify.rkt" abstract-unify)
  (only-in "abstraction-inspection-utils.rkt" extract-subscripted-variables assemble-var-indices)
  (only-in "data-utils.rkt" some-v))
(require (for-doc scribble/manual))

(define (single-subscript-end-equalities constraints subscript-mapping)
  (map
   (match-lambda
     [(cons (a* id _ j) val)
      (abstract-equality (hash-ref subscript-mapping (a* id 'i j)) val)]
     [(cons (g* id _ j) val)
      (abstract-equality (hash-ref subscript-mapping (g* id 'i j)) val)])
   constraints))

;; subscript mapping maps each multi-subscript variable with symbolic index "i" to a fresh regular variable
;; single-subscript-conjunction is what happens when this mapping is applied
;; single-subscript-init is a translation of init constraints on multi-subscript variables to single-subscript variables
;: this part is common to the unfolding of a bounded number of conjunctions the unfolding of a new multi
(define (unfold-multi-* m a-off g-off)
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
  (values subscript-mapping single-subscript-conjunction))

(define (unfold-multi-bounded num m a-off g-off)
  (if (eqv? num 1)
      (let*-values
          ([(subscript-mapping single-subscript-conjunction) (unfold-multi-* m a-off g-off)]
           [(single-subscript-init) (single-subscript-end-equalities (init-constraints (multi-init m)) subscript-mapping)]
           [(single-subscript-final) (single-subscript-end-equalities (final-constraints (multi-final m)) subscript-mapping)]
           [(combined-substitution) (some-v (abstract-unify (append single-subscript-init single-subscript-final) 0))])
        (cons (apply-substitution-to-conjunction combined-substitution single-subscript-conjunction) combined-substitution))
      (match-let*
          ([many-unf (unfold-multi-many m a-off g-off)]
           [new-off (apply max (cons 0 (assemble-var-indices (λ (_) #t) many-unf)))]
           [(cons rec-unf rec-subst) (unfold-multi-bounded (- num 1) (last many-unf) new-off new-off)])
        (cons (append (drop-right many-unf 1) rec-unf) rec-subst))))
(provide
 (proc-doc/names
  unfold-multi-bounded
  (-> exact-positive-integer? multi? exact-nonnegative-integer? exact-nonnegative-integer? (cons/c (listof abstract-atom?) (listof abstract-equality?)))
  (num m a-off g-off)
  @{Unfolds a multi abstraction @racket[m] so that its unfolding does not contain a multi abstraction.
 The unfolding consists of @racket[num] repetitions of the represented conjunction.
 The values @racket[a-off] and @racket[g-off] are offsets for freshly introduced single-subscript variables.
 Normally, these are expected to be the greatest single-subscript variables in the conjunction in which the unfolding is applied.
 The result consists of the resulting unfolding, and of a substitution (which is necessary if initial and final constraints pertain to the same variable).}))

(define (unfold-multi-many m a-off g-off)
  (define-values
    (subscript-mapping single-subscript-conjunction)
    (unfold-multi-* m a-off g-off))
  (define single-subscript-init (single-subscript-end-equalities (init-constraints (multi-init m)) subscript-mapping))
  (define initial-conjunction (apply-substitution-to-conjunction single-subscript-init single-subscript-conjunction))
  (define (consecutive-shift pair)
    (match pair
      [(cons (a* id 'i+1 j) (a* id 'i k))
       (cons (a* id 1 j) (apply-substitution single-subscript-init (hash-ref subscript-mapping (a* id 'i k))))]
      [(cons (g* id 'i+1 j) (g* id 'i k))
       (cons (g* id 1 j) (apply-substitution single-subscript-init (hash-ref subscript-mapping (g* id 'i k))))]))
  (append initial-conjunction
          (list (struct-copy multi m [init (init (map consecutive-shift (consecutive-constraints (multi-consecutive m))))]))))
(provide unfold-multi-many)

(define (unfold-multi-many-bounded num m a-off g-off)
  (if (equal? num 1)
      (unfold-multi-many m a-off g-off)
      (let* ([unf-one (unfold-multi-many m a-off g-off)]
             [unf-m (last unf-one)]
             [off-2 (apply max (cons 0 (assemble-var-indices (λ (_) #t) unf-one)))])
        (append (drop-right unf-one 1) (unfold-multi-many-bounded (sub1 num) unf-m off-2 off-2)))))
(provide unfold-multi-many-bounded)

(define (unfold-multi-many-right m a-off g-off)
  (define-values
    (subscript-mapping single-subscript-conjunction)
    (unfold-multi-* m a-off g-off))
  (define single-subscript-final (single-subscript-end-equalities (final-constraints (multi-final m)) subscript-mapping))
  (define final-conjunction (apply-substitution-to-conjunction single-subscript-final single-subscript-conjunction))
  (define (consecutive-shift pair)
    (match pair
      [(cons (a* id 'i+1 j) (a* id 'i k))
       (cons (a* id 'L k) (apply-substitution single-subscript-final (hash-ref subscript-mapping (a* id 'i j))))]
      [(cons (g* id 'i+1 j) (g* id 'i k))
       (cons (g* id 'L k) (apply-substitution single-subscript-final (hash-ref subscript-mapping (g* id 'i j))))]))
  (append
   (list (struct-copy multi m [final (final (map consecutive-shift (consecutive-constraints (multi-consecutive m))))]))
   final-conjunction))
(module+ test
  (check-equal?
   (unfold-multi-many-right
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init (list (cons (a* 1 1 1) (a 1))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final (list (cons (a* 1 'L 2) (a 2)))))
    100
    100)
   (list
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init (list (cons (a* 1 1 1) (a 1))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final (list (cons (a* 1 'L 2) (a 101)))))
    (abstract-atom 'filter (list (g 101) (a 101) (a 2))))))
(provide unfold-multi-many-right)

(define (unfold-multi* idx conjunction)
  (define m (list-ref conjunction idx))
  (define a-off (apply max (cons 0 (assemble-var-indices a? conjunction))))
  (define g-off (apply max (cons 0 (assemble-var-indices g? conjunction))))
  (define pairs (unfold-multi m a-off g-off))
  (map (match-lambda [(cons unf sub) (apply-substitution sub (append (take conjunction idx) unf (drop conjunction (add1 idx))))]) pairs)) ; replace the multi, apply substitution to the whole thing
(provide unfold-multi*)

(define (unfold-multi m a-off g-off)
  (list
   (unfold-multi-bounded 1 m a-off g-off)
   (cons (unfold-multi-many m a-off g-off) (list)))) ; clash is not possible here
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
    (cons (list (abstract-atom 'filter (list (g 16) (a 1) (a 2)))) (list (abstract-equality (a 11) (a 1)) (abstract-equality (a 12) (a 2))))
    (cons
     (list
      (abstract-atom 'filter (list (g 16) (a 1) (a 12)))
      (multi
       (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
       #t
       (init (list (cons (a* 1 1 1) (a 12))))
       (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
       (final (list (cons (a* 1 'L 2) (a 2))))))
     (list))))
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
    (cons (list (abstract-atom 'filter (list (g 16) (abstract-function 'cons (list (g 1) (a 1))) (a 2)))) (list (abstract-equality (a 11) (abstract-function 'cons (list (g 1) (a 1)))) (abstract-equality (a 12) (a 2))))
    (cons (list
           (abstract-atom 'filter (list (g 16) (abstract-function 'cons (list (g 1) (a 1))) (a 12)))
           (multi
            (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
            #t
            (init (list (cons (a* 1 1 1) (a 12))))
            (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
            (final (list (cons (a* 1 'L 2) (a 2)))))) (list))))
  (check-equal?
   (unfold-multi-bounded
    1
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init (list (cons (g* 1 1 1) (g 1)) (cons (a* 1 1 1) (a 1)) (cons (a* 1 1 2) (a 2))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final (list (cons (a* 1 'L 2) (a 3)))))
    100
    100)
   (cons
    (list
     (abstract-atom 'filter (list (g 1) (a 1) (a 3))))
    (list
     (abstract-equality (g 101) (g 1))
     (abstract-equality (a 101) (a 1))
     (abstract-equality (a 102) (a 2))
     (abstract-equality (a 2) (a 3))))) ; only this one actually matters
  (check-equal?
   (unfold-multi-bounded
    2
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init (list (cons (a* 1 1 1) (a 1))))
     (consecutive (list (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final (list (cons (a* 1 'L 2) (a 2)))))
    100
    100)
   (cons
    (list
     (abstract-atom 'filter (list (g 101) (a 1) (a 102)))
     (abstract-atom 'filter (list (g 103) (a 102) (a 2))))
    (list
     (abstract-equality (a 103) (a 102))
     (abstract-equality (a 104) (a 2))))))
(provide
 (proc-doc/names
  unfold-multi
  (-> multi? exact-positive-integer? exact-positive-integer? (listof (cons/c (listof abstract-conjunct?) (listof abstract-equality?))))
  (m a-off g-off)
  @{Returns the "case: one" and "case: many" unfoldings of a @racket[multi] struct.
 The value @racket[a-off] specifies a safe offset value for "any"-type variables which
 are not constrained by the bindings in the multi abstraction and the @racket[g-off]
 serves the same purpose for "ground"-type variables.
 The resulting substitutions are as in @racket[unfold-multi-bounded].}))