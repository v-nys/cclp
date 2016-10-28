#lang at-exp racket
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(require "abstract-multi-domain.rkt")
(require (only-in "abstraction-inspection-utils.rkt" maximum-var-index))
(require (only-in "data-utils.rkt" some-v some?))
(require (only-in racket-list-utils/utils map-accumulatel))

(define (ground? elem)
  (cond [(list? elem) (andmap ground? elem)]
        [(abstract-atom? elem) (andmap ground? (abstract-atom-args elem))]
        [(abstract-function? elem) (andmap ground? (abstract-function-args elem))]
        [(g? elem) #t]
        [(a? elem) #f]
        [else (error "element unaccounted for in groundness check")]))

(define (depth elem)
  (cond [(list? elem) (apply max (map depth elem))]
        [(abstract-atom? elem) (apply max (map depth (abstract-atom-args elem)))]
        [(abstract-function? elem)
         (+
          1
          (if
           (null? (abstract-function-args elem))
           0
           (apply max (map depth (abstract-function-args elem)))))]
        [(abstract-variable? elem) 1]
        [else (error "element unaccounted for")]))

(define (abstract elem k)
  (define fresh-g (if (some? (maximum-var-index elem g?)) (+ (some-v (maximum-var-index elem g?)) 1) 1))
  (define fresh-a (if (some? (maximum-var-index elem a?)) (+ (some-v (maximum-var-index elem a?)) 1) 1))
  (define init-acc (list fresh-a fresh-g (hash)))
  
  (define (widen-atom k-aux atom acc)
    (let ([accumulation-args (map-accumulatel (curry widen-term k) acc (abstract-atom-args atom))])
      (cons (abstract-atom (abstract-atom-symbol atom) (car accumulation-args)) (cdr accumulation-args))))

  (define (widen-term k-aux term acc)
    (match
        acc
      [(list fresh-a fresh-g mapping)
       (if (<= (depth term) k-aux)
           (cons term acc)
           (if (equal? k-aux 1)
               (let ([current-mapping (hash-ref mapping term #f)])
                 (if current-mapping
                     (cons current-mapping acc)
                     (if (ground? term)
                         (cons (g fresh-g) (list fresh-a (+ fresh-g 1) (hash-set mapping term (g fresh-g))))
                         (cons (a fresh-a) (list (+ fresh-a 1) fresh-g (hash-set mapping term (a fresh-a)))))))
               (let ([recursion (map-accumulatel (curry widen-term (- k-aux 1)) acc (abstract-function-args term))])
                 (cons (abstract-function (abstract-function-functor term) (car recursion))
                       (cdr recursion)))))]))
  
  (cond [(list? elem)
         ; TODO: update once multi is incorporated
         (car (map-accumulatel (curry widen-atom k) init-acc elem))]
        [(abstract-atom? elem)
         (abstract-atom
          (abstract-atom-symbol elem)
          (car (map-accumulatel (curry widen-term k) init-acc (abstract-atom-args elem))))]
        [(abstract-function? elem)
         (car (widen-term k elem init-acc))]
        [(abstract-variable? elem) elem]))
(provide
 (proc-doc/names
  abstract
  (-> abstract-domain-elem? exact-positive-integer? abstract-domain-elem?)
  (abstract-domain-element k)
  @{Return the most specific abstraction of @racket[abstract-domain-element] whose depth is at most @racket[k].
 Depth is defined as follows: for abstract variables and constants in the abstract domain, it is 1.
 For function terms, it is the depth of the argument with the greatest depth, increased by one.
 For abstract atoms and conjunctions, it is identical to the depth of the argument with the greatest depth.
 This definition is based on the assumption that we only use first-order logic and thus cannot nest atoms.}))