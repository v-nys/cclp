#lang racket

(struct none ()
  #:methods
  gen:equal+hash
  [(define (equal-proc n1 n2 equal?-recur) #t)
   (define (hash-proc my-none hash-recur) 357)
   (define (hash2-proc my-none hash2-recur) 729)])
(provide (struct-out none))

(struct some (v)
  #:methods
  gen:equal+hash
  [(define (equal-proc s1 s2 equal?-recur) (equal?-recur s1 s2))
   (define (hash-proc my-some hash-recur) (hash-recur (some-v my-some)))
   (define (hash2-proc my-some hash2-recur) (hash2-recur (some-v my-some)))])
(provide (struct-out some))

(define (maybe type-predicate)
  (λ (elem) (or (none? elem)
                (and (some? elem)
                     (type-predicate (some-v elem))))))
(provide maybe)

; 2-tuples are useful because consing with an empty list can change something's type to list when we want it to be a pair
(struct 2-tuple (first second)
  #:methods
  gen:equal+hash
  [(define (equal-proc t1 t2 equal?-recur)
     (and (equal?-recur (2-tuple-first t1) (2-tuple-first t2))
          (equal?-recur (2-tuple-second t1) (2-tuple-second t2))))
   (define (hash-proc my-tuple hash-recur)
     (+ (hash-recur (2-tuple-first my-tuple))
        (* 3 (hash-recur (2-tuple-second my-tuple)))))
   (define (hash2-proc my-tuple hash2-recur)
     (+ (hash2-recur (2-tuple-first my-tuple))
        (hash2-recur (2-tuple-second my-tuple))))])
(provide (struct-out 2-tuple))

(define (2-tupleof type-predicate1 type-predicate2)
  (λ (elem)
    (and (2-tuple? elem)
         (type-predicate1 (2-tuple-first elem))
         (type-predicate2 (2-tuple-second elem)))))
(provide 2-tupleof)

; may want to create a set utils module?
(define (optional-set-union . sets)
  (foldl (λ (el acc) (set-union el acc)) (set) sets))
(provide optional-set-union)