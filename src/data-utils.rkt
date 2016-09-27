#lang racket

(require racket/string) ; for string-append

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

; NOTE: if I ever need a 4-tuple or higher, it's time for a macro-generating macro

(struct 4-tuple (first second third fourth)
  #:methods
  gen:equal+hash
  [(define (equal-proc t1 t2 equal?-recur)
     (and (equal?-recur (4-tuple-first t1) (4-tuple-first t2))
          (equal?-recur (4-tuple-second t1) (4-tuple-second t2))
          (equal?-recur (4-tuple-third t1) (4-tuple-third t2))
          (equal?-recur (4-tuple-fourth t1) (4-tuple-fourth t2))))
   (define (hash-proc my-tuple hash-recur)
     (+ (hash-recur (4-tuple-first my-tuple))
        (* 3 (hash-recur (4-tuple-second my-tuple)))
        (* 7 (hash-recur (4-tuple-third my-tuple)))
        (* 11 (hash-recur (4-tuple-fourth my-tuple)))))
   (define (hash2-proc my-tuple hash2-recur)
     (+ (hash2-recur (4-tuple-first my-tuple))
        (hash2-recur (4-tuple-second my-tuple))
        (hash2-recur (4-tuple-third my-tuple))
        (hash2-recur (4-tuple-fourth my-tuple))))]
  #:transparent)
(provide (struct-out 4-tuple))

(define (4-tupleof type-predicate1 type-predicate2 type-predicate3 type-predicate4)
  (λ (elem)
    (and (4-tuple? elem)
         (type-predicate1 (4-tuple-first elem))
         (type-predicate2 (4-tuple-second elem))
         (type-predicate3 (4-tuple-third elem))
         (type-predicate4 (4-tuple-fourth elem)))))
(provide 4-tupleof)

; may want to create a set utils module?
(define (optional-set-union . sets)
  (foldl (λ (el acc) (set-union el acc)) (set) sets))
(provide optional-set-union)

; need these because Parenlog doesn't like S-expressions with numbers in them
(define (positive-integer->symbol int)
  (string->symbol (string-append "sym" (number->string int))))
(provide positive-integer->symbol)

(define (symbol->positive-integer sym)
  (string->number (substring (symbol->string sym) 3)))
(provide symbol->positive-integer)