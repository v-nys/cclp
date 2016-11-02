#lang racket

(require racket/serialize)
(require racket/string)

(serializable-struct none ()
  #:methods
  gen:equal+hash
  [(define (equal-proc n1 n2 equal?-recur) #t)
   (define (hash-proc my-none hash-recur) 357)
   (define (hash2-proc my-none hash2-recur) 729)])
(provide (struct-out none))

(serializable-struct some (v)
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

(struct 5-tuple (first second third fourth fifth)
  #:methods
  gen:equal+hash
  [(define (equal-proc t1 t2 equal?-recur)
     (and (equal?-recur (5-tuple-first t1) (5-tuple-first t2))
          (equal?-recur (5-tuple-second t1) (5-tuple-second t2))
          (equal?-recur (5-tuple-third t1) (5-tuple-third t2))
          (equal?-recur (5-tuple-fourth t1) (5-tuple-fourth t2))
          (equal?-recur (5-tuple-fourth t1) (5-tuple-fourth t2))))
   (define (hash-proc my-tuple hash-recur)
     (+ (hash-recur (5-tuple-first my-tuple))
        (* 3 (hash-recur (5-tuple-second my-tuple)))
        (* 7 (hash-recur (5-tuple-third my-tuple)))
        (* 11 (hash-recur (5-tuple-fourth my-tuple)))
        (* 13 (hash-recur (5-tuple-fifth my-tuple)))))
   (define (hash2-proc my-tuple hash2-recur)
     (+ (hash2-recur (5-tuple-first my-tuple))
        (hash2-recur (5-tuple-second my-tuple))
        (hash2-recur (5-tuple-third my-tuple))
        (hash2-recur (5-tuple-fourth my-tuple))
        (hash2-recur (5-tuple-fifth my-tuple))))]
  #:transparent)
(provide (struct-out 5-tuple))

(define (5-tupleof type-predicate1 type-predicate2 type-predicate3 type-predicate4 type-predicate5)
  (λ (elem)
    (and (5-tuple? elem)
         (type-predicate1 (5-tuple-first elem))
         (type-predicate2 (5-tuple-second elem))
         (type-predicate3 (5-tuple-third elem))
         (type-predicate4 (5-tuple-fourth elem))
         (type-predicate5 (5-tuple-fifth elem)))))
(provide 5-tupleof)

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