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