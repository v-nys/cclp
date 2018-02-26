#lang at-exp racket
(require
  scribble/srcdoc
  (for-doc scribble/manual)
  cclp-analysis/gen-graph-structs)

(struct clustering (subclusters gcd)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; compare a and b
     (and (equal?-recur (clustering-subclusters a) (clustering-subclusters b))
          (equal?-recur (clustering-gcd a) (clustering-gcd b))))
   (define (hash-proc a hash-recur)
     ; compute primary hash code of a
     (+ (hash-recur (clustering-subclusters a))
        (* 3 (hash-recur (clustering-gcd a)))))
   (define (hash2-proc a hash2-recur)
     ; compute secondary hash code of a
     (+ (hash2-recur (clustering-subclusters a))
        (hash2-recur (clustering-gcd a))))])
(provide
 (struct*-doc
  clustering
  ([subclusters (or/c (set/c clustering?) gen-node?)]
   [gcd exact-positive-integer?])
  @{A grouping of @racket[gen-node?] structs @racket[subclusters] that have a common ancestor encoded as @racket[gcd].}))