; MIT License
;
; Copyright (c) 2016 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#lang at-exp racket
(require racket/serialize
         racket/struct
         scribble/srcdoc)
(require (for-doc scribble/manual))

(module+ test (require rackunit))

(serializable-struct
 a (index)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'a)
     (λ (obj) (list (a-index obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc a1 a2 equal?-recur)
    (equal?-recur (a-index a1) (a-index a2)))
  (define (hash-proc my-a hash-recur)
    (hash-recur (a-index my-a)))
  (define (hash2-proc my-a hash2-recur)
    (hash2-recur (a-index my-a)))])
(provide
 (struct*-doc
  a
  ([index exact-positive-integer?])
  @{An abstract "any" variable.}))

(serializable-struct
 a* (multi-id atom-index local-index)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'a*)
     (λ (obj) (list (a*-multi-id obj) (a*-atom-index obj) (a*-local-index obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc a*1 a*2 equal?-recur)
    (and (equal?-recur (a*-multi-id a*1) (a*-multi-id a*2))
         (equal?-recur (a*-atom-index a*1) (a*-atom-index a*2))
         (equal?-recur (a*-local-index a*1) (a*-local-index a*2))))
  (define (hash-proc my-a* hash-recur)
    (+ (hash-recur (a*-multi-id my-a*))
       (hash-recur (a*-atom-index my-a*))
       (hash-recur (a*-local-index my-a*))))
  (define (hash2-proc my-a* hash2-recur)
    (+ (hash2-recur (a*-multi-id my-a*))
       (hash2-recur (a*-atom-index my-a*))
       (hash2-recur (a*-local-index my-a*))))])
(provide
 (struct*-doc
  a*
  ([multi-id exact-positive-integer?] [atom-index (or/c 1 'L 'i 'i+1)] [local-index exact-positive-integer?])
  @{A template for abstract "any" variables inside a multi abstraction.}))

(serializable-struct
 g (index)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'g)
     (λ (obj) (list (g-index obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc g1 g2 equal?-recur)
    (equal?-recur (g-index g1) (g-index g2)))
  (define (hash-proc my-g hash-recur)
    (hash-recur (g-index my-g)))
  (define (hash2-proc my-g hash2-recur)
    (hash2-recur (g-index my-g)))])
(provide
 (struct*-doc
  g
  ([index exact-positive-integer?])
  @{An abstract "ground" variable.}))

(serializable-struct
 g* (multi-id atom-index local-index)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'g*)
     (λ (obj) (list (g*-multi-id obj) (g*-atom-index obj) (g*-local-index obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc g*1 g*2 equal?-recur)
    (and (equal?-recur (g*-multi-id g*1) (g*-multi-id g*2))
         (equal?-recur (g*-atom-index g*1) (g*-atom-index g*2))
         (equal?-recur (g*-local-index g*1) (g*-local-index g*2))))
  (define (hash-proc my-g* hash-recur)
    (+ (hash-recur (g*-multi-id my-g*))
       (hash-recur (g*-atom-index my-g*))
       (hash-recur (g*-local-index my-g*))))
  (define (hash2-proc my-g* hash2-recur)
    (+ (hash2-recur (g*-multi-id my-g*))
       (hash2-recur (g*-atom-index my-g*))
       (hash2-recur (g*-local-index my-g*))))])
(provide
 (struct*-doc
  g*
  ([multi-id exact-positive-integer?] [atom-index (or/c 1 'L 'i 'i+1)] [local-index exact-positive-integer?])
  @{A template for abstract "ground" variables inside a multi abstraction.}))

(define (abstract-variable? v)
  (or (a? v) (g? v)))
(provide
 (proc-doc/names
  abstract-variable?
  (-> any/c boolean?)
  (val)
  @{Test whether @racket[val] is an abstract variable.}))

(define (abstract-variable*? v)
  (or (a*? v) (g*? v)))
(provide
 (proc-doc/names
  abstract-variable*?
  (-> any/c boolean?)
  (val)
  @{Test whether @racket[val] is a template for an abstract variable.}))

(define (avar-index v)
  (match v
    [(a i) i]
    [(g i) i]))
(provide
 (proc-doc/names
  avar-index
  (-> abstract-variable? exact-positive-integer?)
  (var)
  @{Extract the index from @racket[var].}))

(serializable-struct
 abstract-function (functor args)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'abstract-function)
     (λ (obj) (list (abstract-function-functor obj)
                    (abstract-function-args obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc af1 af2 equal?-recur)
    (and (equal?-recur (abstract-function-functor af1) (abstract-function-functor af2))
         (equal?-recur (abstract-function-args af1) (abstract-function-args af2))))
  (define (hash-proc af hash-recur)
    (+ (hash-recur (abstract-function-functor af))
       (hash-recur (abstract-function-args af))))
  (define (hash2-proc af hash2-recur)
    (+ (hash2-recur (abstract-function-functor af))
       (hash2-recur (abstract-function-args af))))])
(provide
 (struct*-doc
  abstract-function
  ([functor symbol?] [args (listof abstract-term?)])
  @{Abstract counterpart of a function.}))

(serializable-struct
 abstract-function* (functor args)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'abstract-function*)
     (λ (obj) (list (abstract-function*-functor obj)
                    (abstract-function*-args obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc af*1 af*2 equal?-recur)
    (and (equal?-recur (abstract-function*-functor af*1) (abstract-function*-functor af*2))
         (equal?-recur (abstract-function*-args af*1) (abstract-function*-args af*2))))
  (define (hash-proc af* hash-recur)
    (+ (hash-recur (abstract-function*-functor af*))
       (hash-recur (abstract-function*-args af*))))
  (define (hash2-proc af* hash2-recur)
    (+ (hash2-recur (abstract-function*-functor af*))
       (hash2-recur (abstract-function*-args af*))))])
(provide
 (struct*-doc
  abstract-function*
  ([functor symbol?] [args (listof abstract-term*?)])
  @{A template for abstract functions occurring inside a multi abstraction.}))

(serializable-struct
 abstract-atom (symbol args)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'abstract-atom)
     (λ (obj) (list (abstract-atom-symbol obj) (abstract-atom-args obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc aa1 aa2 equal?-recur)
    (and (equal?-recur (abstract-atom-symbol aa1) (abstract-atom-symbol aa2))
         (equal?-recur (abstract-atom-args aa1) (abstract-atom-args aa2))))
  (define (hash-proc aa hash-recur)
    (+ (hash-recur (abstract-atom-symbol aa))
       (hash-recur (abstract-atom-args aa))))
  (define (hash2-proc aa hash2-recur)
    (+ (hash2-recur (abstract-atom-symbol aa))
       (hash2-recur (abstract-atom-args aa))))])
(provide
 (struct*-doc
  abstract-atom
  ([symbol symbol?] [args (listof abstract-term?)])
  @{Abstract counterpart of an atom.}))

(serializable-struct
 abstract-atom* (symbol args)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'abstract-atom*)
     (λ (obj) (list (abstract-atom*-symbol obj) (abstract-atom*-args obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc aa*1 aa*2 equal?-recur)
    (and (equal?-recur (abstract-atom*-symbol aa*1) (abstract-atom*-symbol aa*2))
         (equal?-recur (abstract-atom*-args aa*1) (abstract-atom*-args aa*2))))
  (define (hash-proc aa* hash-recur)
    (+ (hash-recur (abstract-atom*-symbol aa*))
       (hash-recur (abstract-atom*-args aa*))))
  (define (hash2-proc aa* hash2-recur)
    (+ (hash2-recur (abstract-atom*-symbol aa*))
       (hash2-recur (abstract-atom*-args aa*))))])
(provide
 (struct*-doc
  abstract-atom*
  ([symbol symbol?] [args (listof abstract-term*?)])
  @{A template for abstract atoms occurring inside a multi abstraction.}))

(serializable-struct
 multi (conjunction ascending? init consecutive final)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'multi)
     (λ (obj) (list (multi-conjunction obj)
                    (multi-ascending? obj)
                    (multi-init obj)
                    (multi-consecutive obj)
                    (multi-final obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc m1 m2 equal?-recur)
    (and (equal?-recur (multi-conjunction m1) (multi-conjunction m2))
         (equal?-recur (multi-ascending? m1) (multi-ascending? m2))
         (equal?-recur (multi-init m1) (multi-init m2))
         (equal?-recur (multi-consecutive m1) (multi-consecutive m2))
         (equal?-recur (multi-final m1) (multi-final m2))))
  (define (hash-proc m hash-recur)
    (+ (hash-recur (multi-conjunction m))
       (hash-recur (multi-ascending? m))
       (hash-recur (multi-init m))
       (hash-recur (multi-consecutive m))
       (hash-recur (multi-final m))))
  (define (hash2-proc m hash2-recur)
    (+ (hash2-recur (multi-conjunction m))
       (hash2-recur (multi-ascending? m))
       (hash2-recur (multi-init m))
       (hash2-recur (multi-consecutive m))
       (hash2-recur (multi-final m))))])
(provide
 (struct*-doc
  multi
  ([conjunction (listof abstract-atom*?)]
   [ascending? boolean?]
   [init init?]
   [consecutive consecutive?]
   [final final?])
  @{The multi abstraction.}))

(serializable-struct
 init (constraints)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'init)
     (λ (obj) (list (init-constraints obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc i1 i2 equal?-recur)
    (equal?-recur (init-constraints i1) (init-constraints i2)))
  (define (hash-proc i hash-recur)
    (hash-recur (init-constraints i)))
  (define (hash2-proc i hash2-recur)
    (hash2-recur (init-constraints i)))])
(provide
 (struct*-doc
  init
  ([constraints (listof (cons/c abstract-variable*? abstract-term?))])
  @{Constraints pertaining to the first (in terms of recursion depth) abstracted abstract conjunction in a multi.}))

(serializable-struct
 consecutive (constraints)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'consecutive)
     (λ (obj) (list (consecutive-constraints obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc c1 c2 equal?-recur)
    (equal?-recur (consecutive-constraints c1) (consecutive-constraints c2)))
  (define (hash-proc c hash-recur)
    (hash-recur (consecutive-constraints c)))
  (define (hash2-proc c hash2-recur)
    (hash2-recur (consecutive-constraints c)))])
(provide
 (struct*-doc
  consecutive
  ([constraints (listof (cons/c abstract-variable*? abstract-term*?))])
  @{Constraints pertaining to consecutive (in terms of recursion depth) abstracted abstract conjunctions in a multi.}))

(serializable-struct
 final (constraints)
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'final)
     (λ (obj) (list (final-constraints obj)))))]
 #:methods
 gen:equal+hash
 [(define (equal-proc f1 f2 equal?-recur)
    (equal?-recur (final-constraints f1) (final-constraints f2)))
  (define (hash-proc f hash-recur)
    (hash-recur (final-constraints f)))
  (define (hash2-proc f hash2-recur)
    (hash2-recur (final-constraints f)))])
(provide
 (struct*-doc
  final
  ([constraints (listof (cons/c abstract-variable*? abstract-variable?))])
  @{Constraints pertaining to the final (in terms of recursion depth) abstracted abstract conjunction in a multi.}))

(define (abstract-term? elem)
  (or (abstract-variable? elem) (abstract-function? elem)))
(provide
 (proc-doc/names
  abstract-term?
  (-> any/c boolean?)
  (val)
  @{Test whether @racket[val] is an abstract term.}))

(define (abstract-term*? elem)
  (or (abstract-variable*? elem) (abstract-function*? elem)))
(provide
 (proc-doc/names
  abstract-term*?
  (-> any/c boolean?)
  (val)
  @{Test whether @racket[val] is a template for an abstract term.}))

; TODO: consider including multi abstraction, maybe its component parts as well
(define (abstract-domain-elem? elem)
  (or (abstract-atom? elem)
      (abstract-term? elem)
      ((listof abstract-atom?) elem)))
(provide
 (proc-doc/names
  abstract-domain-elem?
  (-> any/c boolean?)
  (val)
  @{Test whether @racket[val] is an element of the abstract domain.}))