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

(require
  racket/struct
  scribble/srcdoc
  "abstract-multi-domain.rkt")
(require (for-doc scribble/manual))

(struct generation (number origin)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'generation)
      (λ (obj) (list (generation-number obj)
                     (generation-origin obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur)
     (and (equal?-recur (generation-number g1)
                        (generation-number g2))
          (equal?-recur (generation-origin g1)
                        (generation-origin g2))))
   (define (hash-proc my-gen hash-recur)
     (+ (hash-recur (generation-number my-gen))
        (hash-recur (generation-origin my-gen))))
   (define (hash2-proc my-gen hash-recur)
     (+ (hash-recur (generation-number my-gen))
        (hash-recur (generation-origin my-gen))))])
(provide
 (struct*-doc
  generation
  ([number (or/c exact-nonnegative-integer? (cons/c symbol? exact-integer?))]
   [origin (or/c #f exact-nonnegative-integer?)])
  @{Used to track the recursion depth of an atom with respect to a uniquely identified target atom.}))

(struct identified-abstract-conjunct-with-gen-range (id-conjunct range))
(provide (struct-out identified-abstract-conjunct-with-gen-range))

(struct identified-abstract-conjunct (conjunct id-number) #:transparent)
(provide (struct-out identified-abstract-conjunct))

(struct gen-range (first last origin) #:transparent)
(provide (struct-out gen-range))

(struct symsum (sym num))
(provide (struct-out symsum))

(struct identified-atom-with-generation (id-atom generation)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'identified-atom-with-generation)
      (λ (obj) (list (identified-atom-with-generation-id-atom obj)
                     (identified-atom-with-generation-generation obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc a1 a2 equal?-recur)
     (and (equal?-recur (identified-atom-with-generation-id-atom a1)
                        (identified-atom-with-generation-id-atom a2))
          (equal?-recur (identified-atom-with-generation-generation a1)
                        (identified-atom-with-generation-generation a2))))
   (define (hash-proc my-awg hash-recur)
     (+ (hash-recur (identified-atom-with-generation-id-atom my-awg))
        (hash-recur (identified-atom-with-generation-generation my-awg))))
   (define (hash2-proc my-awg hash2-recur)
     (+ (hash2-recur (identified-atom-with-generation-id-atom my-awg))
        (hash2-recur (identified-atom-with-generation-generation my-awg))))])
(provide
 (struct*-doc
  identified-atom-with-generation
  ([id-atom identified-atom?]
   [generation generation?])
  @{The label of an atom vertex in a generational graph.
     The field @racket[atom] is a single identified atom which,
     juxtaposed with other @racket[identified-atom]s at the same depth,
     forms an abstract conjunction encountered during analysis.
     The field @racket[generation] indicates how many recursive unfoldings of the target atom took place before @racket[atom] was introduced.}))

(struct identified-atom (atom uid)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'identified-atom)
      (λ (obj) (list (identified-atom-atom obj)
                     (identified-atom-uid obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc a1 a2 equal?-recur)
     (and (equal?-recur (identified-atom-atom a1)
                        (identified-atom-atom a2))
          (equal?-recur (identified-atom-uid a1)
                        (identified-atom-uid a2))))
   (define (hash-proc my-ida hash-recur)
     (+ (hash-recur (identified-atom-atom my-ida))
        (hash-recur (identified-atom-uid my-ida))))
   (define (hash2-proc my-ida hash2-recur)
     (+ (hash2-recur (identified-atom-atom my-ida))
        (hash2-recur (identified-atom-uid my-ida))))])
(provide
 (struct*-doc
  identified-atom
  ([atom abstract-atom?]
   [uid exact-nonnegative-integer?])
  @{A uniquely identifiable instance of an abstract atom in a generational graph (or skeleton).
     The field @racket[uid] is unique to each atom in a generational graph.}))

(struct index-range (start end-before))
(provide (struct-out index-range))