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

(struct gen (number origin)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'gen)
      (λ (obj) (list (gen-number obj)
                     (gen-origin obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur)
     (and (equal?-recur (gen-number g1)
                        (gen-number g2))
          (equal?-recur (gen-origin g1)
                        (gen-origin g2))))
   (define (hash-proc my-gen hash-recur)
     (+ (hash-recur (gen-number my-gen))
        (hash-recur (gen-origin my-gen))))
   (define (hash2-proc my-gen hash-recur)
     (+ (hash-recur (gen-number my-gen))
        (hash-recur (gen-origin my-gen))))])
(provide
 (struct*-doc
  gen
  ([number (or/c exact-nonnegative-integer? symbol? symsum?)]
   [origin (or/c #f exact-positive-integer?)])
  @{Used to track the recursion depth of an atom with respect to a uniquely identified target atom.}))

(struct gen-range (first last origin ascending?)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'gen-range)
      (λ (obj) (list (gen-range-first obj)
                     (gen-range-last obj)
                     (gen-range-origin obj)
                     (gen-range-ascending? obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur)
     (and (equal?-recur (gen-range-first g1)
                        (gen-range-first g2))
          (equal?-recur (gen-range-last g1)
                        (gen-range-last g2))
          (equal?-recur (gen-range-origin g1)
                        (gen-range-origin g2))
          (equal?-recur (gen-range-ascending? g1)
                        (gen-range-ascending? g2))))
   (define (hash-proc gen hash-recur)
     (+ (hash-recur (gen-range-first gen))
        (hash-recur (gen-range-last gen))
        (hash-recur (gen-range-origin gen))
        (hash-recur (gen-range-ascending? gen))))
   (define (hash2-proc gen hash-recur)
     (+ (hash-recur (gen-range-first gen))
        (hash-recur (gen-range-last gen))
        (hash-recur (gen-range-origin gen))
        (hash-recur (gen-range-ascending? gen))))])
(provide
 (struct*-doc
  gen-range
  ([first (or/c exact-nonnegative-integer? symbol? symsum?)]
   [last (or/c exact-nonnegative-integer? symbol? symsum?)]
   [origin (or/c #f exact-positive-integer?)]
   [ascending? boolean?])
  @{Range of generations represented by a multi abstraction.
     The value @racket[first] represents the generation of the first abstracted conjunct in a syntactic sense.
     The value @racket[last] represents the generation of the last abstracted conjunct in a syntactic sense.
     The value @racket[origin] is the identifier of the abstract atom which first gave rise to the recursion stack.
     Whether the abstracted conjunctions have an ascending or descending sequence of conjunctions is indicated by @racket[ascending?].}))

(struct gen-node (conjunct id range)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'gen-node)
      (λ (obj) (list (gen-node-conjunct obj)
                     (gen-node-id obj)
                     (gen-node-range obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur)
     (and (equal?-recur (gen-node-conjunct g1)
                        (gen-node-conjunct g2))
          (equal?-recur (gen-node-id g1)
                        (gen-node-id g2))
          (equal?-recur (gen-node-range g1)
                        (gen-node-range g2))))
   (define (hash-proc gen hash-recur)
     (+ (hash-recur (gen-node-conjunct gen))
        (hash-recur (gen-node-id gen))
        (hash-recur (gen-node-range gen))))
   (define (hash2-proc gen hash-recur)
     (+ (hash-recur (gen-node-conjunct gen))
        (hash-recur (gen-node-id gen))
        (hash-recur (gen-node-range gen))))])
(provide
 (struct*-doc
  gen-node
  ([conjunct abstract-conjunct?]
   [id exact-positive-integer?]
   [range (or/c #f gen? gen-range?)])
  @{A node in a generational graph.
     A node adds metadata to @racket[conjunct].
     The first piece of metadata, @racket[id], is a unique identifier used to distinguish between occurrences of conjuncts.
     The second, @racket[range], if it is present, is either a single generation (in the case of an abstract atom) or a range of generations (in the case of a multi abstraction).
     A missing value for @racket[range] means that the abstract conjunct has not yet been annotated and is different from a generation with number @racket[0] and origin @racket[#f].}))

(struct symsum (sym num)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'symsum)
      (λ (obj) (list (symsum-sym obj)
                     (symsum-num obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc s1 s2 equal?-recur)
     (and (equal?-recur (symsum-sym s1)
                        (symsum-sym s2))
          (equal?-recur (symsum-num s1)
                        (symsum-num s2))))
   (define (hash-proc s hash-recur)
     (+ (hash-recur (symsum-sym s))
        (hash-recur (symsum-num s))))
   (define (hash2-proc s hash-recur)
     (+ (hash-recur (symsum-sym s))
        (hash-recur (symsum-num s))))])
(provide
 (struct*-doc
  symsum
  ([sym symbol?]
   [num exact-nonnegative-integer?])
  @{A sum of a symbol @racket[sym] and a number @racket[num].
     As symbols are used to represent the maximum generation inside a multi abstraction,
     sums of symbols and numbers can be used to indicate the generation of related conjuncts outside the abstraction.}))

(struct index-range (start end-before)
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'index-range)
      (λ (obj) (list (index-range-start obj)
                     (index-range-end-before obj)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc i1 i2 equal?-recur)
     (and (equal?-recur (index-range-start i1)
                        (index-range-start i2))
          (equal?-recur (index-range-end-before i1)
                        (index-range-end-before i2))))
   (define (hash-proc i hash-recur)
     (+ (hash-recur (index-range-start i))
        (hash-recur (index-range-end-before i))))
   (define (hash2-proc i hash-recur)
     (+ (hash-recur (index-range-start i))
        (hash-recur (index-range-end-before i))))])
(provide
 (struct*-doc
  index-range
  ([start exact-nonnegative-integer?]
   [end-before exact-nonnegative-integer?])
  @{A range of list indices, from @racket[start] up to, but not including @racket[end-before].
 The purpose of this structure is to indicate which elements in a conjunction are abstracted when a generalization operation is applied.}))