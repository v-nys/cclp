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
(serializable-struct
 variable (name)
 #:methods
 gen:equal+hash
 [(define (equal-proc v1 v2 equal?-recur)
    (equal?-recur (variable-name v1) (variable-name v2)))
  (define (hash-proc my-variable hash-recur)
    (hash-recur (variable-name my-variable)))
  (define (hash2-proc my-variable hash2-recur)
    (hash2-recur (variable-name my-variable)))]
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'variable)
     (λ (obj) (list (variable-name obj)))))])
(provide
 (struct*-doc
  variable
  ([name symbol?])
  @{A variable in the concrete domain.}))

(serializable-struct
 function (functor args)
 #:methods
 gen:equal+hash
 [(define (equal-proc f1 f2 equal?-recur)
    (and (equal?-recur (function-functor f1) (function-functor f2))
         (equal?-recur (function-args f1) (function-args f2))))
  (define (hash-proc my-function hash-recur)
    (+ (hash-recur (function-functor my-function))
       (* 3 (hash-recur (function-args my-function)))))
  (define (hash2-proc my-function hash2-recur)
    (+ (hash2-recur (function-functor my-function))
       (hash2-recur (function-args my-function))))]
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'function)
     (λ (obj) (list (function-functor obj)
                    (function-args obj)))))])
(provide
 (struct*-doc
  function
  ([functor symbol?] [args (listof term?)])
  @{A function in the concrete domain.}))

(define (term? t)
  (or (variable? t) (function? t)))
(provide term?)

(serializable-struct
 atom (symbol args)
 #:methods
 gen:equal+hash
 [(define (equal-proc a1 a2 equal?-recur)
    (and (equal?-recur (atom-symbol a1) (atom-symbol a2))
         (equal?-recur (atom-args a1) (atom-args a2))))
  (define (hash-proc my-atom hash-recur)
    (+ (hash-recur (atom-symbol my-atom))
       (* 3 (hash-recur (atom-args my-atom)))))
  (define (hash2-proc my-atom hash2-recur)
    (+ (hash2-recur (atom-symbol my-atom))
       (hash2-recur (atom-args my-atom))))]
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'atom)
     (λ (obj) (list (atom-symbol obj) (atom-args obj)))))])
(provide
 (struct*-doc
  atom
  ([symbol symbol?] [args (listof term?)])
  @{An atom in the concrete domain.}))

(define (concrete-domain-elem? elem)
  (or (term? elem)
      (atom? elem)
      ((listof atom?) elem)))
(provide
 (proc-doc/names
  concrete-domain-elem?
  (-> any/c boolean?)
  (val)
  @{Test whether @racket[val] is an element of the concrete domain.}))