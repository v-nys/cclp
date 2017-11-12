; MIT License
;
; Copyright (c) 2017 Vincent Nys
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
  cclp/concrete-domain)
(require (for-doc scribble/manual))

(struct concrete-equality (term1 term2)
  #:methods
  gen:equal+hash
  [(define (equal-proc ce1 ce2 equal?-recur)
     (and
      (equal?-recur
       (concrete-equality-term1 ce1)
       (concrete-equality-term1 ce2))
      (equal?-recur
       (concrete-equality-term2 ce1)
       (concrete-equality-term2 ce2))))
   (define (hash-proc ce hash-recur)
     (+ (* 3 (hash-recur (concrete-equality-term1 ce)))
        (* 7 (hash-recur (concrete-equality-term2 ce)))))
   (define (hash2-proc ce hash2-recur)
     (+ (hash2-recur (concrete-equality-term1 ce))
        (hash2-recur (concrete-equality-term2 ce))))]
  #:methods
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'concrete-equality)
      (λ (obj)
        (list
         (concrete-equality-term1 obj)
         (concrete-equality-term2 obj)))))])
(provide
 (struct*-doc
  concrete-equality
  ([term1 (or/c concrete-domain-elem? (listof concrete-domain-elem?))]
   [term2 (or/c concrete-domain-elem? (listof concrete-domain-elem?))])
  @{An equality between two concrete domain elements. A list of these makes up a concrete substitution.}))

(define (substitute substitutee substituter ctxt)
  (define rec (curry substitute substitutee substituter))
  (match ctxt
    [(variable v)
     #:when (eq? v (variable-name substitutee))
     substituter]
    [(function sym args)
     (function sym (map rec args))]
    [(atom sym args)
     (atom sym (map rec args))]
    [(? list?)
     (map rec ctxt)]
    [(concrete-equality t1 t2)
     (concrete-equality (rec t1) (rec t2))]
    [else
     ctxt]))
(provide
 (proc-doc/names
  substitute
  (-> variable? term? (or/c term? atom? list? concrete-equality?) (or/c term? atom? list? concrete-equality?))
  (substitutee substituter ctxt)
  @{Replace @racket[substitutee] with @racket[substituter] inside @racket[ctxt].}))