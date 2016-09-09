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

#lang racket

; TODO investigate whether transparency for structs can be removed?

(define (write-a obj port mode)
  (if (eq? mode #t) (fprintf port "#(struct:a ~s)" (a-index obj)) (fprintf port "a~a" (a-index obj))))
(struct a (index) #:transparent #:methods gen:custom-write [(define write-proc write-a)])
(provide (struct-out a))

(define (write-g obj port mode)
  (if (eq? mode #t) (fprintf port "#(struct:g ~s)" (g-index obj)) (fprintf port "g~a" (g-index obj))))
(struct g (index) #:transparent #:methods gen:custom-write [(define write-proc write-g)])
(provide (struct-out g))

(define (write-abstract-function obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:abstract-function ~s ~s)" (abstract-function-functor obj) (abstract-function-args obj))
      (begin (fprintf port "~a(" (abstract-function-functor obj))
             (for ([arg-or-comma (add-between (abstract-function-args obj) ",")]) (if (string? arg-or-comma) (fprintf port arg-or-comma) (fprintf port "~v" arg-or-comma)))
             (fprintf port ")"))))
(struct abstract-function (functor args) #:transparent #:methods gen:custom-write [(define write-proc write-abstract-function)])
(provide (struct-out abstract-function))

(define (write-abstract-atom obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:abstract-atom ~s ~s)" (abstract-atom-symbol obj) (abstract-atom-args obj))
      (begin (fprintf port "~a(" (abstract-atom-symbol obj))
             (for ([arg-or-comma (add-between (abstract-atom-args obj) ",")]) (if (string? arg-or-comma) (fprintf port arg-or-comma) (fprintf port "~v" arg-or-comma)))
             (fprintf port ")"))))
(struct abstract-atom (symbol args) #:transparent #:methods gen:custom-write [(define write-proc write-abstract-atom)])
(provide (struct-out abstract-atom))