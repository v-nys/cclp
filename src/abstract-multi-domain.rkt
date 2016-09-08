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
(require racket/struct) ; for nicer struct output
;(define (write-a structure port mode)
;  ; unquoted - quoted
;  ; 
;  (cond [(eq? mode 0) (write (string->symbol (string-append "a" (a-index structure))) port)]
;        [(eq? mode 1) (write (string->symbol (string-append "a" (a-index structure))) port)]
;        [otherwise ] ; write and display work the same way, we don't need escapes,...

;      (print (string-append "a" (number->string (a-index structure))) port)
;      (error (format "unsupported write method for a: ~a" mode))))

; TODO investigate whether transparency is a good idea, may be better to get rid of it?

(struct a (index) #:transparent #:methods gen:custom-write [(define write-proc (make-constructor-style-printer (lambda (obj) 'a) (lambda (obj) (list (a-index obj)))))])
(provide (struct-out a))
(struct g (index) #:transparent #:methods gen:custom-write [(define write-proc (make-constructor-style-printer (lambda (obj) 'g) (lambda (obj) (list (g-index obj)))))])
(provide (struct-out g))

(require "data-utils.rkt") ; for optional set union

(define (avar-index avar)
  (cond [(a? avar) (a-index avar)]
        [(g? avar) (g-index avar)]))
(provide avar-index)

(struct abstract-function (functor args) #:transparent )
(provide (struct-out abstract-function))

(struct abstract-atom (symbol args) #:transparent)
(provide (struct-out abstract-atom))