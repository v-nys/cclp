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
(require "abstract-multi-domain.rkt")
(require racket/serialize)

(define (write-rule obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:rule ~s ~s)" (abstract-rule-head obj) (abstract-rule-body obj))
      (begin (fprintf port "~v" (abstract-rule-head obj))
             (fprintf port " :- ")
             (for ([atom-or-comma (add-between (abstract-rule-body obj) ",")])
               (if (string? atom-or-comma)
                   (fprintf port atom-or-comma)
                   (fprintf port "~v" atom-or-comma)))
             (fprintf port "."))))
       
(serializable-struct abstract-rule (head body)
  #:methods gen:custom-write
  [(define write-proc write-rule)]
  #:methods gen:equal+hash
  [(define (equal-proc r1 r2 equal?-recur)
     (equal?-recur (abstract-rule-head r1) (abstract-rule-head r2)))
   (define (hash-proc r hash-recur)
     (+ (* (hash-recur (abstract-rule-head r)) 3)
        (* (hash-recur (abstract-rule-body r)) 7)))
   (define (hash2-proc r hash2-recur)
     (+ (hash2-recur (abstract-rule-head r))
        (hash2-recur (abstract-rule-body r))))])
(provide (struct-out abstract-rule))

(define (write-full-eval obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:full-evaluation ~s ~s)" (full-evaluation-input-pattern obj) (full-evaluation-output-pattern obj))
      (begin (fprintf port "~v" (full-evaluation-input-pattern obj))
             (fprintf port " -> ")
             (fprintf port "~v" (full-evaluation-output-pattern obj))
             (fprintf port "."))))

(serializable-struct full-evaluation (input-pattern output-pattern idx)
                     #:transparent
                     #:methods
                     gen:custom-write [(define write-proc write-full-eval)])
(provide (struct-out full-evaluation))

(define (abstract-knowledge? k) (or (abstract-rule? k) (full-evaluation? k)))
(provide abstract-knowledge?)