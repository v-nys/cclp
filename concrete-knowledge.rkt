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
(require "concrete-domain.rkt")
(require racket/serialize)
(serializable-struct rule (head body [idx #:auto])
                     #:auto-value 1
                     #:transparent
                     #:methods
                     gen:equal+hash
                     [(define (equal-proc r1 r2 equal?-recur)
                        (and (equal?-recur (rule-head r1) (rule-head r2))
                             (equal?-recur (rule-body r1) (rule-body r2))
                             (equal?-recur (rule-idx r1) (rule-idx r2))))
                      (define (hash-proc my-rule hash-recur)
                        (+ (hash-recur (rule-head my-rule))
                           (* 3 (hash-recur (rule-body my-rule)))
                           (* 5 (hash-recur (rule-idx my-rule)))))
                      (define (hash2-proc my-rule hash2-recur)
                        (+ (hash2-recur (rule-head my-rule))
                           (hash2-recur (rule-body my-rule))
                           (hash2-recur (rule-idx my-rule))))])
(provide (struct-out rule))