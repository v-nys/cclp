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

(require "abstract-multi-domain.rkt" "abstract-substitution.rkt")
(struct full-ai-rule (input-pattern output-substitution idx)
  #:methods
  gen:equal+hash
  [(define (equal-proc f1 f2 equal?-recur)
     (and (equal?-recur (full-ai-rule-input-pattern f1) (full-ai-rule-input-pattern f2))
          (equal?-recur (full-ai-rule-output-substitution f1) (full-ai-rule-output-substitution f2))
          (equal?-recur (full-ai-rule-idx f1) (full-ai-rule-idx f2))))
   (define (hash-proc my-rule hash-recur)
     (+ (hash-recur (full-ai-rule-input-pattern my-rule))
        (* 3 (hash-recur (full-ai-rule-output-substitution my-rule)))
        (* 5 (hash-recur (full-ai-rule-idx my-rule)))))
   (define (hash2-proc my-rule hash2-recur)
     (+ (hash2-recur (full-ai-rule-input-pattern my-rule))
        (hash2-recur (full-ai-rule-output-substitution my-rule))
        (hash2-recur (full-ai-rule-idx my-rule))))])
(provide (struct-out full-ai-rule))