; MIT License
;
; Copyright (c) 2016-2018 Vincent Nys
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
(require cclp-common-data/abstract-multi-domain)

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
       
(struct abstract-rule (head body)
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

(struct full-evaluation (input-pattern output-pattern idx)
                     #:transparent
                     #:methods
                     gen:custom-write [(define write-proc write-full-eval)])
(provide (struct-out full-evaluation))

(define (abstract-knowledge? k) (or (abstract-rule? k) (full-evaluation? k) (full-ai-rule? k)))
(provide abstract-knowledge?)

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