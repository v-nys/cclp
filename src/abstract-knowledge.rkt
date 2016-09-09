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

(define (write-rule obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:rule ~s ~s)" (rule-head obj) (rule-body obj))
      (begin (fprintf port "~v" (rule-head obj))
             (fprintf port " :- ")
             (for ([atom-or-comma (add-between (rule-body obj) ",")]) (if (string? atom-or-comma) (fprintf port atom-or-comma) (fprintf port "~v" atom-or-comma)))
             (fprintf port "."))))
       
(struct rule (head body) #:transparent #:methods gen:custom-write [(define write-proc write-rule)])
(provide (struct-out rule))

(define (write-full-eval obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:full-evaluation ~s ~s)" (full-evaluation-input-pattern obj) (full-evaluation-output-pattern obj))
      (begin (fprintf port "~v" (full-evaluation-input-pattern obj))
             (fprintf port " -> ")
             (fprintf port "~v" (full-evaluation-output-pattern obj))
             (fprintf port "."))))

(struct full-evaluation (input-pattern output-pattern) #:methods gen:custom-write [(define write-proc write-full-eval)])
(provide (struct-out full-evaluation))