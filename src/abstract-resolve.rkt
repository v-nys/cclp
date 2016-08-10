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

#lang typed/racket
(require "abstract-knowledge.rkt")
(require "abstract-substitution.rkt")
(require "abstract-unify.rkt")
(require "data-utils.rkt")

; NOTE: this does not rename the knowledge!
(: abstract-unfold (-> AbstractConjunct AbstractKnowledge (Opt (Pairof AbstractSubstitution AbstractConjunction))))
(define (abstract-unfold conjunct knowledge)
  (cond [(rule? knowledge)
         (let* ([in-subst (abstract-equality conjunct (rule-head knowledge))]
                [out-subst (abstract-unify in-subst)])
           (if (some? out-subst) (cons (some-v out-subst) (apply-substitution-to-conjunction (some-v out-subst) (rule-body knowledge))) (none)))]
        [(full-evaluation? knowledge)
         (if (>=-extension (input-pattern knowledge) conjunct)
             (let* ([in-subst (abstract-equality conjunct (output-pattern knowledge))]
                    [out-subst (abstract-unify in-subst)])
               (if (some? out-subst) (cons (some-v out-subst) empty)))
             (none))]))
  
; note that there are two types of knowledge: rules and input pattern - output pattern pairs
; for pattern pair? the input pair has to be at least as general as the selected conjunct; no conjunction is returned, so which substitution do we get? that produced by unifying with the output pattern?