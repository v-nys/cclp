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

(require "io-utils.rkt")
(require "data-utils.rkt")
(require (only-in "concrete-knowledge.rkt" rule?))
(require (only-in "fullai-domain.rkt" full-ai-rule?))
(require (only-in "execution.rkt" priority?))
(require (only-in "abstract-multi-domain.rkt" abstract-atom?))
(require racket-tree-utils/src/tree)

(struct tree-label (conjunction selection substitution rule))


; interactie tijdens analyse vereist: huidige boom, KB, full evals, selectiemechanisme
(define (load-analysis filename) (void))

(define (begin-analysis program-data)
  (match program-data
    [(4-tuple clauses full-evaluations preprior initial-query)
     ; using none for selection because no selection will occur more often
     ; using list for substitution because it is not wrong and is consistent
     ; using #f for rule because this is the only case where there is no associated clause
     (begin (define initial-tree-label (tree-label ((list (4-tuple-fourth program-data)) (none) (list) #f)))
            (define initial-tree (node initial-tree-label (list)))
            (interactive-analysis initial-tree clauses full-evaluations preprior))]))

(define (cclp-run filename program-data)
  (define serialized-filename (path-replace-extension (last (explode-path filename)) ".serializedcclp"))
  (define-values (analysis load quit) (values "analyze this program" "load existing analysis" "quit"))
  (define choice (prompt-for-answer "What do you want to do?" analysis load quit))
  (cond [(equal? choice analysis) (begin-analysis program-data)]
        [(equal? choice load) (load-analysis serialized-filename)]
        [(equal? choice quit) (void)]))
(provide (contract-out
          [cclp-run
           (-> path?
               (4-tupleof (listof rule?) (listof full-ai-rule?) (listof priority?) abstract-atom?)
               void?)]))