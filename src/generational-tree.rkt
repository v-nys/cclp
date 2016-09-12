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
(require typed-racket-tree-utils/tree)
(require "typed-abstract-knowledge.rkt")
(require "typed-abstract-multi-domain.rkt")

; an 'input' branch is a list of these guys
(struct resolution-info ([conjunction : AbstractConjunction] [selection : Integer] [clause : AbstractKnowledge]))
(provide (struct-out resolution-info))

(struct atom-with-generation ([atom : AbstractConjunct] [generation : Integer]))
(provide (struct-out atom-with-generation))

(: clause-output-length (-> AbstractKnowledge Integer))
(define (clause-output-length clause)
  (match clause
    [(rule h b) (length b)]
    [(full-evaluation i o) 0]))

; empty branch would be a contract violation, take care of that...
(: generational-tree (-> (Listof resolution-info) (Listof (node atom-with-generation))))
(define (generational-tree branch)
  (match branch [(list res-info) (map (λ ([atom-in-conjunction : AbstractConjunct]) (node (atom-with-generation atom-in-conjunction 0) '())) (resolution-info-conjunction res-info))]
    [(list-rest res-info res-info-rest)
     (let* ([selected (resolution-info-selection res-info)]
            [first-unselected (take (resolution-info-conjunction res-info) selected)]
            [selected-atom (list-ref (resolution-info-conjunction res-info) (resolution-info-selection res-info))]
            [last-unselected (drop (resolution-info-conjunction res-info) (+ 1 selected))]
            [next-layer (generational-tree res-info-rest)]
            [first-successors (take next-layer selected)]
            [selected-successors (take next-layer (clause-output-length (resolution-info-clause res-info)))]
            [last-successors (drop next-layer (+ selected (clause-output-length (resolution-info-clause res-info))))])
       (append (map (λ ([pre : AbstractConjunct] [post : (node atom-with-generation)]) (node (atom-with-generation pre 0) (list post))) first-unselected first-successors)
               (list (node (atom-with-generation selected-atom 0) selected-successors))
               (map (λ ([pre : AbstractConjunct] [post : (node atom-with-generation)]) (node (atom-with-generation pre 0) (list post))) last-unselected last-successors)))]))



(provide generational-tree)
