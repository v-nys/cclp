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
(require racket-tree-utils/src/tree)
(require "abstract-knowledge.rkt")
(require "abstract-multi-domain.rkt")
(require "data-utils.rkt")

(define (write-atom-with-generation obj port mode)
  (if (boolean? mode)
      (fprintf port "#(struct:atom-with-generation ~s ~s)" (atom-with-generation-atom obj) (atom-with-generation-generation obj))
      (begin (fprintf port "~v" (atom-with-generation-atom obj))
             (fprintf port ";")
             (fprintf port "~v" (atom-with-generation-generation obj)))))

(struct resolution-info (conjunction selection-and-clause))
; TODO add contract
; conjunction is a (non-empty-listof abstract-atom)
; selection-and-clause is an Opt pair of Integer, AbstractKnowledge, so (or/c none? (someof (cons/c Integer abstract-knowledge?))
;the Integer is at least 0 and lower than the list length
(provide (struct-out resolution-info))

; TODO add contract
; atom is just an abstract atom, generation is at least 0
(struct atom-with-generation (atom generation)
  #:methods
  gen:custom-write [(define write-proc write-atom-with-generation)]
  #:methods
  gen:equal+hash
  [(define (equal-proc a1 a2 equal?-recur)
     (and (equal?-recur (atom-with-generation-atom a1) (atom-with-generation-atom a2))
          (equal?-recur (atom-with-generation-generation a1) (atom-with-generation-generation a2))))
   (define (hash-proc my-awg hash-recur)
     (+ (hash-recur (atom-with-generation-atom my-awg))
        (hash-recur (atom-with-generation-generation my-awg))))
   (define (hash2-proc my-awg hash2-recur)
     (+ (hash2-recur (atom-with-generation-atom my-awg))
        (hash2-recur (atom-with-generation-generation my-awg))))])
(provide (struct-out atom-with-generation))

;(: clause-output-length (-> AbstractKnowledge Integer))
; could use define/contract for extra information
(define (clause-output-length clause)
  (match clause
    [(rule h b) (length b)]
    [(full-evaluation i o) 0]))

(define (generational-tree branch)
  (match branch
    [(list res-info) (map (λ (atom-in-conjunction) (node (atom-with-generation atom-in-conjunction 0) '())) (resolution-info-conjunction res-info))]
    [(list-rest (resolution-info res-conjunction (some (cons selected clause-used))) res-info-rest)
     (let* ([first-unselected (take res-conjunction selected)]
            [selected-atom (list-ref res-conjunction selected)]
            [last-unselected (drop res-conjunction (+ 1 selected))]
            [next-layer (generational-tree res-info-rest)]
            [first-successors (take next-layer selected)]
            [selected-successors (take next-layer (clause-output-length clause-used))]
            [last-successors (drop next-layer (+ selected (clause-output-length clause-used)))])
       (append (map (λ (pre post) (node (atom-with-generation pre 0) (list post))) first-unselected first-successors)
               (list (node (atom-with-generation selected-atom 0) selected-successors))
               (map (λ (pre post) (node (atom-with-generation pre 0) (list post))) last-unselected last-successors)))]))

; can refine this further:
; first resolution-info should have an atomic query
; not having a selection-and-clause and having a successor list element would also be a violation
; +vice versa
(provide (contract-out
          [generational-tree (-> (non-empty-listof resolution-info?) (listof (nodeof atom-with-generation?)))]))