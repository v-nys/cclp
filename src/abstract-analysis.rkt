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

#lang at-exp racket
(require (only-in "abstract-multi-domain.rkt" abstract-atom?))
(require "abstract-knowledge.rkt")
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require "abstract-substitution.rkt")
(require scribble/srcdoc)
(require racket/serialize)
(require (for-doc scribble/manual))

(define (label-index l)
  (if (tree-label? l)
      (tree-label-index l)
      (widening-index l)))
(provide label-index)

(define (label-conjunction l)
  (if (tree-label? l)
      (tree-label-conjunction l)
      (widening-conjunction l)))
(provide label-conjunction)

(define (label-selection l)
  (if (tree-label? l)
      (tree-label-selection l)
      (widening-selection l)))
(provide label-selection)


(define (write-tree-label obj port mode)
  (if (eq? mode #t)
      (fprintf
       port
       "#(struct:tree-label ~s ~s ~s ~s ~s)"
       (tree-label-conjunction obj)
       (tree-label-selection obj)
       (tree-label-substitution obj)
       (tree-label-rule obj)
       (tree-label-index obj))
      (fprintf
       port
       "tree label conjunction ~a, with selection ~a, obtained through rule ~a and substitution ~a"
       (tree-label-conjunction obj)
       (tree-label-selection obj)
       (tree-label-substitution obj)
       (tree-label-rule obj))))
(serializable-struct
 tree-label (conjunction selection substitution rule index)
 #:methods
 gen:equal+hash
 [(define (equal-proc l1 l2 equal?-recur)
    (and (equal?-recur (tree-label-conjunction l1) (tree-label-conjunction l2))
         (equal?-recur (tree-label-selection l1) (tree-label-selection l2))
         (equal?-recur (tree-label-substitution l1) (tree-label-substitution l2))
         (equal?-recur (tree-label-rule l1) (tree-label-rule l2))
         (equal?-recur (tree-label-index l1) (tree-label-index l2))))
  (define (hash-proc l hash-recur)
    (+ (hash-recur (tree-label-conjunction l))
       (* 3 (hash-recur (tree-label-selection l)))
       (* 7 (hash-recur (tree-label-substitution l)))
       (* 11 (hash-recur (tree-label-rule l)))
       (* 13 (hash-recur (tree-label-index l)))))
  (define (hash2-proc l hash2-recur)
    (+ (hash2-recur (tree-label-conjunction l))
       (hash2-recur (tree-label-selection l))
       (hash2-recur (tree-label-substitution l))
       (hash2-recur (tree-label-rule l))
       (hash2-recur (tree-label-index l))))]
 #:methods
 gen:custom-write
 [(define write-proc write-tree-label)])
(provide
 (struct*-doc
  tree-label
  ([conjunction (listof abstract-atom?)]
   [selection any/c]
   [substitution abstract-substitution?]
   [rule (or/c #f (or/c full-evaluation? ck:rule?))]
   [index (or/c #f exact-positive-integer?)])
  @{The contents of a node in the abstract analysis tree which has not yet been visited or which was successfully unfolded.
     The field @racket[selection] stands for the index (if any) of the atom selected for unfolding.
     The field @racket[substitution] is the substitution which was applied to the parent and an abstract program clause to obtain @racket[conjunction].
     The field @racket[rule] is the knowledge with which the parent was resolved to obtain @racket[conjunction].
     It is @racket[#f] if the node is not the result of resolution.
     It is a @racket[ck:rule?] if the node is the result of a single unfolding step.
     It is a @racket[full-evaluation?] if the node is the result of a full evaluation step.
     The field @racket[index] is a unique label, assigned so that cycles can be clearly marked.
     It is an integer if the node has been visited and @racket[#f] if the node has not yet been visited.}))

(serializable-struct cycle (index)
                     #:methods
                     gen:equal+hash
                     [(define (equal-proc c1 c2 equal?-recur)
                        (equal?-recur (cycle-index c1) (cycle-index c2)))
                      (define (hash-proc c hash-recur)
                        (hash-recur (cycle-index c)))
                      (define (hash2-proc c hash2-recur)
                        (hash2-recur (cycle-index c)))])
(provide
 (struct*-doc
  cycle
  ([index exact-positive-integer?])
  @{A cycle detected during abstract analysis.
     The field @racket[index] stands for the index of a previously handled conjunction which generalizes over the conjunction which introduces the @racket[cycle].
     The latter is normally represented as the parent of the @racket[cycle] in the abstract analysis tree.}))

(serializable-struct widening (conjunction selection message index)
                     ; message has no bearing on the semantics
                     #:methods
                     gen:equal+hash
                     [(define (equal-proc w1 w2 equal?-recur)
                        (and (equal?-recur (widening-conjunction w1) (widening-conjunction w2))
                             (equal?-recur (widening-selection w1) (widening-selection w2))
                             (equal?-recur (widening-index w1) (widening-index w2))))
                      (define (hash-proc w hash-recur)
                        (+ (hash-recur (widening-conjunction w))
                           (hash-recur (widening-selection w))
                           (hash-recur (widening-index w))))
                      (define (hash2-proc w hash2-recur)
                        (+ (hash2-recur (widening-conjunction w))
                           (hash2-recur (widening-selection w))
                           (hash2-recur (widening-index w))))])
(provide
 (struct*-doc
  widening
  ([conjunction (listof abstract-atom?)]
   [selection any/c]
   [message (or/c #f string?)]
   [index (or/c #f exact-positive-integer?)])
  @{An application of widening during abstract analysis.
     This can be either automatic (due to depth-k abstraction being enabled) or specified by the user.
     The field @racket[conjunction] contains the result of the widening operation.
     The field @racket[selection] works the same way as in a @racket[tree-label].
     The @racket[message] field is optional and is used to explain why widening was applied.
     The @racket[index] field serves the same purpose as that of @racket[tree-label].}))