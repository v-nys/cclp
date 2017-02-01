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
(require
  racket/serialize ; so that analysis trees can be saved/loaded
  racket/struct ; for constructor-style printers
  scribble/srcdoc)
(require
  "abstract-knowledge.rkt"
  (only-in "abstract-multi-domain.rkt" abstract-atom?)
  "abstract-substitution.rkt"
  (prefix-in ck: "concrete-knowledge.rkt")
  "data-utils.rkt" ; selection is still expressed as a maybe
  (prefix-in faid: "fullai-domain.rkt")
  "preprior-graph.rkt")
(require (for-doc scribble/manual))

(define (full-ai-rule->full-evaluation r)
  (full-evaluation
   (faid:full-ai-rule-input-pattern r)
   (apply-substitution (faid:full-ai-rule-output-substitution r) (faid:full-ai-rule-input-pattern r))))
(provide
 (proc-doc/names
  full-ai-rule->full-evaluation
  (-> faid:full-ai-rule? full-evaluation?)
  (rule)
  @{Converts a @racket[faid:full-ai-rule?] to a @racket[full-evaluation?].
 This function should be deprecated ASAP.}))

(serializable-struct
 cycle (index)
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
     The field @racket[index] stands for the index of a previous
     conjunction, generalizing over that which introduces the @racket[cycle].
     The latter is normally represented as the parent of the @racket[cycle].}))

(serializable-struct
 similarity-cycle (index)
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
  similarity-cycle
  ([index exact-positive-integer?])
  @{A cycle detected during abstract analysis, based on similarity (rather than pure renaming).
     The field @racket[index] stands for the index of a previously handled conjunction which is similar to that which introduces the @racket[similarity-cycle].
     The latter is normally represented as the parent of the @racket[similarity-cycle] in the abstract analysis tree.
     This struct should be deprecated ASAP.}))

(serializable-struct
 tree-label (conjunction selection substitution rule index preprior-stack)
 #:methods
 gen:equal+hash
 ; ignore preprior-stack because it is not hashable
 [(define (equal-proc l1 l2 equal?-recur)
    (and (equal?-recur (tree-label-conjunction l1) (tree-label-conjunction l2))
         (equal?-recur (tree-label-selection l1) (tree-label-selection l2))
         (equal?-recur (tree-label-substitution l1) (tree-label-substitution l2))
         (equal?-recur (tree-label-rule l1) (tree-label-rule l2))
         (equal?-recur (tree-label-index l1) (tree-label-index l2))
         (equal?-recur (tree-label-preprior-stack l1) (tree-label-preprior-stack l2))))
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
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'tree-label)
     (λ (obj)
       (list
        (tree-label-conjunction obj)
        (tree-label-selection obj)
        (tree-label-substitution obj)
        (tree-label-rule obj)
        (tree-label-index obj)
        (tree-label-preprior-stack obj)))))])
(provide
 (struct*-doc
  tree-label
  ([conjunction (listof abstract-atom?)]
   [selection any/c]
   [substitution abstract-substitution?]
   [rule (or/c #f (or/c full-evaluation? ck:rule?))]
   [index (or/c #f exact-positive-integer?)]
   [preprior-stack (listof preprior-graph?)])
  @{The contents of a node in the abstract analysis tree which has not yet been visited or which was successfully unfolded.
     The field @racket[selection] stands for the index (if any) of the atom selected for unfolding.
     The field @racket[substitution] is the substitution which was applied to the parent and an abstract program clause to obtain @racket[conjunction].
     The field @racket[rule] is the knowledge with which the parent was resolved to obtain @racket[conjunction].
     It is @racket[#f] if the node is not the result of resolution.
     It is a @racket[ck:rule?] if the node is the result of a single unfolding step.
     It is a @racket[full-evaluation?] if the node is the result of a full evaluation step.
     The field @racket[index] is a unique label, assigned so that cycles can be clearly marked.
     It is an integer if the node has been visited and @racket[#f] if the node has not yet been visited.
     The field @racket[preprior-stack] is a stack,
     representing the partial order established before atom selection,
     as well as after the completion of each child, if any.}))

(serializable-struct
 widening (conjunction selection message index preprior-stack)
 ; message has no bearing on the semantics, so ignore that
 ; ignore preprior-stack because it is not hashable
 #:methods
 gen:equal+hash
 [(define (equal-proc w1 w2 equal?-recur)
    (and (equal?-recur (widening-conjunction w1) (widening-conjunction w2))
         (equal?-recur (widening-selection w1) (widening-selection w2))
         (equal?-recur (widening-index w1) (widening-index w2))
         (equal?-recur (widening-preprior-stack w1) (widening-preprior-stack w2))))
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
   [index (or/c #f exact-positive-integer?)]
   [preprior-stack (listof preprior-graph?)])
  @{An application of widening during abstract analysis.
     This can be either automatic (due to depth-k abstraction being enabled) or specified by the user.
     The field @racket[conjunction] contains the result of the widening operation.
     The field @racket[selection] works the same way as in a @racket[tree-label].
     The @racket[message] field is optional and is used to explain why widening was applied.
     The @racket[index] field serves the same purpose as that of @racket[tree-label].
     The field @racket[preprior-stack] is a stack,
     representing the partial order established before atom selection,
     as well as after the completion of each child, if any.}))

(serializable-struct
 case (conjunction selection index preprior-stack)
 ; ignore preprior-stack because it is not hashable
 #:methods
 gen:equal+hash
 [(define (equal-proc c1 c2 equal?-recur)
    (and (equal?-recur (case-conjunction c1) (case-conjunction c2))
         (equal?-recur (case-selection c1) (case-selection c2))
         (equal?-recur (case-index c1) (case-index c2))
         (equal?-recur (case-preprior-stack c1) (case-preprior-stack c2))))
  (define (hash-proc c hash-recur)
    (+ (hash-recur (case-conjunction c))
       (hash-recur (case-selection c))
       (hash-recur (case-index c))))
  (define (hash2-proc c hash2-recur)
    (+ (hash2-recur (case-conjunction c))
       (hash2-recur (case-selection c))
       (hash2-recur (case-index c))))])
(provide
 (struct*-doc
  case
  ([conjunction (listof abstract-atom?)]
   [selection any/c]
   [index (or/c #f exact-positive-integer?)]
   [preprior-stack (listof preprior-graph?)])
  @{A case in a case split applied during abstract analysis.
     The field @racket[conjunction] contains the more specific case of the parent conjunction.
     The field @racket[selection] works the same way as in a @racket[tree-label].
     The @racket[index] field serves the same purpose as that of @racket[tree-label].
     The field @racket[preprior-stack] is a stack,
     representing the partial order established before atom selection,
     as well as after the completion of each child, if any.}))

(define (label-index l)
  (cond
    [(tree-label? l) (tree-label-index l)]
    [(widening? l) (widening-index l)]
    [(case? l) (case-index l)]))
(provide
 (proc-doc/names
  label-index
  (-> label-with-conjunction? (or/c #f exact-positive-integer?))
  (label)
  @{Extracts the index from any tree label type which has it.}))

(define (label-conjunction l)
  (cond
    [(tree-label? l) (tree-label-conjunction l)]
    [(widening? l) (widening-conjunction l)]
    [(case? l) (case-conjunction l)]))
(provide
 (proc-doc/names
  label-conjunction
  (-> label-with-conjunction? (listof abstract-atom?))
  (label)
  @{Extracts the conjunction from any tree label type which represents it explicitly.}))

(define (label-selection l)
  (cond
    [(tree-label? l) (tree-label-selection l)]
    [(widening? l) (widening-selection l)]
    [(case? l) (case-selection l)]))
(provide
 (proc-doc/names
  label-selection
  (-> label-with-conjunction? (maybe exact-nonnegative-integer?))
  (label)
  @{Extracts the index of the selected atom from any tree label type which has it.}))

(define (label-preprior-stack l)
  (cond
    [(tree-label? l) (tree-label-preprior-stack l)]
    [(widening? l) (widening-preprior-stack l)]
    [(case? l) (case-preprior-stack l)]))
(provide
 (proc-doc/names
  label-preprior-stack
  (-> label-with-conjunction? (maybe exact-nonnegative-integer?))
  (label)
  @{Extracts the stack of @racket[preprior-graph?] from any tree label type which has it.}))

(define (label-with-conjunction? l)
  ; so this excludes cycles and similarity cycles
  (or (tree-label? l) (widening? l) (case? l)))
(provide
 (proc-doc/names
  label-with-conjunction?
  (-> any/c boolean?)
  (label)
  @{Checks whether the supplied value is a type of tree label which contains a conjunction.}))