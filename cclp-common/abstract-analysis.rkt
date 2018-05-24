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
  racket/struct ; for constructor-style printers
  scribble/srcdoc)
(require
  cclp-common-data/abstract-knowledge
  (only-in cclp-common-data/abstract-multi-domain abstract-atom? abstract-conjunct?)
  cclp-common-data/abstract-substitution
  "abstract-substitution-application.rkt"
  (prefix-in ck: cclp-common-data/concrete-knowledge)
  "data-utils.rkt" ; selection is still expressed as a maybe
  (only-in "gen-graph-structs.rkt" index-range?)
  (only-in "control-flow.rkt" aif it)
  "preprior-graph.rkt")
(require (for-doc scribble/manual))

(define (full-ai-rule->full-evaluation r)
  (full-evaluation
   (full-ai-rule-input-pattern r)
   (aif (full-ai-rule-output-substitution r)
        (apply-substitution it (full-ai-rule-input-pattern r))
        #f)
   (full-ai-rule-idx r)))
(provide
 (proc-doc/names
  full-ai-rule->full-evaluation
  (-> full-ai-rule? full-evaluation?)
  (rule)
  @{Converts a @racket[full-ai-rule?] to a @racket[full-evaluation?].
 This function should be deprecated ASAP.}))

(struct
 cycle (index)
 #:methods
 gen:equal+hash
 [(define (equal-proc c1 c2 equal?-recur)
    (equal?-recur (cycle-index c1) (cycle-index c2)))
  (define (hash-proc c hash-recur)
    (hash-recur (cycle-index c)))
  (define (hash2-proc c hash2-recur)
    (hash2-recur (cycle-index c)))]
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'cycle)
     (λ (obj)
       (list
        (cycle-index obj)))))])
(provide
 (struct*-doc
  cycle
  ([index exact-positive-integer?])
  @{A cycle detected during abstract analysis.
     The field @racket[index] stands for the index of a previous
     conjunction, generalizing over that which introduces the @racket[cycle].
     The latter is normally represented as the parent of the @racket[cycle].}))

(struct
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
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'tree-label)
     (λ (obj)
       (list
        (tree-label-conjunction obj)
        (tree-label-selection obj)
        (tree-label-substitution obj)
        (tree-label-rule obj)
        (tree-label-index obj)))))])
(provide
 (struct*-doc
  tree-label
  ([conjunction (listof abstract-conjunct?)]
   [selection any/c]
   [substitution abstract-substitution?]
   [rule (or/c #f (or/c full-evaluation? ck:rule? 'one 'many))]
   [index (or/c #f exact-positive-integer?)])
  @{The contents of a node in the abstract analysis tree which has not yet been visited or which was successfully unfolded.
     The field @racket[selection] stands for the index (if any) of the atom selected for unfolding.
     The field @racket[substitution] is the substitution which was applied to the parent and an abstract program clause to obtain @racket[conjunction].
     The field @racket[rule] is the knowledge with which the parent was resolved to obtain @racket[conjunction].
     It is @racket[#f] if the node is not the result of resolution.
     It is a @racket[ck:rule?] if the node is the result of a single unfolding step.
     It is a @racket[full-evaluation?] if the node is the result of a full evaluation step.
     The field @racket[index] is a unique label, assigned so that cycles can be clearly marked.
     It is an integer if the node has been visited and @racket[#f] if the node has not yet been visited.
     It does not track implicit edges (those between more/less general abstract atoms).}))

(struct
 widening (conjunction selection message index)
 ; message has no bearing on the semantics, so ignore that
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
       (hash2-recur (widening-index w))))]
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'widening)
     (λ (obj)
       (list
        (widening-conjunction obj)
        (widening-selection obj)
        (widening-message obj)
        (widening-index obj)))))])
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
     The @racket[index] field serves the same purpose as that of @racket[tree-label].
     It does not track implicit edges (those between more/less general abstract atoms).}))

(struct
 case (conjunction selection index)
 #:methods
 gen:equal+hash
 [(define (equal-proc c1 c2 equal?-recur)
    (and (equal?-recur (case-conjunction c1) (case-conjunction c2))
         (equal?-recur (case-selection c1) (case-selection c2))
         (equal?-recur (case-index c1) (case-index c2))))
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
   [index (or/c #f exact-positive-integer?)])
  @{A case in a case split applied during abstract analysis.
     The field @racket[conjunction] contains the more specific case of the parent conjunction.
     The field @racket[selection] works the same way as in a @racket[tree-label].
     The @racket[index] field serves the same purpose as that of @racket[tree-label].
     It does not track implicit edges (those between more/less general abstract atoms).}))

(struct
 generalization (conjunction selection index abstracted-ranges groupings)
 #:methods
 gen:equal+hash
 [(define (equal-proc l1 l2 equal?-recur)
    (and (equal?-recur (generalization-conjunction l1) (generalization-conjunction l2))
         (equal?-recur (generalization-selection l1) (generalization-selection l2))
         (equal?-recur (generalization-index l1) (generalization-index l2))
         (equal?-recur (generalization-abstracted-ranges l1) (generalization-abstracted-ranges l2))
         (equal?-recur (generalization-groupings l1) (generalization-groupings l2))))
  (define (hash-proc l hash-recur)
    (+ (hash-recur (generalization-conjunction l))
       (* 3 (hash-recur (generalization-selection l)))
       (* 13 (hash-recur (generalization-index l)))
       (* 23 (hash-recur (generalization-abstracted-ranges l)))
       (* 29 (hash-recur (generalization-groupings l)))))
  (define (hash2-proc l hash2-recur)
    (+ (hash2-recur (generalization-conjunction l))
       (hash2-recur (generalization-selection l))
       (hash2-recur (generalization-index l))
       (hash2-recur (generalization-abstracted-ranges l))
       (hash2-recur (generalization-groupings l))))]
 #:methods
 gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (λ (obj) 'generalization)
     (λ (obj)
       (list
        (generalization-conjunction obj)
        (generalization-selection obj)
        (generalization-index obj)
        (generalization-abstracted-ranges obj)
        (generalization-groupings obj)))))])
(provide
 (struct*-doc
  generalization
  ([conjunction (listof abstract-conjunct?)]
   [selection any/c]
   [index (or/c #f exact-positive-integer?)]
   [abstracted-ranges (listof index-range?)]
   [groupings (listof (cons/c (listof index-range?) exact-nonnegative-integer?))])
  @{The contents of a node in the abstract analysis tree which was obtained by grouping conjuncts into a multi abstraction.
     The @racket[groupings] field is a list of pairs consisting of a list of index ranges and a single positive integer.
     The former indicate subconjunctions which make up building blocks in a resulting multi (or refer to an existing multi).
     The latter indicates a multi abstraction at a specific index position in the *resulting* conjunction.}))

(define (label-index l)
  (cond
    [(tree-label? l) (tree-label-index l)]
    [(widening? l) (widening-index l)]
    [(case? l) (case-index l)]
    [(generalization? l) (generalization-index l)]))
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
    [(case? l) (case-conjunction l)]
    [(generalization? l) (generalization-conjunction l)]))
(provide
 (proc-doc/names
  label-conjunction
  (-> label-with-conjunction? (listof abstract-conjunct?))
  (label)
  @{Extracts the conjunction from any tree label type which represents it explicitly.}))

(define (label-selection l)
  (cond
    [(tree-label? l) (tree-label-selection l)]
    [(widening? l) (widening-selection l)]
    [(case? l) (case-selection l)]
    [(generalization? l) (generalization-selection l)]))
(provide
 (proc-doc/names
  label-selection
  (-> label-with-conjunction? (maybe exact-nonnegative-integer?))
  (label)
  @{Extracts the index of the selected conjunct from any tree label type which has it.}))

(define (label-with-conjunction? l)
  (or (tree-label? l) (widening? l) (case? l) (generalization? l)))
(provide
 (proc-doc/names
  label-with-conjunction?
  (-> any/c boolean?)
  (label)
  @{Checks whether the supplied value is a type of tree label which contains a conjunction.}))