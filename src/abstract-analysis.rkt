#lang at-exp racket
(require (only-in "abstract-multi-domain.rkt" abstract-atom?))
(require "abstract-knowledge.rkt")
(require "abstract-substitution.rkt")
(require scribble/srcdoc)
(require racket/serialize)
(require (for-doc scribble/manual))

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

(serializable-struct tree-label (conjunction selection substitution rule index)
                     #:methods
                     gen:equal+hash
                     [(define (equal-proc l1 l2 equal?-recur)
                        (and (equal?-recur (tree-label-conjunction l1) (tree-label-conjunction l2))
                             (equal?-recur (tree-label-selection l1) (tree-label-selection l2))
                             (equal?-recur (tree-label-substitution l1) (tree-label-substitution l2))
                             (equal?-recur (tree-label-rule l1) (tree-label-rule l2))
                             (equal?-recur (tree-label-index l1) (tree-label-index l2))))
                      ; same hash function as in Racket docs, not too concerned about optimum here
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
   [rule (or/c #f abstract-knowledge?)]
   [index (or/c #f exact-positive-integer?)])
  @{The contents of a node in the abstract analysis tree which has not yet been visited or which was successfully unfolded.
     The field @racket[selection] stands for the index (if any) of the atom selected for unfolding.
     The field @racket[substitution] is the substitution which was applied to the parent and a program clause to obtain @racket[conjunction].
     The field @racket[rule] is the abstract knowledge with which the parent was resolved to obtain @racket[conjunction].
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