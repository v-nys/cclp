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

(require "io-utils.rkt")
(require "data-utils.rkt")
(require (only-in "concrete-knowledge.rkt" rule?))
(require "fullai-domain.rkt")
(require (only-in "abstract-multi-domain.rkt" abstract-atom?))
(require racket-tree-utils/src/tree (only-in racket-tree-utils/src/printer tree-display))
(require (only-in parenlog model?))
(require (only-in "execution.rkt" selected-index))
(require "abstract-resolve.rkt")
(require "abstract-knowledge.rkt")
(require "abstract-substitution.rkt")
(require scribble/srcdoc)
(require terminal-color)
(require "abstract-domain-ordering.rkt")

(require (for-doc scribble/manual))

(define use-color #f)

(define (print-substitution s [out (current-output-port)])
  (display "{" out)
  (map (λ (x) (if (string? x) (display x out) (print x out))) (add-between s ";"))
  (display "}" out))

(define (print-conjunction c ms [out (current-output-port)])
  (define last-i (- (length c) 1))
  (for ([i (range 0 (length c))]
        [atom c])
    (begin
      (if (and (some? ms) (eq? i (some-v ms)))
          (if use-color (print-color atom out #:fg 'red) (print atom out))
          (print atom out))
      (when (< i last-i) (display "," out)))))

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

(struct tree-label (conjunction selection substitution rule index)
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
  ("A representation of the contents of a node in the abstract analysis tree which has not yet been visited or which was successfully unfolded."
   "selection stands for the index (if any) of the atom selected for unfolding"
   "substitution is the substitution which was applied to the parent and a program clause to obtain conjunction"
   "rule is the rule with which the parent was resolved to obtain conjunction"
   "index is a unique label, assigned so that cycles can be clearly marked. It is an integer if the node has been visited and #f if the node has not yet been visited.")))

(struct cycle (index)
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
  @{A representation of a cycle detected during abstract analysis.
     @racket[index] stands for the index of a previously handled conjunction which generalizes over the conjunction which introduces the @racket[cycle].
     The latter is normally represented as the parent of the @racket[cycle] in the abstract analysis tree.}))

(define (print-tree-label t [out (current-output-port)])
  (match (node-label t)
    [(tree-label con sel sub r i)
     (begin
       (when i (display (format "~v:" i)))
       (print-conjunction con sel out)
       (when ((compose not null?) sub) (begin (display " " out) (print-substitution sub out))))]))

(define (candidate-and-predecessors t acc)
  (match t
    [(node (tree-label '() _ _ _ _) '()) (cons (none) acc)]
    [(node 'fail '()) (cons (none) acc)]
    [(node (cycle _) '()) (cons (none) acc)]
    [(node (tree-label c (none) s r #f) '())
     (cons (some (node (tree-label c (none) s r #f) '())) acc)]
    [(node (tree-label c (none) s r i) (list (cycle ci)))
     (candidate-and-predecessors ci (cons (c i) acc))]
    [(node (tree-label c (some v) _ _ i) children)
     (foldl
      (λ (child acc2)
        (if (some? (car acc2))
            acc2
            (candidate-and-predecessors
             child
             (cdr acc2))))
      (cons (none) (cons (cons c i) acc))
      (node-children t))]))
; contract could be a bit more specific...
(provide
 (proc-doc/names
  candidate-and-predecessors
  (-> node? list? (cons/c any/c list?))
  (tree accumulator)
  ("Find the next candidate for unfolding and conjunctions which have already been dealt with.")))

(define (resolvent->node res)
  (node
   (tree-label
    (resolvent-conjunction res)
    (none)
    (resolvent-substitution res)
    (resolvent-knowledge res)
    #f) ; resolvents have not yet been visited
   (list)))

(define (interactive-analysis tree clauses full-evaluations preprior next-index)
  (define-values (show-top proceed go-back save end)
    (values "show top level" "proceed" "go back" "save analysis" "end analysis"))
  (define choice (prompt-for-answer "What do you want to do?" show-top proceed go-back save end))
  (cond [(equal? choice show-top)
         (begin (newline)
                (tree-display tree print-tree-label)
                (newline)
                (interactive-analysis tree clauses full-evaluations preprior next-index))]
        [(equal? choice proceed)
         (match (candidate-and-predecessors tree '())
           [(cons (none) _)
            (begin (display "There are no nodes left to analyze.")
                   (interactive-analysis tree clauses full-evaluations preprior next-index))]
           [(cons (some candidate) preds)
            (let* ([candidate-label (node-label candidate)]
                   [conjunction (tree-label-conjunction candidate-label)]
                   [more-general-predecessor
                    (findf (λ (p-and-i) (>=-extension (car p-and-i) conjunction)) preds)])
              ; lots of duplicated code here, can this be improved?
              (if more-general-predecessor
                  (let* ([cycle-node (cycle (cdr more-general-predecessor))]
                         [updated-candidate
                          (node
                           (tree-label
                            (tree-label-conjunction candidate-label)
                            (none)
                            (tree-label-substitution candidate-label)
                            (tree-label-rule candidate-label)
                            next-index)
                           (list cycle-node))]
                         [updated-top (replace-first-subtree tree candidate updated-candidate)])
                    (begin
                      (newline)
                      (tree-display updated-candidate print-tree-label)
                      (newline)
                      (interactive-analysis
                       updated-top clauses full-evaluations preprior (+ next-index 1))))
                  (let* ([resolution-result
                          (abstract-resolve conjunction preprior clauses full-evaluations)]
                         [index-selection (car resolution-result)]
                         [resolvents (cdr resolution-result)]
                         [child-trees (map resolvent->node resolvents)]
                         [updated-candidate
                          (node
                           (tree-label
                            (tree-label-conjunction candidate-label)
                            (some index-selection)
                            (tree-label-substitution candidate-label)
                            (tree-label-rule candidate-label)
                            next-index)
                           child-trees)]
                         [updated-top (replace-first-subtree tree candidate updated-candidate)])
                    (begin
                      (newline)
                      (tree-display updated-candidate print-tree-label)
                      (newline)
                      (interactive-analysis
                       updated-top clauses full-evaluations preprior (+ next-index 1))))))])]
        [(equal? choice end) (void)]
        [else (error 'unsupported)]))

; interactie tijdens analyse vereist: huidige boom, KB, full evals, selectiemechanisme
(define (load-analysis filename) (void))

(define (begin-analysis program-data)
  (match program-data
    [(4-tuple clauses full-evaluations preprior initial-query)
     ; using none for selection because no selection will occur more often
     ; using list for substitution because it is not wrong and is consistent
     ; using #f for rule because this is the only case where there is no associated clause
     (begin (define initial-tree-label (tree-label (list (4-tuple-fourth program-data)) (none) (list) #f #f))
            (define initial-tree (node initial-tree-label (list)))
            (interactive-analysis initial-tree clauses full-evaluations preprior 1))]))

(define (cclp-run filename program-data)
  (log-info "Entered top-level menu for program ~a with data ~s" filename program-data)
  
  (define serialized-filename
    (path-replace-extension (last (explode-path filename)) ".serializedcclp"))
  (define-values (analysis load quit)
    (values "analyze this program" "load existing analysis" "quit"))
  (define choice (prompt-for-answer "What do you want to do?" analysis load quit))
  (define (full-ai-rule->full-evaluation r)
    (full-evaluation
     (full-ai-rule-input-pattern r)
     (apply-substitution (full-ai-rule-output-substitution r) (full-ai-rule-input-pattern r))))
  (define full-evaluations (map full-ai-rule->full-evaluation (4-tuple-second program-data)))
  (define program-data-aux
    (4-tuple
     (4-tuple-first program-data) full-evaluations
     (4-tuple-third program-data) (4-tuple-fourth program-data)))
  (cond [(equal? choice analysis)
         (begin (begin-analysis program-data-aux)
                (cclp-run filename program-data))]
        [(equal? choice load)
         (begin (load-analysis serialized-filename)
                (cclp-run filename program-data))]
        [(equal? choice quit) (void)]
        [else (error 'unsupported)]))
(provide (contract-out
          [cclp-run
           (-> path?
               (4-tupleof (listof rule?) (listof full-ai-rule?) model? abstract-atom?)
               void?)]))