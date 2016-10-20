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
(require racket/serialize)
(require "abstract-analysis.rkt")

(require racket/logging)
(require (for-doc scribble/manual))

(define use-color #f)

(define (print-substitution s [out (current-output-port)])
  (display "{" out)
  (map (λ (x) (if (string? x) (display x out) (print x out))) (add-between s ";"))
  (display "}" out))

(define (print-conjunction c ms [out (current-output-port)])
  (define last-i (- (length c) 1))
  (if (> (length c) 0)
      (for ([i (range 0 (length c))]
            [atom c])
        (begin
          (if (and (some? ms) (eq? i (some-v ms)))
              (if use-color
                  (print-color atom out #:fg 'red)
                  (display (format "*~v*" atom) out))
              (print atom out))
          (when (< i last-i) (display "," out))))
      (if use-color
          (display-color "□" #:fg 'green)
          (display "□"))))

(define (print-tree-label t [out (current-output-port)])
  (match (node-label t)
    [(tree-label con sel sub r i)
     (begin
       (when i (display (format "~v:" i)))
       (print-conjunction con sel out)
       (when ((compose not null?) sub) (begin (display " " out) (print-substitution sub out))))]
    [(cycle i)
     (if use-color
         (display-color (format "cycle back to node ~a" i) out #:fg 'green)
         (display (format "cycle back to node ~a" i) out))]))

(define (candidate-and-predecessors t acc)
  (match t
    [(node (tree-label '() _ _ _ _) '()) (cons (none) acc)]
    [(node 'fail '()) (cons (none) acc)]
    [(node (cycle _) '()) (cons (none) acc)]
    [(node (tree-label c (none) s r #f) '())
     (cons (some (node (tree-label c (none) s r #f) '())) acc)]
    [(node (tree-label c (none) s r i) (list (node (cycle ci) '())))
     (candidate-and-predecessors (node (cycle ci) '()) (cons (cons c i) acc))]
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

(define (candidate-for-undo t)
  (match t
    [(node _ (list)) #f]
    [(node l ch)
     (if (andmap (compose empty? node-children) ch)
         t
         (candidate-for-undo (last (filter (compose not empty? node-children) ch))))]))
(provide candidate-for-undo)

(define (undo t)
  (match t
    [(node (tree-label con sel sub r i) ch)
     (node (tree-label con (none) sub r #f) (list))]))

(define (rewind t)
  (let* ([candidate (candidate-for-undo t)]
         [locally-rewound (if candidate (undo candidate) #f)])
    (if candidate
        (cons locally-rewound (replace-last-subtree t candidate locally-rewound))
        #f)))
(provide
 (proc-doc/names
  rewind
  (-> node? (or/c #f (cons/c node? node?)))
  (t)
  @{Undo the latest unfolding or generalization that occurred in @racket[t]}))

(define (interactive-analysis tree clauses full-evaluations preprior next-index filename)
  (define-values (show-top proceed go-back save genealogy end)
    (values "show top level" "proceed" "rewind last operation" "save analysis" "show genealogical analysis" "end analysis"))
  (define choice (prompt-for-answer "What do you want to do?" show-top proceed go-back save genealogy end))
  (cond [(equal? choice show-top)
         (begin (newline)
                (tree-display tree print-tree-label)
                (newline)
                (interactive-analysis tree clauses full-evaluations preprior next-index filename))]
        [(equal? choice proceed)
         (match (candidate-and-predecessors tree '())
           [(cons (none) _)
            (begin (display "There are no nodes left to analyze.")
                   (interactive-analysis tree clauses full-evaluations preprior next-index filename))]
           [(cons (some candidate) preds)
            (let* ([candidate-label (node-label candidate)]
                   [conjunction (tree-label-conjunction candidate-label)]
                   [more-general-predecessor
                    (findf (λ (p-and-i) (>=-extension (car p-and-i) conjunction)) preds)])
              ; lots of duplicated code here, can this be improved?
              (if more-general-predecessor
                  (let* ([cycle-node (node (cycle (cdr more-general-predecessor)) '())]
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
                       updated-top clauses full-evaluations preprior (+ next-index 1) filename)))
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
                       updated-top clauses full-evaluations preprior (+ next-index 1) filename)))))])]
        [(equal? choice go-back)
         (let ([rewound (rewind tree)])
           (if rewound
               (begin
                 (tree-display (car rewound) print-tree-label)
                 (interactive-analysis (cdr rewound) clauses full-evaluations preprior (- next-index 1) filename))
               (displayln "Can't go back any further!")))]
        [(equal? choice save)
         (let* ([out (open-output-file filename #:exists 'truncate/replace)]
                [serialized-tree (serialize tree)])
           (begin
             (write serialized-tree out)
             (close-output-port out)
             (interactive-analysis tree clauses full-evaluations preprior next-index filename)))]
        [(equal? choice genealogy) (void)]
        [(equal? choice end) (void)]
        [else (error 'unsupported)]))

(define (largest-node-index t)
  (match t
    [(node label (list)) #f]
    [(node label children)
     (or
      (foldr
       (λ (c acc)
         (if (and (not acc) (tree-label? (node-label c)))
             (largest-node-index c)
             acc))
       #f
       children)
      (tree-label-index label))]))

; TODO check if file exists
(define (load-analysis clauses full-evaluations preprior filename)
  (let* ([loaded-tree (deserialize (read (open-input-file filename)))]
         [largest-index (largest-node-index loaded-tree)]
         [fresh-index (if largest-index (+ largest-index 1) 1)])
    (interactive-analysis loaded-tree clauses full-evaluations preprior fresh-index filename)))

(define (begin-analysis program-data filename)
  (match program-data
    [(4-tuple clauses full-evaluations preprior initial-query)
     ; using none for selection because no selection will occur more often
     ; using list for substitution because it is not wrong and is consistent
     ; using #f for rule because this is the only case where there is no associated clause
     (begin (define initial-tree-label (tree-label (list (4-tuple-fourth program-data)) (none) (list) #f #f))
            (define initial-tree (node initial-tree-label (list)))
            (interactive-analysis initial-tree clauses full-evaluations preprior 1 filename))]))

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
         (begin (begin-analysis program-data-aux serialized-filename)
                (cclp-run filename program-data))]
        [(equal? choice load)
         (begin (load-analysis (4-tuple-first program-data) full-evaluations (4-tuple-third program-data) serialized-filename)
                (cclp-run filename program-data))]
        [(equal? choice quit) (void)]
        [else (error 'unsupported)]))
(provide (contract-out
          [cclp-run
           (-> path?
               (4-tupleof (listof rule?) (listof full-ai-rule?) model? abstract-atom?)
               void?)]))