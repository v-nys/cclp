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
(require (only-in "abstract-multi-domain.rkt" abstract-atom? abstract-function?))
(require racket-tree-utils/src/tree (only-in racket-tree-utils/src/printer tree-display))
(require (only-in racket-list-utils/utils findf-index))
(require "abstract-resolve.rkt")
(require "abstract-knowledge.rkt")
(require "abstract-substitution.rkt")
(require scribble/srcdoc)
(require terminal-color)
(require "abstract-domain-ordering.rkt")
(require racket/serialize)
(require "abstract-analysis.rkt")
(require "abstract-analysis-tree.rkt")
(require "generational-tree.rkt")
(require (only-in "concrete-domain.rkt" function?))
(require "cclp-interpreter.rkt")
(require "similarity.rkt")
(require "preprior-graph.rkt")

(require racket/logging)
(require (for-doc scribble/manual))
(define use-color #f)

; full-ai-rules is tricky
; these are converted to full-evaluations pretty fast...
(struct cclp (clauses full-ai-rules concrete-constants query))
(provide (struct-out cclp))

(define (interactive-analysis tree clauses full-evaluations next-index filename concrete-constants)
  (define-values (show-top proceed go-back save widen case-split genealogy end)
    (values "show top level" "proceed" "rewind last operation" "save analysis" "widen the current node" "apply a case split" "show genealogical analysis" "end analysis"))
  (define choice (prompt-for-answer "What do you want to do?" show-top proceed go-back save widen case-split genealogy end))
  (cond
    ;    [(equal? choice show-top)
    ;     (begin (newline)
    ;            (tree-display tree print-tree-label)
    ;            (newline)
    ;            (interactive-analysis tree clauses full-evaluations next-index filename concrete-constants))]
        
    [(equal? choice proceed)
     (match (candidate-and-predecessors tree '())
       [(cons (none) _)
        (begin (displayln "There are no nodes left to analyze.")
               (interactive-analysis tree clauses full-evaluations next-index filename concrete-constants))]
       [(cons (some candidate) preds)
        ; TODO we can select a candidate (conjunction) regardless of preprior
        ; but to advance the analysis, we may need to update the partial order
        ; if the candidate contains conjuncts which are not in its topmost partial order,
        ; or if it contains a new combination of conjuncts seen before but not ordered wrt to one another
        ; we need additional interaction
        ; also, what happens when a branch is completed?
        ; sibling branches are updated
        ; 
        (let-values ([(updated-candidate updated-top)
                      (advance-analysis tree candidate clauses full-evaluations concrete-constants next-index preds)])
          (begin
            (newline)
            (tree-display updated-candidate print-tree-label)
            (newline)
            (interactive-analysis
             updated-top clauses full-evaluations (+ next-index 1) filename concrete-constants)))])]
        
    ;    [(equal? choice go-back)
    ;     (let ([rewound (rewind tree)])
    ;       (if rewound
    ;           (begin
    ;             (tree-display (car rewound) print-tree-label)
    ;             (interactive-analysis (cdr rewound) clauses full-evaluations (- next-index 1) filename concrete-constants))
    ;           (displayln "Can't go back any further!")))]
    ;    [(equal? choice save)
    ;     (let* ([out (open-output-file filename #:exists 'truncate/replace)]
    ;            [serialized-tree (serialize tree)])
    ;       (begin
    ;         (write serialized-tree out)
    ;         (close-output-port out)
    ;         (interactive-analysis tree clauses full-evaluations next-index filename concrete-constants)))]
    ;    [(equal? choice widen)
    ;     (match (candidate-and-predecessors tree '())
    ;       [(cons (none) _)
    ;        (interactive-analysis tree clauses full-evaluations next-index filename concrete-constants)]
    ;       ; candidate can be either a tree-label or a widening
    ;       [(cons (some candidate) _)
    ;        (begin
    ;          (displayln "Please enter a conjunction with an equal or greater extension.")
    ;          (define read-conjunction (read))
    ;          (define widened-conjunction (interpret-abstract-conjunction read-conjunction))
    ;          (displayln "Please enter a reason why widening was applied.")
    ;          (define read-reason (read))
    ;          (define updated-label
    ;            (match (node-label candidate)
    ;              [(tree-label c se su r #f) (tree-label c se su r next-index)]
    ;              [(widening c se msg #f) (widening c se msg next-index)]
    ;              [_ (error "candidate type unaccounted for")]))
    ;          (define updated-top
    ;            (replace-first-subtree
    ;             tree
    ;             candidate
    ;             (node updated-label (list (node (widening widened-conjunction (none) read-reason #f) '())))))
    ;          (interactive-analysis updated-top clauses full-evaluations (+ next-index 1) filename concrete-constants))])]
    ;    [(equal? choice case-split)
    ;     ; similar to widen, but user should enter as many conjunctions as they like
    ;     ; the conjunctions do not need to (and in most cases will not) specify the candidate
    ;     (error "not implemented yet")]
    ;    [(equal? choice genealogy)
    ;     (let* ([active-branch (active-branch-info tree)]
    ;            [outputs (if active-branch (generational-trees active-branch) #f)])
    ;       (begin
    ;         (if active-branch
    ;             (if (empty? outputs)
    ;                 (displayln "There are no target atoms for recursion analysis.")
    ;                 (map (λ (t) (tree-display t print-atom-with-generation-node)) outputs))
    ;             (displayln "There is no active branch."))
    ;         (interactive-analysis tree clauses full-evaluations next-index filename concrete-constants)))]
    ;    [(equal? choice end) (void)]
    [else (error 'unsupported)]))



; TODO check if file exists
(define (load-analysis clauses full-evaluations filename concrete-constants)
  (let* ([loaded-tree (deserialize (read (open-input-file filename)))]
         [largest-index (largest-node-index loaded-tree)]
         [fresh-index (if largest-index (+ largest-index 1) 1)])
    (interactive-analysis loaded-tree clauses full-evaluations fresh-index filename concrete-constants)))

(define (begin-analysis program-data filename)
  (match program-data
    [(cclp clauses full-evaluations concrete-constants initial-query)
     ; using none for selection because no selection will occur more often
     ; using list for substitution because it is not wrong and is consistent
     ; using #f for rule because this is the only case where there is no associated clause
     (begin (define initial-tree-label
              (tree-label (list initial-query) (none) (list) #f #f (list (mk-preprior-graph))))
            (define initial-tree (node initial-tree-label (list)))
            (interactive-analysis initial-tree clauses full-evaluations 1 filename concrete-constants))]))

(define (cclp-top filename program-data)
  (define logger (make-logger 'cc #f))
  (current-logger logger)
  (with-logging-to-port (current-error-port)
    (λ () (cclp-run filename program-data))
    'info))
(provide
 (proc-doc/names
  cclp-top
  (-> path?
      cclp?
      void?)
  (filename program-data)
  @{Top-level function used to run a compiling control logic program.}))

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
  (define full-evaluations (map full-ai-rule->full-evaluation (cclp-full-ai-rules program-data)))
  (define program-data-aux (struct-copy cclp program-data [full-ai-rules full-evaluations]))
  (cond [(equal? choice analysis)
         (begin (begin-analysis program-data-aux serialized-filename)
                (cclp-run filename program-data))]
; TODO restore
;        [(equal? choice load)
;         (begin
;           (load-analysis
;            (cclp-clauses program-data)
;            full-evaluations
;            serialized-filename
;            (cclp-concrete-constants program-data))
;           (cclp-run filename program-data))]
        [(equal? choice quit) (void)]
        [else (error 'unsupported)]))

;                                                                          
;                                                                          
;                                                                          
;                        ;                          ;                      
;                        ;                ;         ;                      
;                                         ;                                
;   ; ;;;      ; ;;;   ;;;     ; ;;;;   ;;;;;;    ;;;     ; ;;;;     ;;; ; 
;   ;;   ;     ;;   ;    ;     ;;   ;;    ;         ;     ;;   ;;   ;   ;; 
;   ;     ;    ;         ;     ;     ;    ;         ;     ;     ;  ;     ; 
;   ;     ;    ;         ;     ;     ;    ;         ;     ;     ;  ;     ; 
;   ;     ;    ;         ;     ;     ;    ;         ;     ;     ;  ;     ; 
;   ;     ;    ;         ;     ;     ;    ;         ;     ;     ;  ;     ; 
;   ;;   ;     ;         ;     ;     ;    ;         ;     ;     ;   ;   ;; 
;   ; ;;;      ;      ;;;;;;;  ;     ;     ;;;   ;;;;;;;  ;     ;    ;;; ; 
;   ;                                                                    ; 
;   ;                                                               ;   ;; 
;   ;                                                                ;;;;  
;                                                                          


(define (print-substitution s [out (current-output-port)])
  (display "{" out)
  (map (λ (x) (if (string? x) (display x out) (print x out))) (add-between s ";"))
  (display "}" out))

(define (print-atom-with-generation-node n [out (current-output-port)])
  (match n
    [(node ag _)
     (fprintf out "~v {~v}" (identified-atom-atom (identified-atom-with-generation-id-atom ag)) (identified-atom-with-generation-generation ag))]))
(provide print-atom-with-generation-node)

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
       (print-conjunction con sel out))]
    [(cycle i)
     (if use-color
         (display-color (format "cycle back to node ~a" i) out #:fg 'green)
         (display (format "cycle back to node ~a" i) out))]
    [(widening con sel msg idx)
     (begin
       (display "[widening]")
       (when idx (display (format "~v:" idx)))
       (print-conjunction con sel out))]))

;                                               
;                                               
;                                               
;                                               
;     ;                          ;              
;     ;                          ;              
;   ;;;;;;     ;;;     ;;;;;   ;;;;;;    ;;;;;  
;     ;       ;   ;   ;     ;    ;      ;     ; 
;     ;      ;     ;  ;          ;      ;       
;     ;      ;     ;  ;;;;       ;      ;;;;    
;     ;      ;;;;;;;      ;;;    ;          ;;; 
;     ;      ;              ;    ;            ; 
;     ;       ;    ;  ;     ;    ;      ;     ; 
;      ;;;     ;;;;    ;;;;;      ;;;    ;;;;;  
;                                               
;                                               
;                                               
;                                               


(module+ test
  (require rackunit)
  (test-case
   "finding the most recently applied operation"
   (let ([root-only-tree
          (node
           (tree-label
            (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
            (none)
            (list)
            #f
            #f) '())])
     (check-equal? (candidate-for-undo root-only-tree) #f))
   (let* ([unwound-leaf
           (node
            (tree-label
             (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),eq(α1,α2)")
             (none)
             (list)
             #f ; doesn't matter for purpose of this test
             #f) '())]
          [unwound-tree
           (node
            (tree-label
             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
             (some 0)
             (list)
             #f
             1) (list unwound-leaf))]
          [rewound-tree
           (node
            (tree-label
             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
             (none)
             (list)
             #f
             #f) '())])
     (check-equal? (candidate-for-undo unwound-tree) unwound-tree)))

  (test-case
   "rewinding the most recently applied operation"
   (let ([root-only-tree
          (node
           (tree-label
            (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
            (none)
            (list)
            #f
            #f) '())])
     (check-equal? (rewind root-only-tree) #f))
   (let* ([unwound-leaf
           (node
            (tree-label
             (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),eq(α1,α2)")
             (none)
             (list)
             #f ; doesn't matter for purpose of this test
             #f) '())]
          [unwound-tree
           (node
            (tree-label
             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
             (some 0)
             (list)
             #f
             1) (list unwound-leaf))]
          [rewound-tree
           (node
            (tree-label
             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
             (none)
             (list)
             #f
             #f) '())])
     (check-equal? (rewind unwound-tree) (cons rewound-tree rewound-tree)))
   (let* ([leaf1 (node (tree-label (list) (none) (list) #f #f) (list))]
          [leaf2 leaf1]
          [grandchild1 (node (tree-label (interpret-abstract-conjunction "quux") 0 (list) #f 3) (list leaf1))]
          [grandchild2 (node (tree-label (interpret-abstract-conjunction "zoom") 0 (list) #f 4) (list leaf2))]
          [child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 grandchild2))]
          [unwound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list child))]
          [rewound-node (node (tree-label (interpret-abstract-conjunction "zoom") (none) (list) #f #f) (list))]
          [rewound-child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 rewound-node))]
          [rewound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list rewound-child))])
     (check-equal? (rewind unwound-tree) (cons rewound-node rewound-tree)))
   (let* ([leaf1 (node (tree-label (list) (none) (list) #f #f) (list))]
          [leaf2 (node (cycle 3) (list))]
          [grandchild1 (node (tree-label (interpret-abstract-conjunction "quux") 0 (list) #f 3) (list leaf1))]
          [grandchild2 (node (tree-label (interpret-abstract-conjunction "zoom") 0 (list) #f 4) (list leaf2))]
          [grandchild3 (node (tree-label (interpret-abstract-conjunction "baz") (none) (list) #f 4) (list))]
          [child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 grandchild2 grandchild3))]
          [unwound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list child))]
          [rewound-node (node (tree-label (interpret-abstract-conjunction "zoom") (none) (list) #f #f) (list))]
          [rewound-child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 rewound-node grandchild3))]
          [rewound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list rewound-child))])
     (check-equal? (rewind unwound-tree) (cons rewound-node rewound-tree)))
   (let* ([leaf (node (widening (interpret-abstract-conjunction "foo(γ1)") #f "some message" #f) '())]
          [root-before (node (tree-label (interpret-abstract-conjunction "foo(nil)") 0 (list) #f 1) (list leaf))]
          [root-after (node (tree-label (interpret-abstract-conjunction "foo(nil)") (none) (list) #f #f) (list))])
     (check-equal? (car (rewind root-before)) root-after)
     (check-equal? (cdr (rewind root-before)) root-after))))