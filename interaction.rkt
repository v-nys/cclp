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
(require (only-in "abstract-multi-domain.rkt" abstract-atom? abstract-function? multi?))
(require racket-tree-utils/src/tree (only-in racket-tree-utils/src/printer tree-display))
(require (only-in racket-list-utils/utils findf-index))
(require "abstract-resolve.rkt")
(require "abstract-knowledge.rkt")
(require "abstract-substitution.rkt")
(require scribble/srcdoc)
(require "abstract-domain-ordering.rkt")
(require racket/serialize)
(require "abstract-analysis.rkt")
(require "abstract-analysis-tree.rkt")
(require (only-in "concrete-domain.rkt" function?))
(require "cclp-interpreter.rkt")
(require "preprior-graph.rkt")
(require "abstract-renaming.rkt")
(require (only-in "abstraction-inspection-utils.rkt" assemble-var-indices))
(require "gen-graph-structs.rkt")
(require "genealogical-graph.rkt"
         "mi-map.rkt")
(require
  file/convertible
  graph
  pict
  pretty-graphs
  cclp/genealogical-graph-visualization)
(require (only-in "multi-unfolding.rkt" unfold-multi-bounded))

(require (only-in br/cond while))

(require racket/logging)
(require (for-doc scribble/manual))

; full-ai-rules is tricky
; these are converted to full-evaluations pretty fast...
(struct cclp (clauses full-ai-rules concrete-constants partial-order query filename))
(provide (struct-out cclp))

;; roughly a combination of the constant and computed parts of an analysis, really
;; note that cclp isn't entirely constant, as partial-order is actually mutated along the way
;; worth considering to keep the partial order immutable and extend the graph, but doing so naively could be expensive
(struct analysis (cclp tree step edge-history) #:transparent)
(provide (struct-out analysis))

(define (save-analysis filename tree edge-history step-acc prior)
  (let* ([out (open-output-file filename #:exists 'truncate/replace)]
         [serialized-tree (serialize tree)])
    (begin
      (write serialized-tree out)
      (write (serialize edge-history) out)
      (write (serialize step-acc) out)
      (write (serialize prior) out)
      (close-output-port out))))

(define (proceed/batch prog-analysis)
  (match prog-analysis
    [(analysis (and source-prog (cclp clauses full-evaluations concrete-constants prior _query _filename)) tree step-acc edge-history)
     (let ([outcome (advance-analysis tree clauses full-evaluations concrete-constants prior)])
       (match outcome
         [(cons 'underspecified-order candidate) ; for proceed/batch, an underspecified order should trigger an exception
          (raise-user-error 'proceed/batch "partial order is underspecified")]
         [(cons cand top) ; simple update to the analysis
          (analysis source-prog top (add1 step-acc) edge-history)]
         ['no-candidate prog-analysis]))]))
(provide
 (proc-doc/names
  proceed/batch
  (-> analysis? analysis?)
  (prog-analysis)
  @{Advances the analysis of @racket[prog-analysis] without side-effects.
 The only exception is a user error which may be raised if the partial order in the analysis does not dictate which atom should be selected.}))

(define (analysis->current-genealogical-graph prog-analysis)
  (match prog-analysis
    [(analysis _ tree _ _)
     (let* ([active-branch (active-branch tree)]
            [gr (if active-branch (genealogical-graph-skeleton active-branch) #f)]
            [root (if active-branch (gen-node (car (tree-label-conjunction (car active-branch))) 1 #f #t #t) #f)]
            [depth (if active-branch (length active-branch) #f)]
            [targets (if active-branch (map (λ (e) (struct-copy gen-node e [range (gen 0 #f)])) (candidate-targets gr)) #f)])
       (when active-branch
         (annotate-general! gr root targets depth))
       gr)]))
(provide analysis->current-genealogical-graph)

(define (interactive-analysis prog-analysis)
  (match prog-analysis
    [(analysis (and source-prog (cclp clauses full-evaluations concrete-constants prior _query _filename)) tree step-acc edge-history)
     (log-debug "performing interactive analysis")
     (define analyzing? #t)
     (define (proceed/interactive)
       (let ([outcome (advance-analysis tree clauses full-evaluations concrete-constants prior)])
         (match outcome
           [(cons 'underspecified-order candidate)
            (displayln "Partial order is underspecified.")
            (displayln "Please select the atom which takes precedence from the following list.")
            (let* ([multis (filter multi? (label-conjunction (node-label candidate)))]
                   [multi-conjuncts (apply append (map (λ (m) (let ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) m)))]) (car (unfold-multi-bounded 1 m offset offset)))) multis))]
                   [options (remove-duplicates (map normalize-abstract-atom (append multi-conjuncts (filter abstract-atom? (label-conjunction (node-label candidate))))))]
                   [user-selection (prompt-for-selection options)]
                   [new-precedences (filter (λ (p) (not (has-edge? prior (car p) (cdr p)))) (map (λ (c) (cons user-selection c)) (remove user-selection options)))])
              (begin
                (for ([precedence new-precedences])
                  (add-directed-edge! prior (car precedence) (cdr precedence)))
                (hash-set! edge-history step-acc new-precedences)
                (set! step-acc (add1 step-acc)) ; assuming step will be successful
                (with-handlers
                    ([exn:fail?
                      (λ (e)
                        (begin (display (format "Error: ~a" e))
                               (set! step-acc (sub1 step-acc))
                               (cons #f tree)))])
                  (match-let ([(cons cand top) (advance-analysis tree clauses full-evaluations concrete-constants prior #:new-edges new-precedences)])
                    (begin (newline) (tree-display cand print-tree-label) (newline) (cons #f top))))))]
           [(cons cand top) (begin (set! step-acc (add1 step-acc)) (newline) (tree-display cand print-tree-label) (newline) (cons #t top))]
           ['no-candidate (cons #f tree)])))
     (while
      analyzing?
      (set!
       tree
       (interactive-dispatch
        "What do you want to do?"
        ["proceed"
         (cdr (proceed/interactive))]
        ["fast-forward"
         (begin
           (displayln "Maximum number of steps? (0 to keep going indefinitely)")
           (match-let* ([steps (prompt-for-integer)]
                        [indefinitely? (equal? steps 0)]
                        [(cons continue? new-tree) (proceed/interactive)])
             (while (and continue? (or indefinitely? (> steps 0)))
                    (begin (set! tree new-tree)
                           (set! steps (sub1 steps))
                           (match (proceed/interactive)
                             [(cons c? nt?)
                              (begin (set! continue? c?)
                                     (set! new-tree nt?))])
                           (save-analysis
                            (format "~a.~a" (serialized-filename source-prog) (size new-tree))
                            new-tree edge-history step-acc prior)))
             new-tree))]
        ["save analysis"
         (begin
           (save-analysis (serialized-filename source-prog) tree edge-history step-acc prior)
           tree)]
        ["show top level tree"
         (begin
           (newline)
           (tree-display tree print-tree-label)
           (newline)
           tree)]
        ["show debugging variables"
         (begin
           (displayln "precedence pairs:")
           (for ([precedence (in-edges prior)])
             (displayln (format "~v > ~v" (first precedence) (second precedence))))
           (displayln "step:")
           (displayln step-acc)
           tree)]
        ["show genealogical graph"
         (let ([gen-graph (analysis->current-genealogical-graph prog-analysis)])
           (if gen-graph
               (begin
                 (with-output-to-file
                     "genealogical-graph.svg"
                   (λ ()
                     (display
                      (convert
                       (dag->pict gen-graph gen-node->pict)
                       'svg-bytes)))
                   #:mode 'binary
                   #:exists 'replace)
                 (displayln "Genealogical graph visualization was written to file."))
               (displayln "There is no active branch."))
           tree)]
        ["generate meta-interpreter map"
         (display-mi-map tree)]
        ["generate generalization/2 clauses"
         (display-generalization-clauses tree)]
        ["end analysis"
         (set! analyzing? #f)])))]))
(provide interactive-analysis)

(define (cclp->initial-analysis program-data)
  (define initial-tree-label
    (tree-label (list (cclp-query program-data)) (none) (list) #f #f (list)))
  (define initial-tree (node initial-tree-label (list)))
  (analysis program-data initial-tree 1 (make-hash)))
(provide cclp->initial-analysis)

(define (begin-analysis program-data)
  (interactive-analysis
   (cclp->initial-analysis program-data)))

(define (cclp-top program-data)
  (define logger (make-logger 'cc #f))
  (current-logger logger)
  (with-logging-to-port (current-error-port)
    (λ ()
      (begin
        (log-debug "logger is active")
        (cclp-run program-data)))
    'warning))
(provide
 (proc-doc/names
  cclp-top
  (-> cclp?
      void?)
  (program-data)
  @{Top-level function used to run a compiling control logic program.}))

(define (load-analysis program-data)
  (let* ([in (open-input-file (serialized-filename program-data))]
         [loaded-tree (deserialize (read in))]
         [edge-history (deserialize (read in))]
         [step-acc (deserialize (read in))]
         [prior (deserialize (read in))])
    (close-input-port in)
    (interactive-analysis
     (analysis
      (struct-copy cclp program-data [partial-order prior])
      loaded-tree
      step-acc
      edge-history))))

(define (serialized-filename program-data)
  (path-replace-extension
   (last
    (explode-path
     (cclp-filename program-data)))
   ".serializedcclp"))

(define (cclp-run program-data)
  (interactive-dispatch
   "What do you want to do?"
   ("analyze this program"
    (let ([preprior-copy (graph-copy (cclp-partial-order program-data))]) ; due to mutability of graphs
      (begin-analysis program-data)
      (cclp-run (struct-copy cclp program-data [partial-order preprior-copy]))))
   ("load existing analysis"
    (begin (load-analysis program-data)
           (cclp-run program-data)))
   ("quit" (void))))

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

;(define (print-atom-with-generation-node n [out (current-output-port)])
;  (match n
;    [(node ag _)
;     (fprintf out "~v {~v}" (identified-atom-atom (identified-atom-with-generation-id-atom ag)) (identified-atom-with-generation-generation ag))]))
;(provide print-atom-with-generation-node)

(define (print-conjunction c ms [out (current-output-port)])
  (define last-i (- (length c) 1))
  (if (> (length c) 0)
      (for ([i (range 0 (length c))]
            [atom c])
        (begin
          (if (and (some? ms) (eq? i (some-v ms)))
              (display (format "*~v*" atom) out)
              (print atom out))
          (when (< i last-i) (display "," out))))
      (display "□")))

(define (print-tree-label t [out (current-output-port)])
  (match (node-label t)
    [(or (tree-label con sel _ _ i _)
         (generalization con sel i _ _ _))
     (begin
       (when i (display (format "~v:" i)))
       (print-conjunction con sel out))]
    [(cycle i)
     (display (format "cycle back to node ~a" i) out)]
    [(widening con sel msg idx edges)
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


;(module+ test
;  (require rackunit)
;  (test-case
;   "finding the most recently applied operation"
;   (let ([root-only-tree
;          (node
;           (tree-label
;            (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
;            (none)
;            (list)
;            #f
;            #f) '())])
;     (check-equal? (candidate-for-undo root-only-tree) #f))
;   (let* ([unwound-leaf
;           (node
;            (tree-label
;             (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),eq(α1,α2)")
;             (none)
;             (list)
;             #f ; doesn't matter for purpose of this test
;             #f) '())]
;          [unwound-tree
;           (node
;            (tree-label
;             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
;             (some 0)
;             (list)
;             #f
;             1) (list unwound-leaf))]
;          [rewound-tree
;           (node
;            (tree-label
;             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
;             (none)
;             (list)
;             #f
;             #f) '())])
;     (check-equal? (candidate-for-undo unwound-tree) unwound-tree)))
;
;  (test-case
;   "rewinding the most recently applied operation"
;   (let ([root-only-tree
;          (node
;           (tree-label
;            (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
;            (none)
;            (list)
;            #f
;            #f) '())])
;     (check-equal? (rewind root-only-tree) #f))
;   (let* ([unwound-leaf
;           (node
;            (tree-label
;             (interpret-abstract-conjunction "collect(γ1,α1),collect(γ2,α2),eq(α1,α2)")
;             (none)
;             (list)
;             #f ; doesn't matter for purpose of this test
;             #f) '())]
;          [unwound-tree
;           (node
;            (tree-label
;             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
;             (some 0)
;             (list)
;             #f
;             1) (list unwound-leaf))]
;          [rewound-tree
;           (node
;            (tree-label
;             (interpret-abstract-conjunction "sameleaves(γ1,γ2)")
;             (none)
;             (list)
;             #f
;             #f) '())])
;     (check-equal? (rewind unwound-tree) (cons rewound-tree rewound-tree)))
;   (let* ([leaf1 (node (tree-label (list) (none) (list) #f #f) (list))]
;          [leaf2 leaf1]
;          [grandchild1 (node (tree-label (interpret-abstract-conjunction "quux") 0 (list) #f 3) (list leaf1))]
;          [grandchild2 (node (tree-label (interpret-abstract-conjunction "zoom") 0 (list) #f 4) (list leaf2))]
;          [child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 grandchild2))]
;          [unwound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list child))]
;          [rewound-node (node (tree-label (interpret-abstract-conjunction "zoom") (none) (list) #f #f) (list))]
;          [rewound-child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 rewound-node))]
;          [rewound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list rewound-child))])
;     (check-equal? (rewind unwound-tree) (cons rewound-node rewound-tree)))
;   (let* ([leaf1 (node (tree-label (list) (none) (list) #f #f) (list))]
;          [leaf2 (node (cycle 3) (list))]
;          [grandchild1 (node (tree-label (interpret-abstract-conjunction "quux") 0 (list) #f 3) (list leaf1))]
;          [grandchild2 (node (tree-label (interpret-abstract-conjunction "zoom") 0 (list) #f 4) (list leaf2))]
;          [grandchild3 (node (tree-label (interpret-abstract-conjunction "baz") (none) (list) #f 4) (list))]
;          [child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 grandchild2 grandchild3))]
;          [unwound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list child))]
;          [rewound-node (node (tree-label (interpret-abstract-conjunction "zoom") (none) (list) #f #f) (list))]
;          [rewound-child (node (tree-label (interpret-abstract-conjunction "bar") 0 (list) #f 2) (list grandchild1 rewound-node grandchild3))]
;          [rewound-tree (node (tree-label (interpret-abstract-conjunction "foo") 0 (list) #f 1) (list rewound-child))])
;     (check-equal? (rewind unwound-tree) (cons rewound-node rewound-tree)))
;   (let* ([leaf (node (widening (interpret-abstract-conjunction "foo(γ1)") #f "some message" #f) '())]
;          [root-before (node (tree-label (interpret-abstract-conjunction "foo(nil)") 0 (list) #f 1) (list leaf))]
;          [root-after (node (tree-label (interpret-abstract-conjunction "foo(nil)") (none) (list) #f #f) (list))])
;     (check-equal? (car (rewind root-before)) root-after)
;     (check-equal? (cdr (rewind root-before)) root-after))))
