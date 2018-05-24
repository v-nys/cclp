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
(require cclp-common/data-utils)
(require (only-in cclp-common-data/concrete-knowledge rule?))
(require (only-in cclp-common-data/abstract-multi-domain abstract-atom? abstract-function? multi?))
(require positional-tree-utils (only-in positional-tree-utils/printer tree-display))
(require (only-in list-utils findf-index))
(require "abstract-resolve.rkt")
(require cclp-common-data/abstract-knowledge)
(require cclp-common-data/abstract-substitution)
(require scribble/srcdoc)
(require cclp-common/abstract-domain-ordering)
(require racket/serialize)
(require cclp-common/abstract-analysis)
(require "abstract-analysis-tree.rkt")
(require (only-in cclp-common-data/concrete-domain function?))
(require cclp-common/preprior-graph)
(require cclp-common/abstract-renaming)
(require (only-in cclp-common/abstraction-inspection-utils assemble-var-indices))
(require cclp-common/gen-graph-structs)
(require "genealogical-graph.rkt"
         "mi-map.rkt"
         (only-in "generalize.rkt" generalize/bu)
         (only-in cclp-analysis/clustering assign-prime-factor-ids cluster))
(require
  file/convertible
  graph
  pict
  pretty-graphs
  cclp-analysis/genealogical-graph-visualization)
(require (only-in cclp-common/multi-unfolding unfold-multi-bounded))

(require (only-in br/cond while))

(require racket/logging)
(require (for-doc scribble/manual))

(serializable-struct cclp (clauses full-ai-rules concrete-constants initial-partial-order query filename k))
(provide (struct-out cclp))

(serializable-struct analysis (cclp tree partial-order edge-types))
(provide (struct-out analysis))

(require anaphoric)

(define (save-analysis prog-analysis #:fn [fn #f])
  (let* ([out
          (open-output-file
           (or
            fn
            (serialized-filename
             (analysis-cclp prog-analysis)))
           #:exists 'truncate/replace)]
         [serialized (serialize prog-analysis)])
    (begin
      (write serialized out)
      (close-output-port out))))
(provide save-analysis)

(define (load-analysis fn)
  (let* ([in (open-input-file fn)]
         [loaded (deserialize (read in))])
    (close-input-port in)
    loaded))
(provide load-analysis)

(define (proceed prog-analysis)
  (match prog-analysis
    [(analysis (and source-prog (cclp clauses full-evaluations concrete-constants _ipo _q _fn k)) tree po)
     (let ([outcome (advance-analysis tree clauses full-evaluations concrete-constants po #:k k)])
       (match outcome
         [(cons 'underspecified-order candidate)
          (begin
            (displayln "Partial order is underspecified.")
            (displayln "Please select the atom which takes precedence.")
            (let* ([current-conjunction (label-conjunction (node-label candidate))]
                   [multis (filter multi? current-conjunction)]
                   [multi-conjuncts ; unfold case: one for maximum specificity
                    (append-map
                     (λ (m)
                       (let ([offset (apply max (cons 0 (assemble-var-indices (λ (_) #t) m)))])
                         (car (unfold-multi-bounded 1 m offset offset))))
                     multis)]
                   [options
                    (remove-duplicates
                     (map normalize-abstract-atom
                          (append
                           multi-conjuncts
                           (filter abstract-atom? current-conjunction))))]
                   [user-selection (prompt-for-selection options)]
                   [new-precedences
                    (filter-map
                     (λ (c)
                       (and (not (has-edge? po user-selection c))
                            (cons user-selection c)))
                     (remove user-selection options))]
                   [updated-po (graph-copy po)])
              (begin
                (for ([precedence new-precedences])
                  (add-directed-edge! updated-po (car precedence) (cdr precedence)))
                (proceed (analysis source-prog tree updated-po)))))]
         [(cons cand top) ; simple update to the analysis
          (analysis source-prog top po)]
         ['no-candidate prog-analysis]))]))
(provide
 (proc-doc/names
  proceed
  (-> analysis?
      analysis?)
  (prog-analysis)
  @{Advances the analysis of @racket[prog-analysis] without side-effects.
 The only exception is a user error which may be raised if the partial order in the analysis does not dictate which atom should be selected.}))

(define (analysis->current-genealogical-graph-skeleton prog-analysis)
  (match prog-analysis
    [(analysis _ tree _)
     (aif (active-branch tree)
          (genealogical-graph-skeleton it)
          #f)]))
(provide analysis->current-genealogical-graph-skeleton)

(define (analysis->current-genealogical-graph prog-analysis)
  (match prog-analysis
    [(analysis _ tree _)
     (let* ([active-branch (active-branch tree)]
            [gr (aif active-branch (genealogical-graph-skeleton it) #f)]
            [root (aif active-branch (gen-node (car (tree-label-conjunction (car it))) 1 #f #t #t) #f)]
            [depth (aif active-branch (length it) #f)]
            [targets (aif active-branch (map (λ (e) (struct-copy gen-node e [range (gen 0 #f)])) (candidate-targets gr)) #f)])
       (when active-branch
         (annotate-general! gr root targets depth))
       gr)]))

(define (save-genealogical-graph! prog-analysis #:fn [fn #f])
  (let* ([gg (analysis->current-genealogical-graph prog-analysis)]
         [serialized (serialize (get-edges gg))])
    (when gg
      (let ([out
             (open-output-file
              (or
               fn
               (path-replace-extension
                (last
                 (explode-path
                  (cclp-filename
                   (analysis-cclp prog-analysis))))
                (format
                 ".serializedgg.~a"
                 (size
                  (analysis-tree prog-analysis)))))
              #:exists 'truncate/replace)])
        (write serialized out)
        (close-output-port out)))))
(provide save-genealogical-graph!)

(define (load-genealogical-graph fn)
  (let* ([in (open-input-file fn)]
         [loaded (deserialize (read in))])
    (close-input-port in)
    (unweighted-graph/directed loaded)))
(provide load-genealogical-graph)

(define (show-analysis prog-analysis)
  (tree-display
   (analysis-tree
    prog-analysis)
   print-tree-label))
(provide show-analysis)

(define (visualize-partial-order my-analysis)
  (for ([precedence
         (in-edges
          (analysis-partial-order
           my-analysis))])
    ;; no need to show edges from more specific atoms to more general atoms
    ;; also no need to show indirectly reached atoms, but how to do that?
    (when (not (>=-extension (second precedence) (first precedence)))
      (displayln
       (format
        "~v < ~v"
        (first precedence)
        (second precedence))))))
(provide visualize-partial-order)

(define (write-svg! pict fn)
  (with-output-to-file
      fn
    (λ ()
      (display
       (convert
        pict
        'svg-bytes)))
    #:mode 'binary
    #:exists 'replace))
(provide write-svg!)

(define (cclp->initial-analysis program-data)
  (define initial-tree-label
    (tree-label
     (list
      (cclp-query program-data))
     (none)
     (list)
     #f
     #f))
  (define initial-tree
    (node initial-tree-label (list)))
  (analysis
   program-data
   initial-tree
   (graph-copy
    (cclp-initial-partial-order program-data))))
(provide cclp->initial-analysis)

(define (serialized-filename program-data)
  (path-replace-extension
   (last
    (explode-path
     (cclp-filename program-data)))
   ".serializedcclp"))

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
      (display "□" out)))

(define (print-tree-label t [out (current-output-port)])
  (match (node-label t)
    [(or (tree-label con sel _ _ i)
         (generalization con sel i _ _))
     (begin
       (when i (display (format "~v:" i) out))
       (print-conjunction con sel out))]
    [(cycle i)
     (display (format "cycle back to node ~a" i) out)]
    [(widening con sel msg idx)
     (begin
       (display "[widening]")
       (when idx (display (format "~v:" idx)))
       (print-conjunction con sel out))]))
(provide print-tree-label)
