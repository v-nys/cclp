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
(require terminal-color)

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
        (hash2-recur (tree-label-index l))))])

(define (print-tree-label t [out (current-output-port)])
  (match (node-label t)
    [(tree-label con sel sub r i)
     (begin
       (display (format "~v:" i))
       (print-conjunction con sel out)
       (when ((compose not null?) sub) (begin (display " " out) (print-substitution sub out))))]))



; TODO: transliterate this to get good tree representations
;displayResolution :: TreeLabel -> IO ()
;displayResolution (Label sub con (Just (selected, fullAI)) prohibited) = 
;  do putStr $ if not (null sub) then show sub ++ " " else ""
;     let (upToSelected, fromSelected) = case (elemIndex selected con) of (Just i) -> (splitAt i con)
;         afterSelected = tail fromSelected
;         in do if not (null upToSelected) then putStr $ showConjunction upToSelected ++ " ^ " else putStr ""
;               if fullAI then setSGR [SetConsoleIntensity BoldIntensity] else setSGR [SetUnderlining SingleUnderline]
;               putStr $ showConjunction [selected]
;               setSGR [SetUnderlining NoUnderline]
;               setSGR [SetConsoleIntensity NormalIntensity]
;               if not (null afterSelected) then putStrLn $ " ^ " ++ showConjunction afterSelected else putStrLn ""
;displayResolution Cycle = putStrLn $ "CYCLE"
;displayResolution Failure = putStrLn $ "FAIL"
;displayResolution other@(Label sub con Nothing prohibited) =
;  do putStr $ if not (null sub) then show sub ++ " " else ""
;     putStrLn $ showConjunction con


;completed :: Tree TreeLabel -> Bool
;completed (Node Failure []) = True
;completed (Node Cycle []) = True
;completed (Node (Label _ [] _ _) []) = True
;completed (Node (Label _ _ _ _) []) = False
;completed (Node (Label _ _ _ _) children) = all completed children

(define (completed-tree? t)
  (match t
    [(node 'fail '()) #t]
    [(node 'cycle '()) #t]
    [(node (tree-label '() _ _ _ _) '()) #t]
    [(node (tree-label _ _ _ _ _) '()) #f]
    [(node (tree-label _ _ _ _ _) children) (andmap completed-tree? children)]))


(define (candidate-for-update tree)
  (match tree
    [(node (tree-label '() _ _ _ _) '()) (none)]
    [(node (tree-label c (none) s r i) '()) (some (node (tree-label c (none) s r i) '()))]
    [(node (tree-label _ (some v) _ _ _) children)
     (if (andmap completed-tree? children)
         (none)
         (candidate-for-update (car (filter (λ (c) (not (completed-tree? c))) children))))]
    [(node 'fail '()) (none)]
    [(node 'cycle '()) (none)]
    [_ (error 'missing-pattern)]))

(define (resolvent->node res)
  (node
   (tree-label
    (resolvent-conjunction res)
    (none)
    (resolvent-substitution res)
    (resolvent-knowledge res)
    1) ; just to check - should probably make this a parameter
   (list)))

(define (interactive-analysis tree clauses full-evaluations preprior)
  (define-values (show-top proceed go-back save end)
    (values "show top level" "proceed" "go back" "save analysis" "end analysis"))
  (define choice (prompt-for-answer "What do you want to do?" show-top proceed go-back save end))
  (cond [(equal? choice show-top)
         (begin (newline)
                (tree-display tree print-tree-label)
                (newline)
                (interactive-analysis tree clauses full-evaluations preprior))]
        [(equal? choice proceed)
         (begin (define candidate (candidate-for-update tree))
                (if (none? candidate)
                    (begin (display "There are no nodes left to analyze.")
                           (interactive-analysis tree clauses full-evaluations preprior))
                    (let* ([candidate-label (node-label (some-v candidate))]
                           [conjunction (tree-label-conjunction candidate-label)]
                           [resolution-result (abstract-resolve conjunction preprior clauses full-evaluations)]
                           [index-selection (car resolution-result)]
                           [resolvents (cdr resolution-result)]
                           [child-trees (map resolvent->node resolvents)]
                           [updated-candidate (node
                                               (tree-label (tree-label-conjunction candidate-label)
                                                           (some index-selection)
                                                           (tree-label-substitution candidate-label)
                                                           (tree-label-rule candidate-label)
                                                           (tree-label-index candidate-label))
                                               child-trees)]
                           [updated-top (replace-first-subtree tree (some-v candidate) updated-candidate)])
                      (begin
                        (newline)
                        (tree-display updated-candidate print-tree-label)
                        (newline)
                        (interactive-analysis updated-top clauses full-evaluations preprior)))))]
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
     (begin (define initial-tree-label (tree-label (list (4-tuple-fourth program-data)) (none) (list) #f 1))
            (define initial-tree (node initial-tree-label (list)))
            (interactive-analysis initial-tree clauses full-evaluations preprior))]))

(define (cclp-run filename program-data)
  (log-info "Entered top-level menu for program ~a with data ~s" filename program-data)
  
  ;  (begin
  ;    (when (colorterm?) (foreground 'magenta))
  ;    (displayln "Color output test")
  ;    (displayln (capability? 'blink))
  ;    (displayln (capability? 'underline))
  ;    (displayln (capability? 'bold))
  ;    (when (colorterm?) (reset)))
  
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