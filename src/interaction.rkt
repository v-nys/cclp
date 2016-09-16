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
(require (only-in "fullai-domain.rkt" full-ai-rule?))
(require (only-in "execution.rkt" priority?))
(require (only-in "abstract-multi-domain.rkt" abstract-atom?))
(require racket-tree-utils/src/tree (only-in racket-tree-utils/src/printer tree-display))

(struct tree-label (conjunction selection substitution rule))
(define display-tree-label display)

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
    [(node (tree-label '() _ _ _) '()) #t]
    [(node (tree-label _ _ _ _) '()) #f]
    [(node (tree-label _ _ _ _) children) (andmap completed-tree? children)]))


(define (candidate-for-update tree)
  (match tree
    [(node (tree-label '() _ _ _) '()) (none)]
    [(node (tree-label c (none) s r) '()) (some (tree-label c (none) s r))]
    [(node (tree-label _ (some v) _ _) children)
     (if (andmap completed-tree? children)
         (none)
         (candidate-for-update (car (filter (λ (c) (not (completed-tree? c))) children))))]
    [(node 'fail '()) (none)]
    [(node 'cycle '()) (none)]
    [_ (error 'missing-pattern)]))


(define (interactive-analysis tree clauses full-evaluations preprior)
  (define-values (show-top proceed go-back save end)
    (values "show top level" "proceed" "go back" "save analysis" "end analysis"))
  (define choice (prompt-for-answer "What do you want to do?" show-top proceed go-back save end))
  (cond [(equal? choice show-top)
         (begin (tree-display tree display-tree-label)
                (interactive-analysis tree clauses full-evaluations preprior))]
        [(equal? choice proceed)
         (begin (define candidate (candidate-for-update tree))
                (if (some? candidate)
                    (begin (define next-tree )
                           )
                    (display "There are no nodes left to analyze."))
                (interactive-analysis next-tree clauses full-evaluations preprior))]
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
     (begin (define initial-tree-label (tree-label (list (4-tuple-fourth program-data)) (none) (list) #f))
            (define initial-tree (node initial-tree-label (list)))
            (interactive-analysis initial-tree clauses full-evaluations preprior))]))

(define (cclp-run filename program-data)
  (define serialized-filename
    (path-replace-extension (last (explode-path filename)) ".serializedcclp"))
  (define-values (analysis load quit)
    (values "analyze this program" "load existing analysis" "quit"))
  (define choice (prompt-for-answer "What do you want to do?" analysis load quit))
  (cond [(equal? choice analysis)
         (begin (begin-analysis program-data)
                (cclp-run filename program-data))]
        [(equal? choice load)
         (begin (load-analysis serialized-filename)
                (cclp-run filename program-data))]
        [(equal? choice quit) (void)]
        [else (error 'unsupported)]))
(provide (contract-out
          [cclp-run
           (-> path?
               (4-tupleof (listof rule?) (listof full-ai-rule?) (listof priority?) abstract-atom?)
               void?)]))