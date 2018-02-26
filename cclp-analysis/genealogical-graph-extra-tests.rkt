#lang racket
(require graph rackunit)
(require "abstract-multi-domain.rkt" "gen-graph-structs.rkt" "genealogical-graph.rkt")

(require (prefix-in o-primes-branch-tree: "analysis-trees/optimus-primes-branch.rkt")
         (prefix-in o-primes-branch-skeleton: "analysis-trees/optimus-primes-branch-gen-graph-skeleton.rkt"))
(define o-primes-branch (active-branch o-primes-branch-tree:val))
(define o-primes-graph-skeleton o-primes-branch-skeleton:val)
;(check-equal? (genealogical-graph-skeleton o-primes-branch) o-primes-graph-skeleton)

(define o-primes-skeleton-root (gen-node (abstract-atom 'oprimes (list (g 1) (a 1))) 1 #f #t))
(define o-primes-candidate-targets
  (list
   (gen-node (abstract-atom 'siftA (list (abstract-function 'cons (list (g 2) (a 4))) (a 3))) 7 #f #t)
   (gen-node (abstract-atom 'siftB (list (abstract-function 'cons (list (g 2) (a 6))) (a 1))) 13 #f #t)))
;(check-equal?
; (candidate-targets
;  o-primes-graph-skeleton)
; o-primes-candidate-targets)

(require
  (prefix-in o-primes-graph-annotated: "analysis-trees/optimus-primes-branch-gen-graph.rkt"))
(define o-primes-graph-annotated (graph-copy o-primes-graph-skeleton))
(annotate-general!
 o-primes-graph-annotated
 o-primes-skeleton-root
 ;; note: annotated the target atoms manually
 ;; candidate-targets do not have annotations yet, as they are drawn from the skeleton!
 (list
   (gen-node (abstract-atom 'siftA (list (abstract-function 'cons (list (g 2) (a 4))) (a 3))) 7 (gen 0 #f) #t)
   (gen-node (abstract-atom 'siftB (list (abstract-function 'cons (list (g 2) (a 6))) (a 1))) 13 (gen 0 #f) #t))
 (length o-primes-branch))
(define o-primes-annotated-root (struct-copy gen-node o-primes-skeleton-root [range (gen 0 #f)]))
(for ([lv (range 1 22)])
  (check-equal?
   (sort (rdag-level o-primes-graph-annotated o-primes-annotated-root lv) < #:key gen-node-id)
   (sort (rdag-level o-primes-graph-annotated:val o-primes-annotated-root lv)  < #:key gen-node-id)))
(check-equal?
 o-primes-graph-annotated
 o-primes-graph-annotated:val)