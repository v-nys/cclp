; this is just a quick test program to see if I am interpreting the results of the profiler correctly
; it looks like replacing subtrees is very slow in larger trees
; this assumes a reasonably long analysis for primes has been serialized
; this program just deserializes it and replaces the candidate tree with something else
; if my hypothesis is correct, this will take a long time, even though it should be quite easy

#lang racket
(require racket/serialize)

(require racket-tree-utils/src/tree)
(require "abstract-analysis.rkt")
(require "cclp-interpreter.rkt")
(require "abstract-substitution.rkt")
(require "abstract-multi-domain.rkt")
(require "domain-switching.rkt")
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require "concrete-domain.rkt")

(define top-level-tree
  (deserialize (read (open-input-file "primes.serializedcclp"))))

(displayln "loaded")

(define bogus-tree (node 'label '()))

(define candidate-conjunction
  (interpret-abstract-conjunction
   (string-append
    "integers(γ36,α47),does_not_divide(γ25,γ34),filter(γ25,α47,α48),"
    "filter(γ27,cons(γ34,α48),α38),filter(γ28,α38,α40),sift(α40,α42),length(α42,γ32)")))

(displayln "interpreted conjunction")

(define candidate-substitution
  (list
   (abstract-equality (a 45) (g 25))
   (abstract-equality (a 46) (g 34))
   (abstract-equality (a 44) (a 47))
   (abstract-equality (a 35) (abstract-function 'cons (list (g 34) (a 48))))))

(define candidate-rule
  (ck:rule
   (atom
    'filter
    (list
     (variable 'N)
     (function
      'cons
      (list
       (variable 'M)
       (variable 'I)))
     (function
      'cons
      (list
       (variable 'M)
       (variable 'F)))))
   (list
    (atom 'does_not_divide (list (variable 'N) (variable 'M)))
    (atom 'filter (list (variable 'N) (variable 'I) (variable 'F))))))

(define candidate-tree
  (node (tree-label candidate-conjunction #f candidate-substitution candidate-rule #f) '()))

(displayln "defined candidate")

(displayln (replace-first-subtree top-level-tree candidate-tree bogus-tree))

(displayln "done")