#lang at-exp racket
(require
  scribble/srcdoc
  "abstract-multi-domain.rkt"
  "gen-graph-structs.rkt")
(require (for-doc scribble/manual))

;; gets the origin from either a generation or a generation range
(define (gen-thing-origin gen-thing)
  (match gen-thing
    [(? gen-range?) (gen-range-origin gen-thing)]
    [(? gen?) (gen-origin gen-thing)]))

; Q: can I combine define and match?
(define (group-conjuncts node grouping)
  (match-let ([(list completed potential-abstraction current-gen) grouping])
    (match* (completed potential-abstraction current-gen node)
      [(_ #f (list) (gen-node conjunct _ (gen 0 #f) #f _))
       (list (append completed (list conjunct)) #f (list))]
      [(_ _ _ _) grouping])))

(define (generalize-level lvl)
  (first
   (foldl
    group-conjuncts
    (list (list) #f (list))
    (sort lvl < #:key gen-node-id))))
(module+ test
  (require rackunit)
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 3 (gen 3 1) #f  #t)
     (gen-node (abstract-atom 'append (list (a 1) (a 2) (a 3))) 4 (gen 3 1) #f  #t)
     (gen-node (abstract-atom 'collect (list (g 4) (a 4))) 5 (gen 2 1) #f  #t)
     (gen-node (abstract-atom 'append (list (a 3) (a 4) (a 5))) 6 (gen 2 1) #f  #t)
     (gen-node (abstract-atom 'collect (list (g 6) (a 6))) 7 (gen 1 1) #f  #t)
     (gen-node (abstract-atom 'append (list (a 5) (a 6) (a 7))) 8 (gen 1 1) #f  #t)
     (gen-node (abstract-atom 'collect (list (g 8) (a 8))) 9 (gen 0 #f) #f  #t)
     (gen-node (abstract-atom 'eq (list (a 7) (a 8))) 10 (gen 0 #f) #f  #t)))
   (list
    (abstract-atom 'collect (list (g 1) (a 1)))
    (abstract-atom 'collect (list (g 2) (a 2)))
    (abstract-atom 'append (list (a 1) (a 2) (a 3)))
    (multi
     (list
      (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
      (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
     #f
     (init
      (list
       (cons (g* 1 1 1) (g 4))
       (cons (a* 1 1 1) (a 4))
       (cons (a* 1 1 2) (a 3))
       (cons (a* 1 1 3) (a 5))))
     (consecutive
      (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
     (final
      (list
       (cons (g* 1 'L 1) (g 6))
       (cons (a* 1 'L 1) (a 5))
       (cons (a* 1 'L 2) (a 6))
       (cons (a* 1 'L 3) (a 7)))))))
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
     (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 3 (gen 1 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 3) (a 2) (a 3))) 4 (gen 2 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5))) 5 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 6) (a 5) (a 6))) 6 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 7) (a 6) (a 7))) 7 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 8) (a 7) (a 8))) 8 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'sift (list (a 8) (a 9))) 9 (gen 6 1) #f #t)
     (gen-node (abstract-atom 'len (list (a 9) (g 9))) 10 (gen 0 #f) #f #t)))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 2))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive
      (list
       (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 3))
       (cons (a* 1 'L 1) (a 2))
       (cons (a* 1 'L 2) (a 3)))))
    (abstract-atom 'filter (list (g 4) (abstract-function 'cons (list (g 5) (a 3) (a 4))) (a 5)))
    (multi
     (list (abstract-atom* 'filter (list (g* 2 'i 1) (a* 2 'i 1) (a* 2 'i 2))))
     #t
     (init
      (list
       (cons (g* 2 1 1) (g 6))
       (cons (a* 2 1 1) (a 5))
       (cons (a* 2 1 2) (a 6))))
     (consecutive
      (list
       (cons (a* 2 'i+1 1) (a* 2 'i 2))))
     (final
      (list
       (cons (g* 2 'L 1) (g 7))
       (cons (a* 2 'L 1) (a 6))
       (cons (a* 2 'L 2) (a 7)))))
    (abstract-atom 'filter (list (g 8) (a 7) (a 8)))
    (abstract-atom 'sift (list (a 8) (a 9)))
    (abstract-atom 'len (list (a 9) (g 9)))))
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 2 (gen 0 #f) #f #t)
     (gen-node
      (multi
       (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
       #t
       (init
        (list
         (cons (g* 1 1 1) (g 2))
         (cons (a* 1 1 1) (a 1))
         (cons (a* 1 1 2) (a 2))))
       (consecutive
        (list
         (cons (a* 1 'i+1 1) (a* 1 'i 2))))
       (final
        (list
         (cons (g* 1 'L 1) (g 3))
         (cons (a* 1 'L 1) (a 2))
         (cons (a* 1 'L 2) (a 3)))))
      3
      (gen-range 1 2 1 #t)
      #f
      #t)
     (gen-node (abstract-atom 'filter (list (g 4) (a 3) (a 4))) 4 (gen 3 1) #f #t)
     (gen-node (abstract-atom 'filter (list (abstract-function 'cons (list (g 5) (a 4) (a 5))))) 5 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'filter (list (g 6) (a 5) (a 6))) 6 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'sift (list (a 6) (a 7))) 7 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'len (list (a 7) (g 7))) 8 (gen 0 #f) #f #t)))
   (list
    (abstract-atom 'integers (list (g 1) (a 1)))
    (multi
     (list (abstract-atom* 'filter (list (g* 1 'i 1) (a* 1 'i 1) (a* 1 'i 2))))
     #t
     (init
      (list
       (cons (g* 1 1 1) (g 2))
       (cons (a* 1 1 1) (a 1))
       (cons (a* 1 1 2) (a 2))))
     (consecutive
      (list
       (cons (a* 1 'i+1 1) (a* 1 'i 2))))
     (final
      (list
       (cons (g* 1 'L 1) (g 4))
       (cons (a* 1 'L 1) (a 3))
       (cons (a* 1 'L 2) (a 4)))))
    (abstract-atom 'filter (list (g 6) (a 5) (a 6)))
    (abstract-atom 'sift (list (a 6) (a 7)))
    (abstract-atom 'len (list (a 7) (g 7)))))
  (check-equal?
   (generalize-level
    (list
     (gen-node (abstract-atom 'collect (list (g 1) (a 1))) 2 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 2) (a 2))) 3 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 1) (a 2) (a 3))) 4 (gen 5 1) #f #t)
     (gen-node (abstract-atom 'collect (list (g 4) (a 4))) 5 (gen 4 1) #f #t)
     (gen-node (abstract-atom 'append (list (a 3) (a 4) (a 5))) 6 (gen 4 1) #f #t)
     (gen-node
      (multi
       (list
        (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
        (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
       #f
       (init
        (list
         (cons (a* 1 1 2) (a 5))))
       (consecutive
        (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
       (final
        (list
         (cons (g* 1 'L 1) (g 7))
         (cons (a* 1 'L 1) (a 8))
         (cons (a* 1 'L 2) (a 9))
         (cons (a* 1 'L 3) (a 10)))))
      7
      (gen-range 3 1 1 #f)
      #f
      #t)
     (gen-node (abstract-atom 'collect (list (g 11) (a 11))) 8 (gen 0 #f) #f #t)
     (gen-node (abstract-atom 'eq (list (a 10) (a 11))) 9 (gen 0 #f) #f #t)))
   (list
    (abstract-atom 'collect (list (g 1) (a 1)))
    (abstract-atom 'collect (list (g 2) (a 2)))
    (abstract-atom 'append (list (a 1) (a 2) (a 3)))
    (multi
     (list
      (abstract-atom* 'collect (list (g* 1 'i 1) (a* 1 'i 1)))
      (abstract-atom* 'append (list (a* 1 'i 2) (a* 1 'i 1) (a* 1 'i 3))))
     #f
     (init
      (list
       (cons (g* 1 1 1) (g 4))
       (cons (a* 1 1 1) (a 4))
       (cons (a* 1 1 2) (a 3))
       (cons (a* 1 1 3) (a 5))))
     (consecutive
      (list (cons (a* 1 'i+1 2) (a* 1 'i 3))))
     (final
      (list
       (cons (g* 1 'L 1) (g 7))
       (cons (a* 1 'L 1) (a 8))
       (cons (a* 1 'L 2) (a 9))
       (cons (a* 1 'L 3) (a 10)))))
    (abstract-atom 'collect (list (g 11) (a 11)))
    (abstract-atom 'eq (list (a 10) (a 11)))))
  (let ([node1 (gen-node (abstract-atom 'allsafe (list (g 1) (g 2) (g 3) (abstract-function 'cons (list (g 4) (a 1))))) 2 (gen 1 1) #f #f)]
        [node2
         (gen-node
          (multi
           (list
            (abstract-atom* 'allsafe (list (g* 1 'i 1) (g* 1 'i 2) (g* 1 'i 3) (abstract-function* 'cons (list (g* 1 'i 4) (a* 1 'i 1))))))
           #t
           (init (list (cons (g* 1 1 4) (g 4))))
           (consecutive
            (list
             (cons (g* 1 'i+1 4) (g* 1 'i 4))
             (cons (a* 1 'i+1 1) (a* 1 'i 1))))
           (final (list)))
          3
          (gen-range 2 5 1 #t)
          #f
          #f)])
    (check-equal?
     (generalize-level (list node1 node2))
     (list (gen-node-conjunct node1) (gen-node-conjunct node2)))))
(provide
 (proc-doc/names
  generalize-level
  (-> (listof gen-node?) (listof abstract-conjunct?))
  (lvl)
  @{Attempts to generalize the conjunction represented by @racket[lvl].}))