#lang racket
(require racket/generator pretty-graphs cclp/interaction cclp/genealogical-graph-visualization cclp/gen-graph-structs graph math)
;(dag->pict
;    (load-genealogical-graph
;     "genealogical-graph-31")
;    gen-node->pict)


;; not very efficient because implementation likely computes n-1st,... prime as well
(define prime-gen (generator () (for ([i (in-naturals)]) (yield (nth-prime i)))))

;; this will work as long as there are nu multis - otherwise child could have multiple parents
(define (process-queue! g q)
  (unless (null? q)
    (let* ([h (first q)]
           [ch (get-neighbors g (car h))]
           [next-prime (prime-gen)]
           [new-id (* (cdr h) next-prime)]
           [ch/parent-id
            (map
             (λ (c)
               (cons c new-id))
             ch)])
      (rename-vertex!
       g
       (car h)
       (struct-copy
        gen-node
        (car h)
        [id new-id]))
      (process-queue! g (append (cdr q) ch/parent-id)))))
      
(write-svg!
 (let* ([gg
        (load-genealogical-graph
         "genealogical-graph-31")]
       [root
        (findf
         (λ (v)
           (equal?
            (gen-node-id v)
            1))
         (get-vertices gg))])
  (process-queue! gg (list (cons root 1)))
  (dag->pict gg gen-node->pict))
 "just-before-multi.svg")