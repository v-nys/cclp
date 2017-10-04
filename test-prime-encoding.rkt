#lang racket
(require racket/generator pretty-graphs cclp/interaction cclp/genealogical-graph-visualization cclp/gen-graph-structs graph math)


(define (assign-prime-factor-ids! g)
  (define (aux g q id-lb ignored-children)
    (unless (null? q)
      (let* ([h (first q)] ; head of the queue is a node in the graph
             [ch (filter ; to avoid processing new multis several times
                  (λ (c) (not (set-member? (gen-node-id c) ignored-children)))
                  (get-neighbors g (car h)))]
             [chosen-prime (next-prime (ceiling (/ id-lb (cdr h))))]
             [new-id (* (cdr h) chosen-prime)]
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
        (aux
         g
         (append (cdr q) ch/parent-id)
         new-id
         (set-union
          ignored-children
          (list->set
           (map
            gen-node-id
            ch)))))))
  (aux g (list (cons (first (tsort g)) 1)) 1 (set)))
(provide
 (proc-doc/names
  assign-prime-factor-ids!
  (-> genealogical-graph? void?)
  (g)
  @{Assigns ID's to the @racket[gen-node?] elements in the genealogical graph @racket[g], so that the ID of each node is sufficient to determine the logical ancestors of that node.
            ID's are assigned in a breadth-first manner and the eventual ID of a conjunct is always lower than those of its right siblings as well as of conjuncts at a deeper level.}))

;; TODO: test!
;; (process-queue! gg (list (cons root 1)) 1 (set))