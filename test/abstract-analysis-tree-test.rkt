#lang racket
(require rackunit)
(require racket-tree-utils/src/tree)
(require "../src/data-utils.rkt")
(require "../src/abstract-analysis.rkt")
(require "../src/cclp-interpreter.rkt")
(require "../src/abstract-analysis-tree.rkt")

(test-case
 "candidate and predecessors for various scenarios"
 (let ([tree (node (tree-label (list (interpret-abstract-atom "foo(γ1)")) (none) '() #f #f) '())])
   (check-equal? (candidate-and-predecessors tree '())
                 (cons (some tree) '())))
 (let* ([leaf1
         (node (tree-label (list (interpret-abstract-atom "bar(γ1)")) (none) '() #f #f) '())]
        [leaf2
         (node (tree-label (list (interpret-abstract-atom "baz(α1)")) (none) '() #f #f) '())]
        [tree
         (node
          (tree-label (list (interpret-abstract-atom "foo(γ1)")) (some 0) '() #f 1)
          (list leaf1 leaf2))])
   (begin
     (check-equal?
      (node-label (some-v (car (candidate-and-predecessors tree '()))))
      (node-label leaf1))
     (check-equal?
      (cdr (candidate-and-predecessors tree '()))
      (list (cons (list (interpret-abstract-atom "foo(γ1)")) 1)))))
 (let* ([leaf1 (node 'fail '())]
        [middle
         (node
          (tree-label (list (interpret-abstract-atom "bar(γ1)")) (some 0) '() #f 2)
          (list leaf1))]
        [leaf2
         (node (tree-label (list (interpret-abstract-atom "baz(α1)")) (none) '() #f #f) '())]
        [tree
         (node
          (tree-label (list (interpret-abstract-atom "foo(γ1)")) (some 0) '() #f 1)
          (list middle leaf2))])
   (begin
     (check-equal?
      (car (candidate-and-predecessors tree '()))
      (some leaf1))
     (check-equal?
      (cdr (candidate-and-predecessors tree '()))
      (list
       (cons (list (interpret-abstract-atom "bar(γ1)")) 2)
       (cons (list (interpret-abstract-atom "foo(γ1)")) 1)))))
 (let* ([bottom-left (node (cycle 1) '())]
        [above-bottom-left
         (node (tree-label (list (interpret-abstract-atom "a")) (none) (list) #f 3)
               (list bottom-left))]
        [left-of-root
         (node (tree-label (list (interpret-abstract-atom "b")) (some 0) (list) #f 2)
               (list above-bottom-left))]
        [bottom-right
         (node (tree-label (list (interpret-abstract-atom "c")) (none) (list) #f #f) '())]
        [tree
         (node (tree-label (list (interpret-abstract-atom "a")) (some 0) (list) #f 1)
               (list left-of-root bottom-right))])
   (check-equal?
    (car (candidate-and-predecessors tree '()))
    (some bottom-right))))