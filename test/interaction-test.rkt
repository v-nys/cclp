#lang racket
(require rackunit)
(require "../src/interaction.rkt")
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require "../src/data-utils.rkt")
(require racket-tree-utils/src/tree)

(test-case
 "candidate and predecessors for various scenarios"
 (let ([tree (node (tree-label (list (abp:parse-abstract-atom "foo(γ1)")) (none) '() #f #f) '())])
   (check-equal? (candidate-and-predecessors tree '())
                 (cons (some tree) '())))
 (let* ([leaf1
         (node (tree-label (list (abp:parse-abstract-atom "bar(γ1)")) (none) '() #f #f) '())]
        [leaf2
         (node (tree-label (list (abp:parse-abstract-atom "baz(α1)")) (none) '() #f #f) '())]
        [tree
         (node
          (tree-label (list (abp:parse-abstract-atom "foo(γ1)")) (some 0) '() #f 1)
          (list leaf1 leaf2))])
   (begin
     (check-equal?
      (node-label (some-v (car (candidate-and-predecessors tree '()))))
      (node-label leaf1))
     (check-equal?
      (cdr (candidate-and-predecessors tree '()))
      (list (cons (list (abp:parse-abstract-atom "foo(γ1)")) 1)))))
 (let* ([leaf1 (node 'fail '())]
        [middle
         (node
          (tree-label (list (abp:parse-abstract-atom "bar(γ1)")) (some 0) '() #f 2)
          (list leaf1))]
        [leaf2
         (node (tree-label (list (abp:parse-abstract-atom "baz(α1)")) (none) '() #f #f) '())]
        [tree
         (node
          (tree-label (list (abp:parse-abstract-atom "foo(γ1)")) (some 0) '() #f 1)
          (list middle leaf2))])
   (begin
     (check-equal?
      (car (candidate-and-predecessors tree '()))
      (some leaf1))
     (check-equal?
      (cdr (candidate-and-predecessors tree '()))
      (list
       (cons (list (abp:parse-abstract-atom "bar(γ1)")) 2)
       (cons (list (abp:parse-abstract-atom "foo(γ1)")) 1))))))