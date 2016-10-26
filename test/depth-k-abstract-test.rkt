#lang racket
(require rackunit)

(require "../src/abstract-multi-domain.rkt")
(require "../src/depth-k-abstraction.rkt")

(check-equal? (abstract (a 1) 0) (a 1))
(check-equal? (abstract (g 1) 0) (g 1))

(check-equal? (abstract (a 1) 1) (a 1))
(check-equal? (abstract (g 1) 1) (g 1))

(check-equal?
 (abstract (abstract-atom 'foo (list (a 1))) 0)
 (abstract-atom 'foo (list (a 1))))
(check-equal?
 (abstract (abstract-atom 'foo (list (abstract-function 'bar '()))) 0)
 (abstract-atom 'foo (list (abstract-function 'bar '()))))
(check-equal?
 (abstract (abstract-atom 'foo (list (abstract-function 'bar (list (a 1) (a 2))))) 0)
 (abstract-atom 'foo (list (a 3))))
(check-equal?
 (abstract
  (abstract-atom
   'foo
   (list
    (abstract-function
     'bar
     (list
      (abstract-function 'baz (list (a 1)))
      (abstract-function 'baz (list (a 1)))))))
  1)
 (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2)))))
 "abstraction preserves aliasing within an atom")

(let ([test-atom
       (abstract-atom
        'foo
        (list
         (abstract-function
          'bar
          (list
           (abstract-function 'baz (list (a 1)))
           (abstract-function 'baz (list (a 1)))))))])
  (check-equal?
   (abstract (list test-atom test-atom) 1)
   (list
    (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2)))))
    (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2))))))
   "abstraction preserves aliasing within an abstract conjunction"))