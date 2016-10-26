#lang racket
(require rackunit)

(require "../src/abstract-multi-domain.rkt")
(require "../src/depth-k-abstraction.rkt")

(check-equal? (abstract (a 1) 1) (a 1))
(check-equal? (abstract (g 1) 1) (g 1))

(check-equal? (abstract (a 1) 2) (a 1))
(check-equal? (abstract (g 1) 2) (g 1))

(check-equal?
 (abstract (abstract-atom 'foo (list (a 1))) 1)
 (abstract-atom 'foo (list (a 1))))
(check-equal?
 (abstract (abstract-atom 'foo (list (abstract-function 'bar '()))) 1)
 (abstract-atom 'foo (list (abstract-function 'bar '()))))
(check-equal?
 (abstract (abstract-atom 'foo (list (abstract-function 'bar (list (a 1) (a 2))))) 1)
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
  2)
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
   (abstract (list test-atom test-atom) 2)
   (list
    (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2)))))
    (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2))))))
   "abstraction preserves aliasing within an abstract conjunction"))
(check-equal?
 (abstract
  (abstract-function ; depth is 5 - occurs at level 1
   'foo
   (list
    (abstract-function ; depth is 2 - occurs at level 2
     'bar
     (list
      (g 1) ; depth is 1
      (a 2))) ; depth is 1
    (abstract-function ; depth is 4 - occurs at level 2
     'cons
     (list
      (g 3)
      (abstract-function ; depth is 3 - occurs at level 3
       'cons
       (list
        (abstract-function ; depth is 2 - occurs at level 4
         'bar
         (list
          (g 1) ; depth is 1 - occurs at level 5
          (a 2))) ; depth is 1 - occurs at level 5
        (abstract-function
         'nil ; depth is 1 - occurs at level 5
         '())))))))
  4)
 (abstract-function ; depth is at most 4
   'foo
   (list ; elements have depth at most 3
    (abstract-function
     'bar
     (list ; elements have depth at most 2
      (g 1)
      (a 2)))
    (abstract-function
     'cons
     (list ; elements have depth at most 2
      (g 3)
      (abstract-function
       'cons
       (list ; elements have depth 1
        (a 3)
        (abstract-function
         'nil
         '())))))))
 "aliasing can be lost if aliased terms occur at different levels")