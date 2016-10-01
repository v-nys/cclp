#lang racket
(require rackunit)
(require (prefix-in ad: "../src/abstract-multi-domain.rkt"))
(require (prefix-in cd: "../src/concrete-domain.rkt"))
(require (prefix-in exp: "../src/cclp-expander.rkt"))
(require (prefix-in ph1-exp: (for-syntax "../src/cclp-expander.rkt")))
(require (prefix-in ph2-exp: (for-syntax (for-syntax "../src/cclp-expander.rkt"))))
(require syntax/macro-testing)

(check-equal? (exp:abstract-variable-a "α" 1) (ad:a 1))
(check-equal? (exp:abstract-variable-g "γ" 2) (ad:g 2))
(check-equal? (exp:abstract-number 3) (ad:abstract-function 3 '()))
(check-equal? (exp:concrete-number 4) (cd:function 4 '()))

(check-equal? (exp:abstract-variable (exp:abstract-variable-a "α" 1)) (ad:a 1))
(check-equal? (exp:abstract-variable (exp:abstract-variable-g "γ" 2)) (ad:g 2))

; note: (abstract) number terms could be folded in the grammar...
(check-equal? (exp:abstract-number-term (exp:abstract-number 3)) (ad:abstract-function 3 '()))
(check-equal? (exp:number-term (exp:concrete-number 4)) (cd:function 4 '()))

(check-equal? (exp:lplist "[" "]") (cd:function 'nil '()))
(check-equal? (exp:abstract-function-term "my-func") (ad:abstract-function 'my-func '()))
(check-equal? (exp:abstract-atom-without-args "my-atom") (ad:abstract-atom 'my-atom '()))
(check-equal? (exp:abstract-atom (exp:abstract-atom-without-args "my-atom")) (ad:abstract-atom 'my-atom '()))
(check-equal? (exp:conjunction
               (exp:abstract-atom (exp:abstract-atom-without-args "my-atom1"))
               "," (exp:abstract-atom (exp:abstract-atom-without-args "my-atom2"))
               "," (exp:abstract-atom (exp:abstract-atom-without-args "my-atom3")))
              (list (ad:abstract-atom 'my-atom1 '())
                    (ad:abstract-atom 'my-atom2 '())
                    (ad:abstract-atom 'my-atom3 '())))

(check-equal?
 (phase1-eval
  (ph1-exp:preprior-pair
   (ph2-exp:abstract-atom
    (ph2-exp:abstract-atom-without-args "myatom1"))
   ","
   (ph2-exp:abstract-atom
    (ph2-exp:abstract-atom-without-args "myatom2"))))
 '(before (myatom1) (myatom2)))