#lang racket
(require rackunit)
(require (prefix-in ad: "../src/abstract-multi-domain.rkt"))
(require (prefix-in cd: "../src/concrete-domain.rkt"))
(require (prefix-in exp: "../src/cclp-expander.rkt"))

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

; TODO need to cover more cases