#lang racket
(require rackunit)
(require (prefix-in ad: "../src/abstract-multi-domain.rkt"))
(require (prefix-in cd: "../src/concrete-domain.rkt"))
(require (prefix-in exp: "../src/cclp-expander.rkt"))

; atomic elements: abstract variables, concrete variables and both types of numbers
; no structures that can be peeled away
(check-equal? (exp:abstract-variable-a "α" 1) (ad:a 1))
(check-equal? (exp:abstract-variable-g "γ" 2) (ad:g 2))
(check-equal? (exp:abstract-number 3) (ad:abstract-function 3 '()))
(check-equal? (exp:concrete-number 4) (cd:function 4 '()))

; abstract variables, with layers that can be peeled away
(check-equal? (exp:abstract-variable (exp:abstract-variable-a "α" 1)) (ad:a 1))
(check-equal? (exp:abstract-variable (exp:abstract-variable-g "γ" 2)) (ad:g 2))

; numbers with layer that can be peeled away
(check-equal? (exp:abstract-number-term (exp:abstract-number 3)) (ad:abstract-function 3 '()))
(check-equal? (exp:number-term (exp:concrete-number 4)) (cd:function 4 '()))

; function-term : (SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]) | number-term
; concrete function terms without nested arguments

; concrete function terms with nested arguments