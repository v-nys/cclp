#lang racket
(require rackunit)
(require (prefix-in ad: "../src/abstract-multi-domain.rkt"))
(require (prefix-in cd: "../src/concrete-domain.rkt"))
(require (prefix-in exp: "../src/cclp-expander.rkt"))
(require (prefix-in ph1-exp: (for-syntax "../src/cclp-expander.rkt")))
(require (prefix-in ph2-exp: (for-syntax (for-syntax "../src/cclp-expander.rkt"))))
(require (prefix-in fai: "../src/fullai-domain.rkt"))
(require syntax/macro-testing)

; concrete domain
(check-equal? (exp:number-term (exp:concrete-number 4)) (cd:function 4 '()))
(check-equal? (exp:lplist "[" "]") (cd:function 'nil '()))

; abstract domain
(check-equal? (exp:abstract-variable-a "α" 1) (ad:a 1))
(check-equal? (exp:abstract-variable-g "γ" 2) (ad:g 2))
(check-equal? (exp:abstract-number 3) (ad:abstract-function 3 '()))
(check-equal? (exp:concrete-number 4) (cd:function 4 '()))
(check-equal? (exp:abstract-variable (exp:abstract-variable-a "α" 1)) (ad:a 1))
(check-equal? (exp:abstract-variable (exp:abstract-variable-g "γ" 2)) (ad:g 2))
(check-equal? (exp:abstract-number-term (exp:abstract-number 3)) (ad:abstract-function 3 '()))
(check-equal? (exp:abstract-function-term "my-func") (ad:abstract-function 'my-func '()))
(check-equal? (exp:abstract-atom-without-args "my-atom") (ad:abstract-atom 'my-atom '()))
(check-equal? (exp:abstract-atom (exp:abstract-atom-without-args "my-atom"))
              (ad:abstract-atom 'my-atom '()))
(check-equal? (exp:abstract-lplist "[" "]") (ad:abstract-function 'nil '()))
(check-equal? (exp:abstract-lplist
               "["
               (exp:abstract-variable (exp:abstract-variable-g "γ" 2))
               ","
               (exp:abstract-variable (exp:abstract-variable-a "α" 1))
               "]")
              (ad:abstract-function
               'cons
               (list (ad:g 2)
                     (ad:abstract-function
                      'cons
                      (list (ad:a 1)
                            (ad:abstract-function 'nil '()))))))
(check-equal? (exp:abstract-lplist
               "["
               (exp:abstract-variable (exp:abstract-variable-g "γ" 2))
               ","
               (exp:abstract-variable (exp:abstract-variable-a "α" 1))
               "|"
               (exp:abstract-variable (exp:abstract-variable-a "α" 2))
               "]")
              (ad:abstract-function
               'cons
               (list (ad:g 2) (ad:abstract-function 'cons (list (ad:a 1) (ad:a 2))))))
(check-equal? (exp:conjunction
               (exp:abstract-atom (exp:abstract-atom-without-args "my-atom1"))
               "," (exp:abstract-atom (exp:abstract-atom-without-args "my-atom2"))
               "," (exp:abstract-atom (exp:abstract-atom-without-args "my-atom3")))
              (list (ad:abstract-atom 'my-atom1 '())
                    (ad:abstract-atom 'my-atom2 '())
                    (ad:abstract-atom 'my-atom3 '())))
(check-equal? (exp:abstract-function-term
               "my-func"
               "("
               (exp:abstract-term (exp:abstract-variable (exp:abstract-variable-g "γ" 1)))
               ","
               (exp:abstract-term (exp:abstract-variable (exp:abstract-variable-g "γ" 2))) ")")
              (ad:abstract-function 'my-func (list (ad:g 1) (ad:g 2))))
(check-equal? (exp:abstract-atom
               (exp:abstract-atom-with-args
                "my-atom"
                "("
                (exp:abstract-variable-g "γ" 1)
                ","
                (exp:abstract-variable-g "γ" 2)
                ")"))
              (ad:abstract-atom 'my-atom (list (ad:g 1) (ad:g 2))))

; concrete program section

; full eval section
(check-equal?
 (exp:fullai-rule-without-body
  (exp:abstract-atom-with-args
   "myatom"
   "("
   (exp:abstract-variable
    (exp:abstract-variable-g "γ" 1))
   ","
   (exp:abstract-variable
    (exp:abstract-variable-g "γ" 2))
   ")")
  ".")
 (fai:full-ai-rule
  (ad:abstract-atom 'myatom (list (ad:g 1) (ad:g 2)))
  (list)))

; prior section
(check-equal?
 ; this is needed, because we can't check the unquoted result
 ; it looks like a function evaluation
 (phase1-eval
  (ph1-exp:preprior-pair
   (ph2-exp:abstract-atom
    (ph2-exp:abstract-atom-without-args "myatom1"))
   ","
   (ph2-exp:abstract-atom
    (ph2-exp:abstract-atom-without-args "myatom2"))))
 '(before (myatom1) (myatom2)))

(check-equal?
 (phase1-eval
  (ph1-exp:preprior-pair
   (ph2-exp:abstract-atom
    (ph2-exp:abstract-atom-with-args
     "myatom1"
     "("
     (ph2-exp:abstract-variable-g "γ" 1)
     ","
     (ph2-exp:abstract-variable-g "γ" 2)
     ")"))
   ","
   (ph2-exp:abstract-atom
    (ph2-exp:abstract-atom-with-args
     "myatom2"
     "("
     (ph2-exp:abstract-variable-g "γ" 1)
     ","
     (ph2-exp:abstract-variable-g "γ" 2)
     ")"))))
 '(before (myatom1 (γ sym1) (γ sym2)) (myatom2 (γ sym1) (γ sym2))))