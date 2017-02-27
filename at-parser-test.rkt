#lang br
(require brag/support
         rackunit
         "at-parser.rkt"
         "at-tokenizer.rkt")

(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.!CY 1)"))
 '(at (cyclenode 1)))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.□)"))
 '(at (treelabel (selectionless-abstract-conjunction))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom)"))
 '(at (treelabel (selectionless-abstract-conjunction (abstract-atom "myatom")))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom1,myatom2)"))
 '(at (treelabel (selectionless-abstract-conjunction (abstract-atom "myatom1") (abstract-atom "myatom2")))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom(a))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-function "a"))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom(a(b(c))))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom"
                                                       (abstract-function "a"
                                                                          (abstract-function "b"
                                                                                             (abstract-function "c"))))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom(a,b))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-function "a") (abstract-function "b"))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom(a1))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-a-variable 1))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom(g1))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-g-variable 1))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom([]))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-list))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom([g1]))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-list (abstract-g-variable 1)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom([g1,g2]))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-list (abstract-g-variable 1) "," (abstract-g-variable 2)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom([g1,g2,g3]))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-list (abstract-g-variable 1) "," (abstract-g-variable 2) "," (abstract-g-variable 3)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom([g1|g2]))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (abstract-atom "myatom" (abstract-list (abstract-g-variable 1) "|" (abstract-g-variable 2)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.1.myatom,*myotheratom*,mythirdatom [myotheratom < myatom, myotheratom < mythirdatom])"))
 '(at
   (treelabel
    1
    (abstract-conjunction-selection (selectionless-abstract-conjunction (abstract-atom "myatom")) (abstract-atom "myotheratom") (selectionless-abstract-conjunction (abstract-atom "mythirdatom")))
    (precedence-list (precedence (abstract-atom "myotheratom") (abstract-atom "myatom")) (precedence (abstract-atom "myotheratom") (abstract-atom "mythirdatom"))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.multi((abc),#t,{},{},{}))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction (multi-abstraction (parameterized-abstract-conjunction (parameterized-abstract-atom "abc")) #t (init) (consecutive) (final))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom {} myfact.)"))
 '(at (treelabel (selectionless-abstract-conjunction (abstract-atom "myatom")) (abstract-substitution) (knowledge (fact (atom "myfact"))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.myatom {a1/g1, g2/nil} myhead(a1,a2) -> {a1/g1, a2/nil})"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction
     (abstract-atom "myatom"))
    (abstract-substitution
     (abstract-substitution-pair (abstract-a-variable 1) (abstract-g-variable 1))
     (abstract-substitution-pair (abstract-g-variable 2) (abstract-function "nil")))
    (knowledge
     (fullai-rule
      (abstract-atom "myhead" (abstract-a-variable 1) (abstract-a-variable 2))
      (abstract-substitution
       (abstract-substitution-pair (abstract-a-variable 1) (abstract-g-variable 1))
       (abstract-substitution-pair (abstract-a-variable 2) (abstract-function "nil"))))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.b,c {} a :- b,c.)"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction
     (abstract-atom "b")
     (abstract-atom "c"))
    (abstract-substitution)
    (knowledge (clause (atom "a") (conjunction (atom "b") (atom "c")))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.b,c {} a(X,foo,[X,Y]) :- b,c.)"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction
     (abstract-atom "b")
     (abstract-atom "c"))
    (abstract-substitution)
    (knowledge
     (clause (atom "a" (variable "X") (function "foo") (lplist (variable "X") "," (variable "Y")))
             (conjunction (atom "b") (atom "c")))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(.multi((abc(a<1,i,1>,a<1,i,2>)),#t,{},{},{}))"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction
     (multi-abstraction
      (parameterized-abstract-conjunction
       (parameterized-abstract-atom
        "abc"
        (parameterized-abstract-a-variable 1 "i" 1)
        (parameterized-abstract-a-variable 1 "i" 2)))
      #t
      (init)
      (consecutive)
      (final))))))
;(check-equal?
; (parse-to-datum (apply-tokenizer make-tokenizer "(.!GEN multi((abc),#t,{},{},{}))"))
; '(at
;   (generalization
;    (selectionless-abstract-conjunction
;     (multi-abstraction
;      (parameterized-abstract-conjunction
;       (parameterized-abstract-atom "abc"))
;      #t
;      (init)
;      (consecutive)
;      (final))))))
;(check-equal?
; (parse-to-datum
;  (apply-tokenizer
;   make-tokenizer
;   "(.collect(g1,a1),collect(g2,a2),append(a1,a2,a3)
;      (.!GEN collect(g1,a1),multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),
;             #f,
;             {a<1,1,1>/a1},
;             {a<1,i+1,2>/a<1,i,3>},
;             {a<1,L,3>/a3})))"))
; '(at
;   (treelabel
;    (selectionless-abstract-conjunction
;     (abstract-atom "collect" (abstract-g-variable 1) (abstract-a-variable 1))
;     (abstract-atom "collect" (abstract-g-variable 2) (abstract-a-variable 2))
;     (abstract-atom "append" (abstract-a-variable 1) (abstract-a-variable 2) (abstract-a-variable 3))))
;   (at
;    (generalization
;     (selectionless-abstract-conjunction
;      (abstract-atom "collect" (abstract-g-variable 1) (abstract-a-variable 1))
;      (multi-abstraction
;       (parameterized-abstract-conjunction
;        (parameterized-abstract-atom "collect"
;                                     (parameterized-abstract-g-variable 1 "i" 1)
;                                     (parameterized-abstract-a-variable 1 "i" 1))
;        (parameterized-abstract-atom "append"
;                                     (parameterized-abstract-a-variable 1 "i" 2)
;                                     (parameterized-abstract-a-variable 1 "i" 1)
;                                     (parameterized-abstract-a-variable 1 "i" 3)))
;       #f
;       (init (init-pair (parameterized-abstract-a-variable 1 1 1) (abstract-a-variable 1)))
;       (consecutive (consecutive-pair (parameterized-abstract-a-variable 1 "i+1" 2) (parameterized-abstract-a-variable 1 "i" 3)))
;       (final (final-pair (parameterized-abstract-a-variable 1 "L" 3) (abstract-a-variable 3)))))))))

(check-equal?
 (parse-to-datum
  (apply-tokenizer
   make-tokenizer
   "(.1.*primes(g1,a1)*)"))
 '(at (treelabel 1 (abstract-conjunction-selection (abstract-atom "primes" (abstract-g-variable 1) (abstract-a-variable 1))))))

(check-equal?
 (parse-to-datum
  (apply-tokenizer
   make-tokenizer
   "(.integers(g2,a6),sift(a6,a5),length(a5,g1)
  {a4/g1, a1/a5} primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N).)"))
 '(at
   (treelabel
    (selectionless-abstract-conjunction
     (abstract-atom "integers" (abstract-g-variable 2) (abstract-a-variable 6))
     (abstract-atom "sift" (abstract-a-variable 6) (abstract-a-variable 5))
     (abstract-atom "length" (abstract-a-variable 5) (abstract-g-variable 1)))
    (abstract-substitution
     (abstract-substitution-pair (abstract-a-variable 4) (abstract-g-variable 1))
     (abstract-substitution-pair (abstract-a-variable 1) (abstract-a-variable 5)))
    (knowledge
     (clause
      (atom "primes" (variable "N") (variable "Primes"))
      (conjunction
       (atom "integers" (function 2) (variable "I"))
       (atom "sift" (variable "I") (variable "Primes"))
       (atom "length" (variable "Primes") (variable "N"))))))))