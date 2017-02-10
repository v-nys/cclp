#lang br
(require brag/support
         rackunit
         "at-parser.rkt"
         "at-tokenizer.rkt")

(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(!CY 1)"))
 '(at (cyclenode 1)))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(□)"))
 '(at (treelabel ())))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom)"))
 '(at (treelabel ((abstract-atom "myatom")))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom1,myatom2)"))
 '(at (treelabel ((abstract-atom "myatom1") (abstract-atom "myatom2")))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom(a))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-function "a"))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom(a(b(c))))"))
 '(at
   (treelabel
    ((abstract-atom "myatom"
                    (abstract-function "a"
                                       (abstract-function "b"
                                                          (abstract-function "c"))))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom(a,b))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-function "a") (abstract-function "b"))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom(a1))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-a-variable 1))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom(g1))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-g-variable 1))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom([]))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-list))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom([g1]))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-list (abstract-g-variable 1)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom([g1,g2]))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-list (abstract-g-variable 1) "," (abstract-g-variable 2)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom([g1,g2,g3]))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-list (abstract-g-variable 1) "," (abstract-g-variable 2) "," (abstract-g-variable 3)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(myatom([g1|g2]))"))
 '(at
   (treelabel
    ((abstract-atom "myatom" (abstract-list (abstract-g-variable 1) "|" (abstract-g-variable 2)))))))
(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer "(multi((abc),#t,{},{},{}))"))
 '(at
   (treelabel
    ((multi-abstraction (parameterized-abstract-conjunction (parameterized-abstract-atom "abc")) #t (init) (consecutive) (final))))))