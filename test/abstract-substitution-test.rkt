#lang racket
(require rackunit)
(require "abstract-domain-boilerplate.rkt")
(require "../src/data-utils.rkt")
(require "../src/abstract-multi-domain.rkt")
(require "../src/abstraction-inspection-utils.rkt")
(require "../src/abstract-substitution.rkt")
(require (prefix-in ak: "../src/abstract-knowledge.rkt"))
(require "../src/cclp-interpreter.rkt")

(check-equal? (maximum-var-index (interpret-abstract-term "γ1") g?) (some 1))
(check-equal? (maximum-var-index (interpret-abstract-term "γ1") a?) (none))
(check-equal? (maximum-var-index (interpret-abstract-term "α2") a?) (some 2))
(check-equal? (maximum-var-index (interpret-abstract-term "α2") g?) (none))

(check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,α2)") g?) (some 1))
(check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,α2)") a?) (some 2))
(check-equal? (maximum-var-index (interpret-abstract-term "foo(γ1,γ2)") a?) (none))
(check-equal? (maximum-var-index (interpret-abstract-term "foo(α1,α2)") g?) (none))

(check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,α2)") g?) (some 1))
(check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,α2)") a?) (some 2))
(check-equal? (maximum-var-index (interpret-abstract-atom "foo(γ1,γ2)") a?) (none))
(check-equal? (maximum-var-index (interpret-abstract-atom "foo(α1,α2)") g?) (none))

(check-equal? (substitute-in-substitution (interpret-abstract-term "γ5") (interpret-abstract-term "α1") (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))
              (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,γ5,α2))"))))
(check-equal? (substitute-in-substitution (interpret-abstract-term "γ5") (interpret-abstract-term "α4") (list (abstract-equality (interpret-abstract-term "α4") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))
              (list (abstract-equality (interpret-abstract-term "γ5") (interpret-abstract-term "foo(bar(α3,α1,α2))"))))

(check-equal? (apply-substitution-to-term (list (abstract-equality (g 1) (interpret-abstract-term "quux")) (abstract-equality (a 2) (g 4)))
                                          (interpret-abstract-term "foo(bar(γ1,α1),baz(γ2,α2,α3))"))
              (interpret-abstract-term "foo(bar(quux,α1),baz(γ2,γ4,α3))"))

(check-equal? (apply-substitution-to-conjunct (list (abstract-equality (g 1) (interpret-abstract-term "quux")) (abstract-equality (a 2) (g 4)))
                                              (interpret-abstract-atom "foo(bar(γ1,α1),baz(γ2,α2,α3))"))
              (interpret-abstract-atom "foo(bar(quux,α1),baz(γ2,γ4,α3))"))

(check-equal? (apply-substitution-to-conjunction (list (abstract-equality (g 1) (interpret-abstract-term "quux")) (abstract-equality (a 2) (g 4)))
                                                 (list (interpret-abstract-atom "foo(bar(γ1,α1),baz(γ2,α2,α3))") (interpret-abstract-atom "zip(zoom(γ1,α1),kweh(α2,γ2,α5))")))
              (list (interpret-abstract-atom "foo(bar(quux,α1),baz(γ2,γ4,α3))") (interpret-abstract-atom "zip(zoom(quux,α1),kweh(γ4,γ2,α5))")))

(check-equal? (apply-substitution-to-full-evaluation
               (list (abstract-equality (a 2) (a 16))
                     (abstract-equality (a 1) (a 15))
                     (abstract-equality (g 2) (g 21))
                     (abstract-equality (g 1) (g 20)))
               (ak:full-evaluation
                (interpret-abstract-atom "del(α1,[γ1|γ2],α2)")
                (interpret-abstract-atom "del(γ3,[γ1|γ2],γ4)")))
              (ak:full-evaluation
                (interpret-abstract-atom "del(α15,[γ20|γ21],α16)")
                (interpret-abstract-atom "del(γ3,[γ20|γ21],γ4)")))
                                                     

;  describe "substitution for abstract constants in terms" $ do
;
;    it "can replace an 'any' value with an indexed 'g' value" $
;      (substitute_in_term (FuncTerm $ AConst (UG 1))
;                          (FuncTerm $ AConst (A 1))
;                          (ConjunctTerm $ CAtom $ Atom "mortal" [AConst (A 1)])) `shouldBe` (ConjunctTerm $ CAtom $ Atom  "mortal" [AConst (UG 1)])
;
;    it "can replace an indexed 'g' with another indexed 'g'" $
;      (substitute_in_term (FuncTerm (AConst (UG 3)))
;                          (FuncTerm (AConst (UG 1)))
;                          (ConjunctTerm $ CAtom $ Atom "divine" [AConst (UG 1), 
;                                                                 AConst (UG 2),
;                                                                 AConst (UG 1)])) `shouldBe` (ConjunctTerm $ CAtom $ Atom "divine" [AConst (UG 3), 
;                                                                                                                                    AConst (UG 2),
;                                                                                                                                    AConst (UG 3)])
;    it "does not replace a G with a different index from that specified" $
;      (substitute_in_term (FuncTerm $ AConst (UG 3))
;                          (FuncTerm $ AConst (UG 1))
;                          (FuncTerm $ AConst (UG 2))) `shouldBe` (FuncTerm $ AConst $ UG 2)
;
;  describe "substitution for abstract constants in substitutions" $ do
;
;    it "can replace 'any' values with 'g' values" $
;      (substitute_in_substitution (FuncTerm $ AConst (UG 1)) -- substituter
;                                  (FuncTerm $ AConst (A 1)) -- substitutee
;                                  [AEquality (FuncTerm $ ADomainFunc "testfunc" [AConst (A 1)])
;                                             (FuncTerm $ ADomainFunc "testfunc" [AConst (UG 2)])]) `shouldBe` [AEquality (FuncTerm $ ADomainFunc "testfunc" [AConst (UG 1)])
;                                                                                                                        (FuncTerm $ ADomainFunc "testfunc" [AConst (UG 2)])]
;
;  describe "restricting a substitution to a given term" $
;
;    let equality1 = AEquality (FuncTerm $ AConst $ UG 2) (FuncTerm $ AConst $ UG 6)
;        equality2 = AEquality (FuncTerm $ AConst $ UG 4) (FuncTerm $ AConst $ UG 7) -- variable from term on LHS
;        equality3 = AEquality (FuncTerm $ AConst $ UG 7) (FuncTerm $ AConst $ UG 8)
;        equality4 = AEquality (FuncTerm $ AConst $ UG 10) (FuncTerm $ AConst $ UG 4) -- variable from term on RHS
;        sub = [equality1, equality2, equality3, equality4]
;        in it "only retains pairs whose left-hand side is a variable in the supplied term" $
;           restrict sub (FuncTerm $ ADomainFunc "myfunc" [AConst (UG 4), AConst (UG 7)]) `shouldBe` [equality2, equality3]