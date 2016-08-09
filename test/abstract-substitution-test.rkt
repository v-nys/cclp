#lang racket
(require rackunit)
(require "domain-boilerplate.rkt")
(require "../src/data-utils.rkt")
(require "../src/abstract-multi-domain.rkt")
(require "../src/abstract-substitution.rkt")

(check-equal? (maximum-var-index (parse-term "γ1") g?) (some 1))
(check-equal? (maximum-var-index (parse-term "γ1") a?) (none))
(check-equal? (maximum-var-index (parse-term "α2") a?) (some 2))
(check-equal? (maximum-var-index (parse-term "α2") g?) (none))

(check-equal? (maximum-var-index (parse-term "foo(γ1,α2)") g?) (some 1))
(check-equal? (maximum-var-index (parse-term "foo(γ1,α2)") a?) (some 2))
(check-equal? (maximum-var-index (parse-term "foo(γ1,γ2)") a?) (none))
(check-equal? (maximum-var-index (parse-term "foo(α1,α2)") g?) (none))

(check-equal? (maximum-var-index (parse-atom "foo(γ1,α2)") g?) (some 1))
(check-equal? (maximum-var-index (parse-atom "foo(γ1,α2)") a?) (some 2))
(check-equal? (maximum-var-index (parse-atom "foo(γ1,γ2)") a?) (none))
(check-equal? (maximum-var-index (parse-atom "foo(α1,α2)") g?) (none))

(check-equal? (substitute-in-substitution (parse-term "γ5") (parse-term "α1") (list (abstract-equality (parse-term "α4") (parse-term "foo(bar(α3,α1,α2))"))))
              (list (abstract-equality (parse-term "α4") (parse-term "foo(bar(α3,γ5,α2))"))))
(check-equal? (substitute-in-substitution (parse-term "γ5") (parse-term "α4") (list (abstract-equality (parse-term "α4") (parse-term "foo(bar(α3,α1,α2))"))))
              (list (abstract-equality (parse-term "γ5") (parse-term "foo(bar(α3,α1,α2))"))))

; TODO test for conjunctions

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