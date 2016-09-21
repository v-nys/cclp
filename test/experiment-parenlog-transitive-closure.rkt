#lang parenlog
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require (only-in "../src/abstract-domain-ordering.rkt" >=-extension))

(priority
 (abp:parse-atom "integers(γ1,α1)")
 (abp:parse-atom "filter(γ1,α1,α2)"))

; can I use Parenlog to check for consistency of the computation rule?
; let's try adding an inconsistent rule...
(priority
 (abp:parse-atom "integers(γ1,α1)")
 (abp:parse-atom "integers(γ1,γ2)"))

(:- (before X Y)
    (priority X Y))
(:- (before X Y)
    (priority X Z)
    (before Z Y))

(:- (inconsistency X Y)
    (before X Y)
    ; zal contract violation geven als ik er een toevoeg? X en Y zijn nog geen structs!
    ((,>=-extension) X Y))
(? (inconsistency X Y))