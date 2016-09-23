#lang racket
(require rackunit)
(require "../src/execution.rkt")
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require parenlog)

(define looping-graph (abp:parse-prior-relation "a,b a,c b,a b,d c,b c,e d,e"))
(query-model looping-graph (preprior-pair A B C))
(query-model looping-graph (before A B))
(define non-looping-graph (abp:parse-prior-relation "a,b a,c b,d c,b c,e d,e"))
(define permsort-graph (abp:parse-prior-relation "perm(γ1,α1),ord(α1) perm(γ1,α1),ord([γ1|α1]) ord([γ1,γ2|α1]),perm(γ1,α1)"))
(define hypothetical-graph-for-consistency (abp:parse-prior-relation "foo(γ1,α1),bar(γ1,α1)"))

(check-true (is-valid? non-looping-graph))
;(check-false (is-valid? looping-graph))
(check-true (is-valid? permsort-graph))
(check-true (is-valid? hypothetical-graph-for-consistency))

;(check-equal?
; (selected-index
;  (abp:parse-abstract-conjunction "bar(γ1,α1),foo(γ1,γ2)")
;  hypothetical-graph-for-consistency)
; 1)
;
;(check-equal?
; (selected-index
;  (abp:parse-abstract-conjunction "bar(α1,α2),foo(γ1,α2)")
;  hypothetical-graph-for-consistency)
; 1)
; ! Parenlog gedraagt zich niet goed met getallen,...
; moet ook conversie naar S-expressies controleren voor lijsten... krijg ik verwachte cons en ()?
; conversie naar S-expressies vereist misschien verdere aanpassingen.

