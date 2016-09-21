#lang racket
(require parenlog)
(require (prefix-in abp: "abstract-domain-boilerplate.rkt"))
(require (only-in "../src/abstract-domain-ordering.rkt" >=-extension))

(define-model prior
  (priority (abp:parse-atom "integers(γ1,α1)") (abp:parse-atom "filter(γ1,α1,α2)"))
  (priority (abp:parse-atom "integers(γ1,α1)") (abp:parse-atom "integers(γ1,γ2)"))
  (:- (before X Y)
      (priority X Y))
  (:- (before X Y)
      (priority X Z)
      (before Z Y))
  (:- (inconsistency X Y)
      (before X Y)
      (,>=-extension X Y)))

(query-model prior (inconsistency X Y))