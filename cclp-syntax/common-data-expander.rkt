#lang racket
(require
  (for-syntax syntax/parse)
  (prefix-in ad: cclp-common-data/abstract-multi-domain)
  (prefix-in ak: cclp-common-data/abstract-knowledge)
  (prefix-in as: cclp-common-data/abstract-substitution)
  (prefix-in cd: cclp-common-data/concrete-domain)
  (prefix-in ck: cclp-common-data/concrete-knowledge))

(define-syntax (ag-abstract-atom-with-args stx)
  (syntax-parse stx
    [(_ sym "(" tl ")")
     (syntax/loc stx
       (ad:abstract-atom (string->symbol (quote sym)) tl))]))
(provide ag-abstract-atom-with-args)

(define-syntax (ag-abstract-termlist stx)
  (syntax-parse stx
    [(_ t)
     (syntax/loc stx (list t))]
    [(_ t "," t-or-comma ...)
     (syntax/loc stx (cons t (ag-abstract-termlist t-or-comma ...)))]))
(provide ag-abstract-termlist)

(define-syntax (ag-parameterized-abstract-termlist stx)
  (syntax-parse stx
    [(_ t)
     (syntax/loc stx (list t))]
    [(_ t "," t-or-comma ...)
     (syntax/loc stx (cons t (ag-parameterized-abstract-termlist t-or-comma ...)))]))
(provide ag-parameterized-abstract-termlist)

(define-syntax-rule (ag-abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide ag-abstract-atom-without-args)

(define-syntax (ag-abstract-atom stx)
  (syntax-parse stx [(_ args-or-nothing) (syntax/loc stx args-or-nothing)]))
(provide ag-abstract-atom)

(define-syntax-rule (ag-abstract-term specific-term) specific-term)
(provide ag-abstract-term)

(define-syntax (ag-abstract-variable stx)
  (syntax-parse stx
    [(_ "a" idx)
     (syntax/loc stx (ad:a (quote idx)))]
    [(_ "g" idx)
     (syntax/loc stx (ad:g (quote idx)))]))
(provide ag-abstract-variable)

(define-syntax (ag-abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "[]") '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 (ad:abstract-function (string->symbol "[]") '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 (ag-abstract-lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 rest)))]))
(provide ag-abstract-lplist)

(define-syntax (ag-abstract-conjunction stx)
  (syntax-parse stx
    [(_ conjunct) (syntax/loc stx (list conjunct))]
    [(_ conjunct "," conjunct-or-comma ...) (syntax/loc stx (cons conjunct (ag-abstract-conjunction conjunct-or-comma ...)))]))
(provide ag-abstract-conjunction)

(define-syntax (ag-abstract-substitution stx)
  (syntax-parse stx
    [(_ "{" "}") (list)]
    [(_ "{" subst-pair "}") (syntax/loc stx (list subst-pair))]
    [(_ "{" subst-pair "," rest-arg ... "}") (syntax/loc stx (cons subst-pair (ag-abstract-substitution "{" rest-arg ... "}")))]))
(provide ag-abstract-substitution)

(define-syntax-rule (ag-abstract-substitution-pair lhs "/" rhs)
  (as:abstract-equality lhs rhs))
(provide ag-abstract-substitution-pair)

(define-syntax-rule (ag-fullai-rule with-or-without-body) with-or-without-body)
(provide ag-fullai-rule)

(define-syntax-rule (ag-abstract-number n) (ad:abstract-function (string->symbol (number->string n)) empty))
(provide ag-abstract-number)

(define-syntax (ag-abstract-function-term stx)
  (syntax-parse stx #:literals (ag-abstract-number)
    [(_ (ag-abstract-number n)) (syntax/loc stx (ag-abstract-number n))]
    [(_ functor) (syntax/loc stx (ad:abstract-function (string->symbol functor) '()))]
    [(_ functor "(" tl ")") (syntax/loc stx (ad:abstract-function (string->symbol functor) tl))]))
(provide ag-abstract-function-term)

(define-syntax-rule (ag-fail _sym) (quote #f))
(provide ag-fail)

(define-syntax (ag-fullai-rule-without-body stx)
  (syntax-parse stx
    [(_ aawa num)
     (syntax/loc stx (ak:full-ai-rule aawa (quote #f) num))]))
(provide ag-fullai-rule-without-body)

(define-syntax (ag-fullai-rule-with-body stx)
  (syntax-parse stx
    [(_ aawa "->" outcome num)
     (syntax/loc stx (ak:full-ai-rule aawa outcome num))]))
(provide ag-fullai-rule-with-body)

(define-syntax-rule (ag-parameterized-abstract-number-term num)
  (abstract-function* (number->symbol num) empty))
(provide ag-parameterized-abstract-number-term)

(define-syntax (ag-symbolic-index stx)
  (syntax-parse stx
    [(_ "i") (syntax/loc stx 'i)]
    [(_ "i+1") (syntax/loc stx 'i+1)]
    [(_ "l") (syntax/loc stx 'L)]
    [(_ num) (syntax/loc stx num)]))
(provide ag-symbolic-index)

(define-syntax-rule (ag-abstract-conjunct ac) ac)
(provide ag-abstract-conjunct)

(define-syntax (ag-parameterized-abstract-variable stx)
  (syntax-parse stx
    [(_ "a" "<" midx "," sidx "," vidx ">")
     (syntax/loc stx (ad:a* (quote midx) sidx (quote vidx)))]
    [(_ "g" "<" midx "," sidx "," vidx ">")
     (syntax/loc stx (ad:g* (quote midx) sidx (quote vidx)))]))
(provide ag-parameterized-abstract-variable)

(define-syntax (ag-parameterized-abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "[]") '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "'[|]'") (list term0 (ad:abstract-function* (string->symbol "[]") '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "'[|]'") (list term0 (ag-parameterized-abstract-lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "'[|]'") (list term0 rest)))]))
(provide ag-parameterized-abstract-lplist)

(define-syntax (ag-parameterized-abstract-function-term stx)
  (syntax-parse stx #:literals (ag-parameterized-abstract-number-term)
    [(_ (ag-parameterized-abstract-number-term n)) (syntax/loc stx (ag-parameterized-abstract-number-term n))]
    [(_ functor) (syntax/loc stx (ad:abstract-function* (string->symbol functor) '()))]
    [(_ functor "(" tl ")") (syntax/loc stx (ad:abstract-function* (string->symbol functor) tl))]))
(provide ag-parameterized-abstract-function-term)

(define-syntax (ag-parameterized-abstract-atom stx)
  (syntax-parse stx
    [(_ sym "(" tl ")")
     (syntax/loc stx
       (ad:abstract-atom* (string->symbol (quote sym)) tl))]))
(provide ag-parameterized-abstract-atom)

(define-syntax (ag-parameterized-abstract-conjunction stx)
  (syntax-parse stx
    [(_ conjunct) (syntax/loc stx (list conjunct))]
    [(_ conjunct "," conjunct-or-comma ...) (syntax/loc stx (cons conjunct (ag-parameterized-abstract-conjunction conjunct-or-comma ...)))]))
(provide ag-parameterized-abstract-conjunction)

(define-syntax-rule (ag-parameterized-abstract-term pat) pat)
(provide ag-parameterized-abstract-term)

(define-syntax (ag-init stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx empty)]
    [(_ "{" pav "=" at "}") (syntax/loc stx (list (cons pav at)))]
    [(_ "{" pav "=" at "," rest ... "}") (syntax/loc stx (cons (cons pav at) (ag-init "{" rest ... "}")))]))
(provide ag-init)

(define-syntax (ag-final stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx empty)]
    [(_ "{" pav "=" at "}") (syntax/loc stx (list (cons pav at)))]
    [(_ "{" pav "=" at "," rest ... "}") (syntax/loc stx (cons (cons pav at) (ag-final "{" rest ... "}")))]))
(provide ag-final)

(define-syntax (ag-consecutive stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx empty)]
    [(_ "{" pav "=" pat "}") (syntax/loc stx (list (cons pav pat)))]
    [(_ "{" pav "=" pat "," rest ... "}") (syntax/loc stx (cons (cons pav pat) (ag-consecutive "{" rest ... "}")))]))
(provide ag-consecutive)

(define-syntax (ag-multi-abstraction stx)
  (syntax-parse stx
    [(_ "multi" "(" pac "," "t" "," i "," c "," f "," num ")")
     (syntax/loc stx (ad:multi pac #t i c f num))]
    [(_ "multi" "(" pac "," "f" "," i "," c "," f "," num ")")
     (syntax/loc stx (ad:multi pac #f i c f num))]))
(provide ag-multi-abstraction)

(define-syntax-rule (ag-concrete-multi tl) (cd:concrete-multi tl))
(provide ag-concrete-multi)

(define-syntax (ag-atom stx)
  (syntax-parse stx
    [(_ sym) (syntax/loc stx (cd:atom (string->symbol sym) empty))]
    [(_ sym "(" tl ")") (syntax/loc stx (cd:atom (string->symbol sym) tl))]))
(provide ag-atom)

(define-syntax (ag-rule stx)
  (syntax-parse stx
    [(_ at ":-" con num) (syntax/loc stx (ck:rule at con num))]
    [(_ at num) (syntax/loc stx (ck:rule at empty num))]))
(provide ag-rule)

(define-syntax-rule (ag-variable sym) (cd:variable (string->symbol sym)))
(provide ag-variable)

(define-syntax-rule (ag-number-term num) (cd:function (string->symbol (number->string num)) empty))

(define-syntax (ag-function-term stx)
  (syntax-parse stx #:literals (ag-number-term)
    [(_ (ag-number-term n)) (syntax/loc stx (number-term n))]
    [(_ functor) (syntax/loc stx (cd:function (string->symbol functor) '()))]
    [(_ functor "(" tl ")") (syntax/loc stx (cd:function (string->symbol functor) tl))]))
(provide ag-function-term)

(define-syntax-rule (ag-conjunct c) c)
(provide ag-conjunct)

(define-syntax (ag-conjunction stx)
  (syntax-parse stx
    [(_ c)
     (syntax/loc stx (list c))]
    [(_ c "," c-or-comma ...)
     (syntax/loc stx (cons c (ag-termlist c-or-comma ...)))]))
(provide ag-conjunction)

(define-syntax (ag-termlist stx)
  (syntax-parse stx
    [(_ t)
     (syntax/loc stx (list t))]
    [(_ t "," t-or-comma ...)
     (syntax/loc stx (cons t (ag-termlist t-or-comma ...)))]))
(provide ag-termlist)

(define-syntax (ag-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (cd:function (string->symbol "[]") '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (cd:function (string->symbol "'[|]'") (list term0 (cd:function (string->symbol "[]") '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (cd:function (string->symbol "'[|]'") (list term0 (ag-lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest "]")
     (syntax/loc stx (cd:function (string->symbol "'[|]'") (list term0 rest)))]))
(provide ag-lplist)

(define-syntax-rule (ag-term specific-term) specific-term)
(provide ag-term)