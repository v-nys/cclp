#lang racket
(require
  (for-syntax syntax/parse)
  (prefix-in ad: cclp-common-data/abstract-multi-domain))

(define-syntax (abstract-atom-with-args stx)
  (syntax-parse stx
    [(_ sym "(" tl ")")
     (syntax/loc stx
       (ad:abstract-atom (string->symbol (quote sym)) tl))]))
(provide abstract-atom-with-args)

(define-syntax (abstract-termlist stx)
  (syntax-parse stx
    [(_ t)
     (syntax/loc stx (list t))]
    [(_ t "," t-or-comma ...)
     (syntax/loc stx (cons t (abstract-termlist t-or-comma ...)))]))
(provide abstract-termlist)

(define-syntax (parameterized-abstract-termlist stx)
  (syntax-parse stx
    [(_ t)
     (syntax/loc stx (list t))]
    [(_ t "," t-or-comma ...)
     (syntax/loc stx (cons t (parameterized-abstract-termlist t-or-comma ...)))]))
(provide parameterized-abstract-termlist)

(define-syntax-rule (abstract-atom-without-args symbol)
  (ad:abstract-atom (string->symbol (quote symbol)) (list)))
(provide abstract-atom-without-args)

(define-syntax (abstract-atom stx)
  (syntax-parse stx [(_ args-or-nothing) (syntax/loc stx args-or-nothing)]))
(provide abstract-atom)

(define-syntax-rule (abstract-term specific-term) specific-term)
(provide abstract-term)

(define-syntax (abstract-variable stx)
  (syntax-parse stx
    [(_ "a" idx)
     (syntax/loc stx (ad:a (quote idx)))]
    [(_ "g" idx)
     (syntax/loc stx (ad:g (quote idx)))]))
(provide abstract-variable)

(define-syntax (abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "[]") '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 (ad:abstract-function (string->symbol "[]") '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 (abstract-lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest "]")
     (syntax/loc stx (ad:abstract-function (string->symbol "'[|]'") (list term0 rest)))]))
(provide abstract-lplist)

(define-syntax (abstract-conjunction stx)
  (syntax-parse stx
    [(_ conjunct) (syntax/loc stx (list conjunct))]
    [(_ conjunct "," conjunct-or-comma ...) (syntax/loc stx (cons conjunct (abstract-conjunction conjunct-or-comma ...)))]))
(provide abstract-conjunction)

(define-syntax (abstract-substitution stx)
  (syntax-parse stx
    [(_ "{" "}") (list)]
    [(_ "{" subst-pair "}") (syntax/loc stx (list subst-pair))]
    [(_ "{" subst-pair "," rest-arg ... "}") (syntax/loc stx (cons subst-pair (abstract-substitution "{" rest-arg ... "}")))]))
(provide abstract-substitution)

(define-syntax-rule (abstract-substitution-pair lhs "/" rhs)
  (as:abstract-equality lhs rhs))
(provide abstract-substitution-pair)

(define-syntax-rule (fullai-rule with-or-without-body) with-or-without-body)
(provide fullai-rule)

(define-syntax (abstract-function-term stx)
  (syntax-parse stx #:literals (abstract-number)
    [(_ (abstract-number n)) (syntax/loc stx (abstract-number n))]
    [(_ functor) (syntax/loc stx (ad:abstract-function (string->symbol functor) '()))]
    [(_ functor "(" tl ")") (syntax/loc stx (ad:abstract-function (string->symbol functor) tl))]))
(provide abstract-function-term)

(define-syntax-rule (fail _sym) (quote #f))
(provide fail)

(define-syntax (fullai-rule-without-body stx)
  (syntax-parse stx
    [(_ aawa num)
     (syntax/loc stx (ak:full-ai-rule aawa (quote #f) (string->number num)))]))
(provide fullai-rule-without-body)

(define-syntax (fullai-rule-with-body stx)
  (syntax-parse stx
    [(_ aawa "->" outcome num)
     (syntax/loc stx (ak:full-ai-rule aawa outcome (string->number num)))]))
(provide fullai-rule-with-body)

(define-syntax-rule (parameterized-abstract-number-term num)
  (abstract-function* (number->symbol num) empty))
(provide parameterized-abstract-number-term)

(define-syntax (symbolic-index stx)
  (syntax-parse stx
    [(_ "i") (syntax/loc stx 'i)]
    [(_ "i+1") (syntax/loc stx 'i+1)]
    [(_ "L") (syntax/loc stx 'L)]
    [(_ num) (syntax/loc stx (string->number num))]))
(provide symbolic-index)

(define-syntax-rule (abstract-conjunct ac) ac)
(provide abstract-conjunct)

(define-syntax (parameterized-abstract-variable stx)
  (syntax-parse stx
    [(_ "a" "<" midx "," sidx "," vidx ">")
     (syntax/loc stx (ad:a* (quote midx) sidx (quote vidx)))]
    [(_ "g" "<" midx "," sidx "," vidx ">")
     (syntax/loc stx (ad:g* (quote midx) sidx (quote vidx)))]))
(provide parameterized-abstract-variable)

(define-syntax (parameterized-abstract-lplist stx)
  (syntax-parse stx
    [(_ "[" "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "[]") '()))]
    [(_ "[" term0 "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "'[|]'") (list term0 (ad:abstract-function* (string->symbol "[]") '()))))]
    [(_ "[" term0 "," rest ... "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "'[|]'") (list term0 (parameterized-abstract-lplist "[" rest ... "]"))))]
    [(_ "[" term0 "|" rest "]")
     (syntax/loc stx (ad:abstract-function* (string->symbol "'[|]'") (list term0 rest)))]))
(provide parameterized-abstract-lplist)

(define-syntax (parameterized-abstract-function-term stx)
  (syntax-parse stx #:literals (parameterized-abstract-number-term)
    [(_ (parameterized-abstract-number-term n)) (syntax/loc stx (parameterized-abstract-number-term n))]
    [(_ functor) (syntax/loc stx (ad:abstract-function* (string->symbol functor) '()))]
    [(_ functor "(" tl ")") (syntax/loc stx (ad:abstract-function* (string->symbol functor) tl))]))
(provide parameterized-abstract-function-term)

(define-syntax (parameterized-abstract-atom stx)
  (syntax-parse stx
    [(_ sym "(" tl ")")
     (syntax/loc stx
       (ad:abstract-atom* (string->symbol (quote sym)) tl))]))
(provide abstract-atom)

(define-syntax (parameterized-abstract-conjunction stx)
  (syntax-parse stx
    [(_ conjunct) (syntax/loc stx (list conjunct))]
    [(_ conjunct "," conjunct-or-comma ...) (syntax/loc stx (cons conjunct (parameterized-abstract-conjunction conjunct-or-comma ...)))]))
(provide parameterized-abstract-conjunction)

(define-syntax-rule (parameterized-abstract-term pat) pat)
(provide parameterized-abstract-term)

(define-syntax (init stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx empty)]
    [(_ "{" pav "=" at "}") (syntax/loc stx (list (cons pav at)))]
    [(_ "{" pav "=" at "," rest ... "}") (syntax/loc stx (cons (cons pav at) (init "{" rest ... "}")))]))
(provide init)

(define-syntax (final stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx empty)]
    [(_ "{" pav "=" at "}") (syntax/loc stx (list (cons pav at)))]
    [(_ "{" pav "=" at "," rest ... "}") (syntax/loc stx (cons (cons pav at) (init "{" rest ... "}")))]))
(provide final)

(define-syntax (consecutive stx)
  (syntax-parse stx
    [(_ "{" "}") (syntax/loc stx empty)]
    [(_ "{" pav "=" pat "}") (syntax/loc stx (list (cons pav pat)))]
    [(_ "{" pav "=" pat "," rest ... "}") (syntax/loc stx (cons (cons pav pat) (init "{" rest ... "}")))]))
(provide consecutive)

(define-syntax (multi-abstraction stx)
  (syntax-parse stx
    [(_ "multi" "(" pac "t" i c f num ")")
     (syntax/loc stx (ad:multi pac #t i c f num))]
    [(_ "multi" "(" pac "f" i c f num ")")
     (syntax/loc stx (ad:multi pac #f i c f num))]))
(provide multi-abstraction)