#lang br/quicklang
(require (for-syntax syntax/parse))
(require (only-in sugar/coerce ->symbol))

(require (prefix-in ad: "abstract-multi-domain.rkt"))
(require (prefix-in ak: "abstract-knowledge.rkt"))
(require (prefix-in as: "abstract-substitution.rkt"))
(require (prefix-in cd: "concrete-domain.rkt"))
(require (prefix-in ck: "concrete-knowledge.rkt"))
(require (prefix-in faid: "fullai-domain.rkt"))
(require (prefix-in aa: "abstract-analysis.rkt"))
(require racket-tree-utils/src/tree)
(require (for-syntax syntax/strip-context))
(require "data-utils.rkt")
(require (only-in "gen-graph-structs.rkt" index-range))

(define-syntax (at-module-begin stx)
  (syntax-parse stx
    [(_ _PARSE-TREE)
     (with-syntax ([REPLACED (replace-context stx #'val)])
       (syntax/loc stx
         (#%module-begin
          (define REPLACED _PARSE-TREE)
          (provide REPLACED))))]))
(provide (rename-out [at-module-begin #%module-begin]) #%top-interaction)

(define-syntax-rule (at content subtree ...)
  (node
   content
   (list subtree ...)))
(module+ test
  (require rackunit)
  (check-equal?
   (at (cyclenode 1) (at (cyclenode 2)) (at (cyclenode 3)))
   (node (aa:cycle 1)
         (list
          (node (aa:cycle 2) (list))
          (node (aa:cycle 3) (list)))))) ; nonsense tree but enough for test
(provide at)

(define-syntax-rule (cyclenode num) (aa:cycle num))
(provide cyclenode)

(define-macro-cases treelabel
  [(treelabel (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...))
   (syntax/loc
       caller-stx
     (aa:tree-label (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...) (none) (list) #f #f (list)))]
  [(treelabel NUMBER (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...))
   (syntax/loc
       caller-stx
     (aa:tree-label (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...) (none) (list) #f NUMBER (list)))]
  [(treelabel (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE))
   (syntax/loc caller-stx
     (aa:tree-label (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...) (none) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE) #f (list)))]
  [(treelabel NUMBER (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE))
   (syntax/loc caller-stx
     (aa:tree-label (selectionless-abstract-conjunction ABSTRACT-CONJUNCT ...) (none) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE) NUMBER (list)))]
  [(treelabel NUMBER (abstract-conjunction-selection SEL) REST ...)
   (syntax/loc caller-stx
     (treelabel NUMBER (abstract-conjunction-selection (selectionless-abstract-conjunction) SEL (selectionless-abstract-conjunction)) REST ...))]
  [(treelabel NUMBER (abstract-conjunction-selection SEL (selectionless-abstract-conjunction UNSEL2 ...)) REST ...)
   (syntax/loc caller-stx
     (treelabel NUMBER (abstract-conjunction-selection (selectionless-abstract-conjunction) SEL (selectionless-abstract-conjunction UNSEL2 ...)) REST ...))]
  [(treelabel NUMBER (abstract-conjunction-selection (selectionless-abstract-conjunction UNSEL1 ...) SEL) REST ...)
   (syntax/loc caller-stx
     (treelabel NUMBER (abstract-conjunction-selection (selectionless-abstract-conjunction UNSEL1 ...) SEL (selectionless-abstract-conjunction)) REST ...))]
  [(treelabel NUMBER (abstract-conjunction-selection UNSEL1 SEL UNSEL2))
   (syntax/loc caller-stx
     (aa:tree-label (car (abstract-conjunction-selection UNSEL1 SEL UNSEL2)) (some (cdr (abstract-conjunction-selection UNSEL1 SEL UNSEL2))) (list) #f NUMBER (list)))]
  [(treelabel NUMBER (abstract-conjunction-selection UNSEL1 SEL UNSEL2) (precedence-list PRECEDENCE ...))
   (syntax/loc caller-stx
     (aa:tree-label (car (abstract-conjunction-selection UNSEL1 SEL UNSEL2)) (some (cdr (abstract-conjunction-selection UNSEL1 SEL UNSEL2))) (list) #f NUMBER (precedence-list PRECEDENCE ...)))]
  [(treelabel NUMBER (abstract-conjunction-selection UNSEL1 SEL UNSEL2) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE))
   (syntax/loc caller-stx
     (aa:tree-label (car (abstract-conjunction-selection UNSEL1 SEL UNSEL2)) (some (cdr (abstract-conjunction-selection UNSEL1 SEL UNSEL2))) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE) NUMBER (list)))]
  [(treelabel NUMBER (abstract-conjunction-selection UNSEL1 SEL UNSEL2) (precedence-list PRECEDENCE ...) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE))
   (syntax/loc caller-stx
     (aa:tree-label (car (abstract-conjunction-selection UNSEL1 SEL UNSEL2)) (some (cdr (abstract-conjunction-selection UNSEL1 SEL UNSEL2))) (abstract-substitution PAIR ...) (knowledge KNOWLEDGE) NUMBER (precedence-list PRECEDENCE ...)))])
(module+ test
  (check-equal?
   (treelabel (selectionless-abstract-conjunction (abstract-atom "foo") (abstract-atom "bar")))
   (aa:tree-label (list (ad:abstract-atom 'foo (list)) (ad:abstract-atom 'bar (list))) (none) (list) #f #f (list)))
  (check-equal?
   (treelabel (selectionless-abstract-conjunction (abstract-atom "foo")) (abstract-substitution) (knowledge (fact (atom "bar"))))
   (aa:tree-label (list (ad:abstract-atom 'foo (list))) (none) (list) (ck:rule (cd:atom 'bar (list)) (list)) #f (list)))
  (check-equal?
   (treelabel 1 (abstract-conjunction-selection (selectionless-abstract-conjunction) (abstract-atom "foo") (selectionless-abstract-conjunction)))
   (aa:tree-label (list (ad:abstract-atom 'foo (list))) (some 0) (list) #f 1 (list)))
  (check-equal?
   (treelabel 1 (abstract-conjunction-selection (selectionless-abstract-conjunction) (abstract-atom "foo") (selectionless-abstract-conjunction)) (precedence-list))
   (aa:tree-label (list (ad:abstract-atom 'foo (list))) (some 0) (list) #f 1 (list)))
  (check-equal?
   (treelabel 1 (abstract-conjunction-selection (selectionless-abstract-conjunction) (abstract-atom "foo") (selectionless-abstract-conjunction)) (abstract-substitution) (knowledge (fact (atom "bar"))))
   (aa:tree-label (list (ad:abstract-atom 'foo (list))) (some 0) (list) (ck:rule (cd:atom 'bar (list)) (list)) 1 (list)))
  (check-equal?
   (treelabel 1 (abstract-conjunction-selection (selectionless-abstract-conjunction) (abstract-atom "foo") (selectionless-abstract-conjunction)) (precedence-list) (abstract-substitution) (knowledge (fact (atom "bar"))))
   (aa:tree-label (list (ad:abstract-atom 'foo (list))) (some 0) (list) (ck:rule (cd:atom 'bar (list)) (list)) 1 (list))))
(provide treelabel)

(define-syntax-rule (selectionless-abstract-conjunction conjunct ...) (list conjunct ...))
(provide selectionless-abstract-conjunction)

(define-syntax-rule (abstract-atom symbol args ...) (ad:abstract-atom (->symbol symbol) (list args ...)))
(module+ test
  (check-equal?
   (abstract-atom "foo")
   (ad:abstract-atom 'foo (list)))
  (check-equal?
   (abstract-atom "foo" (abstract-g-variable 1) (abstract-a-variable 1))
   (ad:abstract-atom 'foo (list (ad:g 1) (ad:a 1)))))
(provide abstract-atom)

(define-syntax-rule (abstract-function symbol args ...) (ad:abstract-function (->symbol symbol) (list args ...)))
(module+ test
  (check-equal?
   (abstract-function "foo")
   (ad:abstract-function 'foo (list)))
  (check-equal?
   (abstract-function "foo" (abstract-g-variable 1) (abstract-a-variable 1))
   (ad:abstract-function 'foo (list (ad:g 1) (ad:a 1)))))
(provide abstract-function)

(define-syntax-rule (abstract-g-variable num) (ad:g num))
(provide abstract-g-variable)

(define-syntax-rule (abstract-a-variable num) (ad:a num))
(provide abstract-a-variable)

(define-syntax (abstract-list stx)
  (syntax-parse stx
    [(_) (syntax/loc stx (ad:abstract-function 'nil '()))]
    [(_ term0)
     (syntax/loc stx (ad:abstract-function 'cons (list term0 (ad:abstract-function 'nil '()))))]
    [(_ term0 "," rest ...)
     (syntax/loc stx (ad:abstract-function 'cons (list term0 (abstract-list rest ...))))]
    [(_ term0 "|" rest)
     (syntax/loc stx (ad:abstract-function 'cons (list term0 rest)))]))
(module+ test
  (check-equal?
   (abstract-list)
   (ad:abstract-function 'nil '()))
  (check-equal?
   (abstract-list (abstract-g-variable 1))
   (ad:abstract-function 'cons (list (ad:g 1) (ad:abstract-function 'nil '()))))
  (check-equal?
   (abstract-list (abstract-g-variable 1) "," (abstract-g-variable 1))
   (ad:abstract-function 'cons (list (ad:g 1) (ad:abstract-function 'cons (list (ad:g 1) (ad:abstract-function 'nil '()))))))
  (check-equal?
   (abstract-list (abstract-g-variable 1) "|" (abstract-a-variable 1))
   (ad:abstract-function 'cons (list (ad:g 1) (ad:a 1)))))
(provide abstract-list)

(define-syntax-rule (multi-abstraction parameterized-conjunction ascending? init consecutive final)
  (ad:multi parameterized-conjunction ascending? init consecutive final))
(provide multi-abstraction)

(define-syntax-rule (parameterized-abstract-conjunction parameterized-atom ...) (list parameterized-atom ...))
(provide parameterized-abstract-conjunction)

(define-syntax parameterized-abstract-atom
  (syntax-rules ()
    [(_ symbol) (ad:abstract-atom* (->symbol symbol) (list))]
    [(_ symbol arg ...) (ad:abstract-atom* (->symbol symbol) (list arg ...))]))
(module+ test
  (check-equal?
   (parameterized-abstract-atom "foo")
   (ad:abstract-atom* 'foo (list)))
  (check-equal?
   (parameterized-abstract-atom "foo" (parameterized-abstract-g-variable 1 1 1) (parameterized-abstract-a-variable 1 1 1))
   (ad:abstract-atom* 'foo (list (ad:g* 1 1 1) (ad:a* 1 1 1)))))
(provide parameterized-abstract-atom)

(define-syntax parameterized-abstract-function
  (syntax-rules ()
    [(_ symbol) (ad:abstract-function* (->symbol symbol) (list))]
    [(_ symbol arg ...) (ad:abstract-function* (->symbol symbol) (list arg ...))]))
(module+ test
  (check-equal?
   (parameterized-abstract-function "foo")
   (ad:abstract-function* 'foo (list)))
  (check-equal?
   (parameterized-abstract-function "foo" (parameterized-abstract-g-variable 1 1 1) (parameterized-abstract-a-variable 1 1 1))
   (ad:abstract-function* 'foo (list (ad:g* 1 1 1) (ad:a* 1 1 1)))))
(provide parameterized-abstract-function)

(define-syntax (parameterized-abstract-a-variable stx)
  (syntax-parse stx
    [(_ idx1 idx2:str idx3) (syntax/loc stx (ad:a* idx1 (string->symbol idx2) idx3))]
    [(_ idx1 idx2 idx3) (syntax/loc stx (ad:a* idx1 idx2 idx3))]))
(module+ test
  (check-equal?
   (parameterized-abstract-a-variable 1 1 1)
   (ad:a* 1 1 1))
  (check-equal?
   (parameterized-abstract-a-variable 1 "i" 1)
   (ad:a* 1 'i 1))
  (check-equal?
   (parameterized-abstract-a-variable 1 "i+1" 1)
   (ad:a* 1 'i+1 1))
  (check-equal?
   (parameterized-abstract-a-variable 1 "L" 1)
   (ad:a* 1 'L 1)))
(provide parameterized-abstract-a-variable)

(define-syntax (parameterized-abstract-g-variable stx)
  (syntax-parse stx
    [(_ idx1 idx2:str idx3) (syntax/loc stx (ad:g* idx1 (string->symbol idx2) idx3))]
    [(_ idx1 idx2 idx3) (syntax/loc stx (ad:g* idx1 idx2 idx3))]))
(module+ test
  (check-equal?
   (parameterized-abstract-g-variable 1 1 1)
   (ad:g* 1 1 1))
  (check-equal?
   (parameterized-abstract-g-variable 1 "i" 1)
   (ad:g* 1 'i 1))
  (check-equal?
   (parameterized-abstract-g-variable 1 "i+1" 1)
   (ad:g* 1 'i+1 1))
  (check-equal?
   (parameterized-abstract-g-variable 1 "L" 1)
   (ad:g* 1 'L 1)))
(provide parameterized-abstract-g-variable)

(define-syntax (parameterized-abstract-list stx)
  (syntax-parse stx
    [(_) (syntax/loc stx (ad:abstract-function* 'nil '()))]
    [(_ term0)
     (syntax/loc stx (ad:abstract-function* 'cons (list term0 (ad:abstract-function* 'nil '()))))]
    [(_ term0 "," rest ...)
     (syntax/loc stx (ad:abstract-function* 'cons (list term0 (parameterized-abstract-list rest ...))))]
    [(_ term0 "|" rest)
     (syntax/loc stx (ad:abstract-function* 'cons (list term0 rest)))]))
(module+ test
  (check-equal?
   (parameterized-abstract-list)
   (ad:abstract-function* 'nil '()))
  (check-equal?
   (parameterized-abstract-list (parameterized-abstract-g-variable 1 1 1))
   (ad:abstract-function* 'cons (list (ad:g* 1 1 1) (ad:abstract-function* 'nil '()))))
  (check-equal?
   (parameterized-abstract-list (parameterized-abstract-g-variable 1 1 1) "," (parameterized-abstract-g-variable 1 1 1))
   (ad:abstract-function* 'cons (list (ad:g* 1 1 1) (ad:abstract-function* 'cons (list (ad:g* 1 1 1) (ad:abstract-function* 'nil '()))))))
  (check-equal?
   (parameterized-abstract-list (parameterized-abstract-g-variable 1 1 1) "|" (parameterized-abstract-a-variable 1 1 1))
   (ad:abstract-function* 'cons (list (ad:g* 1 1 1) (ad:a* 1 1 1)))))
(provide parameterized-abstract-list)

(define-syntax-rule (init pair ...) (ad:init (list pair ...)))
(provide init)

(define-syntax-rule (init-pair avar* aterm) (cons avar* aterm))
(provide init-pair)

(define-syntax-rule (consecutive pair ...) (ad:consecutive (list pair ...)))
(provide consecutive)

(define-syntax-rule (consecutive-pair avar* aterm*) (cons avar* aterm*))
(provide consecutive-pair)

(define-syntax-rule (final pair ...) (ad:final (list pair ...)))
(provide final)

(define-syntax-rule (final-pair avar* avar) (cons avar* avar))
(provide final-pair)

(define-syntax-rule (abstract-conjunction-selection unsel1 sel unsel2)
  (cons (append unsel1 (list sel) unsel2) (length unsel1)))
(provide abstract-conjunction-selection)

(define-syntax-rule (precedence-list precedence ...) (list precedence ...))
(provide precedence-list)

(define-syntax-rule (precedence at1 at2) (cons at1 at2))
(provide precedence)

(define-syntax-rule (abstract-substitution pair ...) (list pair ...))
(provide abstract-substitution)

(define-syntax-rule (abstract-substitution-pair av at) (as:abstract-equality av at))
(provide abstract-substitution-pair)

(define-syntax-rule (knowledge k) k)
(provide knowledge)

(define-syntax-rule (fact a) (ck:rule a (list)))
(provide fact)

(define-syntax-rule (clause a c) (ck:rule a c))
(provide clause)

(define-syntax-rule (atom symbol args ...) (cd:atom (->symbol symbol) (list args ...)))
(provide atom)

(define-syntax-rule (conjunction a ...) (list a ...))
(provide conjunction)

(define-syntax-rule (variable id) (cd:variable (string->symbol id)))
(provide variable)

(define-syntax (function stx)
  (syntax-parse stx
    [(_ SYM:str ARG ...)
     (syntax/loc stx (cd:function (->symbol SYM) (list ARG ...)))]
    [(_ NUM:number)
     (syntax/loc stx (cd:function (->symbol NUM) (list)))]))
(module+ test
  (check-equal?
   (function "foo")
   (cd:function 'foo (list)))
  (check-equal?
   (function "foo" (function "bar"))
   (cd:function 'foo (list (cd:function 'bar (list)))))
  (check-equal?
   (function 3)
   (cd:function (->symbol 3) (list))))
(provide function)

(define-syntax (lplist stx)
  (syntax-parse stx
    [(_) (syntax/loc stx (cd:function 'nil '()))]
    [(_ term0)
     (syntax/loc stx (cd:function 'cons (list term0 (cd:function 'nil '()))))]
    [(_ term0 "," rest ...)
     (syntax/loc stx (cd:function 'cons (list term0 (lplist rest ...))))]
    [(_ term0 "|" rest)
     (syntax/loc stx (cd:function 'cons (list term0 rest)))]))
(module+ test
  (check-equal?
   (lplist)
   (cd:function 'nil '()))
  (check-equal?
   (lplist (variable "X"))
   (cd:function 'cons (list (cd:variable 'X) (cd:function 'nil '()))))
  (check-equal?
   (lplist (variable "X") "," (variable "X"))
   (cd:function 'cons (list (cd:variable 'X) (cd:function 'cons (list (cd:variable 'X) (cd:function 'nil '()))))))
  (check-equal?
   (lplist (variable "X") "|" (variable "Y"))
   (cd:function 'cons (list (cd:variable 'X) (cd:variable 'Y)))))
(provide lplist)

(define-syntax-rule (fullai-rule aa as) (ak:full-evaluation aa (as:apply-substitution as aa) 1))
(provide fullai-rule)

(define-macro-cases generalization
  [(_ (selectionless-abstract-conjunction ACON ...) INDEX-RANGES)
   (syntax/loc caller-stx (aa:generalization (selectionless-abstract-conjunction ACON ...) (none) #f (list) INDEX-RANGES))]
  [(_ NUM (abstract-conjunction-selection UNSEL1 SEL UNSEL2) INDEX-RANGES)
   (syntax/loc caller-stx (generalization NUM (abstract-conjunction-selection UNSEL1 SEL UNSEL2) (precedence-list) INDEX-RANGES))]
  [(_ NUM (abstract-conjunction-selection UNSEL1 SEL UNSEL2) (precedence-list PRECEDENCE ...) INDEX-RANGES)
   (syntax/loc caller-stx
     (aa:generalization
      (car (abstract-conjunction-selection UNSEL1 SEL UNSEL2))
      (some (cdr (abstract-conjunction-selection UNSEL1 SEL UNSEL2)))
      NUM
      (precedence-list PRECEDENCE ...)
      INDEX-RANGES))])
(provide generalization)

(define-macro (generalized-index-ranges RANGE ...) (syntax/loc caller-stx (list RANGE ...)))
(provide generalized-index-ranges)

(define-macro (generalized-index-range NUM1 NUM2) (syntax/loc caller-stx (index-range NUM1 (+ NUM2 1))))
(provide generalized-index-range)