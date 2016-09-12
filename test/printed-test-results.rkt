#lang racket
(require rackunit)

(provide readable-check-eq?)
(define-syntax-rule (readable-check-eq? actual-expr expected-expr)
  (with-check-info* (list (make-check-info 'actual-as-string (~v actual-expr))
                          (make-check-info 'expected-as-string (~v expected-expr)))
    (lambda () (check-eq? actual-expr expected-expr))))

(provide readable-check-equal?)
(define-syntax-rule (readable-check-equal? actual-expr expected-expr)
  (with-check-info* (list (make-check-info 'actual-as-string (~v actual-expr))
                          (make-check-info 'expected-as-string (~v expected-expr)))
    (lambda () (check-equal? actual-expr expected-expr))))

; there is a function tree-display, but it does not return a string