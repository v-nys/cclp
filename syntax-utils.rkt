#lang racket
; takes a syntax object, returns a list containing a syntax object for every other argument
(define-syntax (odd-elems-as-list stx)
  (syntax-case stx ()
    [(_ arg0) #'(list arg0)]
    [(_ arg0 arg1 arg2 ...) #'(cons arg0 (odd-elems-as-list arg2 ...))]))
(provide odd-elems-as-list)