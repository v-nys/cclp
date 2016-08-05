#lang typed/racket

(struct none () #:transparent)
(provide (struct-out none))
(struct (a) some ([v : a]) #:transparent)
(provide (struct-out some))
(define-type (Opt a) (U none (some a)))
(provide Opt)