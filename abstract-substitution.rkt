#lang typed/racket
(require "abstract-multi-domain.rkt")

(struct abstract-equality ([term1 : AbstractDomainElem] [term2 : AbstractDomainElem]) #:transparent)
(provide (struct-out abstract-equality))
(define-type AbstractSubstitution (Listof abstract-equality))
(provide AbstractSubstitution)