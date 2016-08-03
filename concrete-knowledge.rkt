#lang typed/racket
(require "concrete-domain.rkt")
(struct rule ([head : atom] [body : Conjunction]))
(provide (struct-out rule))