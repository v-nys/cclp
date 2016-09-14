#lang racket

(require "io-utils.rkt")

; TODO modify this so that user is presented with options
(define (cclp-run filename program-data)
  (print (path-replace-extension (last (explode-path filename)) ".serializedcclp"))
  (print program-data))
(provide cclp-run)