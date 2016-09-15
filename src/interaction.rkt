#lang racket

(require "io-utils.rkt")
(require (only-in "data-utils.rkt" 3-tuple? 3-tuple-first 3-tupleof))
;(require (only-in "abstract-knowledge.rkt" abstract-knowledge? rule? full-evaluation))
(require "abstract-knowledge.rkt")
(require (only-in "fullai-domain.rkt" full-ai-rule?))
(require (only-in "execution.rkt" priority?))

(define (cclp-run filename program-data)
  ;(print (path-replace-extension (last (explode-path filename)) ".serializedcclp"))
  (print (ormap rule? (3-tuple-first program-data)))
  (print (3-tuple-first program-data))
  (print program-data))
(provide (contract-out [cclp-run (-> path? (3-tupleof list? (listof full-ai-rule?) (listof priority?)) void?)]))
;(provide (contract-out [cclp-run (-> path? (3-tupleof (listof rule?) (listof full-ai-rule?) (listof priority?)) void?)]))
;(provide (contract-out [cclp-run (-> path? (3-tupleof (listof abstract-knowledge?) (listof full-ai-rule?) (listof priority?)) void?)]))