#lang at-exp racket
(require
  "concrete-domain.rkt"
  (only-in
   "domain-switching.rkt"
   concrete-multi
   concrete-multi?)
  (only-in racket-list-utils/utils frequencies))
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (extract-all-concrete-variables/duplicates c)
  (match c
    [(? list?)
     (append-map extract-all-concrete-variables/duplicates c)]
    [(? variable?) (list c)]
    [(or
      (function sym args)
      (atom sym args))
     (extract-all-concrete-variables/duplicates args)]
    [(concrete-multi lst)
     (extract-all-concrete-variables/duplicates lst)]))
(provide
 (proc-doc/names
  extract-all-concrete-variables/duplicates
  (-> (or/c concrete-domain-elem? concrete-multi? (listof (or/c term? atom? concrete-multi?))) (listof variable?))
  (c)
  @{Extracts all concrete variables from a concrete element @racket[c].}))

(define (mask-singletons c)
  (define singletons
    ((compose
      list->set
      (curry filter (match-lambda [(cons k v) (= v 1)]))
      frequencies
      extract-all-concrete-variables/duplicates)
     c))
  (define (mask context)
    (match context
      [(? list?)
       (map mask context)]
      [(concrete-multi lst)
       (concrete-multi (map mask lst))]
      [(atom sym args)
       (atom sym (map mask args))]
      [(function sym args)
       (function sym (map mask args))]
      [(variable name)
       #:when (not (set-member? singletons context))
       context]
      [(variable name)
       (variable '_)]))
  (mask c))
(provide mask-singletons)