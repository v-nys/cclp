#lang racket
(require cclp/concrete-domain
         (only-in cclp/domain-switching cons-symbol concrete-nil))

(define (synth-str concrete-e)
  (match concrete-e
    [(? list?)
     (string-join
      (map synth-str concrete-e)
      ",")]
    [(concrete-multi lst)
     (format
      "multi(~a)"
      (synth-str lst))]
    [(or
      (atom sym (list))
      (function sym (list)))
     (format "~a" sym)]
    [(or
      (atom sym lst)
      (function sym lst))
     (format "~a(~a)" sym (synth-str lst))]
    [(variable v)
     (symbol->string v)]
    [else (error (format "can't print this: ~a" else))]))
(module+ test
  (require rackunit)
  (check-equal?
   (synth-str
    (list
     (atom 'integers (map variable '(G24 A31A)))
     (concrete-multi
      (function
       cons-symbol
       (list
        (function
         'building_block
         (list
          (function
           cons-symbol
           (list         
            (function
             'filter
             (map variable '(G25 A31B A35A)))
            concrete-nil))))
        (function
         cons-symbol
         (list
          (function
           'building_block
           (list
            (function
             cons-symbol
             (list
              (function
               'filter
               (map variable '(G27 A35B A38A)))
              concrete-nil))))
          concrete-nil)))))
     (atom 'filter (map variable '(G28 A38B A40A)))
     (atom 'sift (map variable '(A40B A39A)))
     (atom 'alt_length (map variable '(G28A39B G20)))))   "integers(G24,A31A),multi('[|]'(building_block('[|]'(filter(G25,A31B,A35A),[])),'[|]'(building_block('[|]'(filter(G27,A35B,A38A),[])),[]))),filter(G28,A38B,A40A),sift(A40B,A39A),alt_length(G28A39B,G20)"))
(provide synth-str)
