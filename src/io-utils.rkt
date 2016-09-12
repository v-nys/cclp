#lang racket
;(: prompt-for-integer (-> Integer))
(define (prompt-for-integer)
  (define read-something (read))
  (if (exact-integer? read-something)
      read-something
      (begin (displayln "Not an integer!")
             (prompt-for-integer))))

;(: prompt-for-answer (-> String String String String * String))
(define (prompt-for-answer question choice1 choice2 . choices)
  (define all-choices (cons choice1 (cons choice2 choices)))
  (begin
    (displayln question)
    (foldl (λ (el acc) (begin (printf "~a: ~a\n" acc el) (+ acc 1))) 1 all-choices)
    (define int (prompt-for-integer))
    (if (and (>= int 1) (<= int (length all-choices)))
        (list-ref all-choices (- int 1))
        (begin (displayln "Not a valid choice!")
               (apply prompt-for-answer question choice1 choice2 choices)))))
(provide prompt-for-answer)