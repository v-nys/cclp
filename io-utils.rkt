#lang at-exp racket
(require syntax/macro-testing)
(require (only-in "control-flow.rkt" until))
(require scribble/srcdoc)

(define (prompt-for-integer)
  (define read-something (read))
  (if (exact-integer? read-something)
      read-something
      (begin (displayln "Not an integer!")
             (prompt-for-integer))))
(module+ test
  (require rackunit)
  (let ([output-port (open-output-string)])
    (parameterize
        ([current-output-port output-port]
         [current-input-port (open-input-string "hello\ntwelve\n12")])
      (check-equal? (prompt-for-integer) 12)
      (check-equal? (get-output-string output-port) "Not an integer!\nNot an integer!\n"))))
(provide
 (proc-doc/names
  prompt-for-integer
  (-> exact-positive-integer?)
  ()
  @{"Asks the user to enter a positive integer and returns it."}))

(define (between? val lower upper)
  (and (>= val lower) (<= val upper)))

(define-syntax-rule (interactive-dispatch prompt (label body ...) ...)
  (#%app
   (let ([choices (list label ...)]
         [actions (list (λ () body ...) ...)])
     (displayln prompt)
     (foldl (λ (el acc) (begin (printf "~a: ~a\n" acc el) (+ acc 1))) 1 choices)
     (define int (prompt-for-integer))
     (until (between? int 1 (length choices))
            (displayln "Not a valid choice!")
            (set! int (prompt-for-integer)))
     (list-ref actions (- int 1)))))
(module+ test
  (let ([output-port (open-output-string)])
    (parameterize
        ([current-output-port output-port]
         [current-input-port (open-input-string "hello\ntwelve\n12\n2")])
      (check-equal?
       (interactive-dispatch
        "What can I get you?"
        ("Gimme foo" 'foo)
        ("Gimme bar" 'bar)
        ("Print something" (print "Don't print this!")))
       'bar)
      (check-equal?
       (get-output-string output-port)
       "What can I get you?\n1: Gimme foo\n2: Gimme bar\n3: Print something\nNot an integer!\nNot an integer!\nNot a valid choice!\n"))))
(provide
 (form-doc
  (interactive-dispatch prompt (label body ...) ...)
  @{Presents the user with several options and executes the bodies associated with the selected one.}))