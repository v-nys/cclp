#lang br
(require "gg-lexer.rkt" brag/support)
(provide gg-colorer)

(define (gg-colorer port)
  (define (handle-lexer-error excn)
    (define excn-srclocs (exn:fail:read-srclocs excn))
    (srcloc-token (token 'ERROR) (car excn-srclocs)))
  (define srcloc-tok
    (with-handlers ([exn:fail:read? handle-lexer-error])
      (top-lexer port)))
  (match srcloc-tok
    [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
    [else
     (match-define
       (srcloc-token
        (token-struct type val _ _ _ _ _)
        (srcloc _ _ _ posn span)) srcloc-tok)
     (define start posn)
     (define end (+ start span))
     (match-define (list cat paren)
       (match val
                 ["(" '(parenthesis |(|)]
                 [")" '(parenthesis |)|)]
                 ["[" '(parenthesis |[|)]
                 ["]" '(parenthesis |]|)]
                 ["{" '(parenthesis |{|)]
                 ["}" '(parenthesis |}|)]
                 [else '(no-color #f)]))
     (values val cat paren start end)]))