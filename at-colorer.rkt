#lang br
(require "at-lexer.rkt" brag/support)
(provide at-colorer)

(define (at-colorer port)
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
       (match type
         ['NUMBER '(constant #f)]
         ['ERROR '(error #f)]
         ['COMMENT '(comment #f)]
         ['SYMBOL '(no-color #f)]
         ['BOOLEAN '(no-color #f)]
         ['AVAR-G '(no-color #f)]
         ['AVAR-A '(no-color #f)]
         [else (match val
                 ["(" '(parenthesis |(|)]
                 [")" '(parenthesis |)|)]
                 ["[" '(parenthesis |[|)]
                 ["]" '(parenthesis |]|)]
                 ["{" '(parenthesis |{|)]
                 ["}" '(parenthesis |}|)]
                 [else '(no-color #f)])]))
     (values val cat paren start end)]))