#lang at-exp racket
(require
  pict
  scribble/srcdoc
  cclp/abstract-multi-domain
  cclp/gen-graph-structs)
(require
  (for-doc scribble/manual))

(define (gen-node->pict gn)
  (define empty-pict (rectangle 0 0))
  (define (id->pict id)
    (text
     (string-append
      (number->string id)
      ".")))
  (define (abstract-term->pict t)
    (match t
      ['comma (text ",")]
      [(a i)
       (hbl-append
        (text "a")
        (text (~a i) '(subscript)))]
      [(g j)
       (hbl-append
        (text "g")
        (text (~a j) '(subscript)))]
      [(abstract-function sym (list))
       (text (symbol->string sym))]
      [(abstract-function sym args)
       (hc-append
        (text (symbol->string sym))
        (text "(")
        (apply hbl-append
         (for/list ([a (add-between args 'comma)])
           (abstract-term->pict a)))
        (text ")"))]))
  (define (abstract-conjunct->pict c)
    (match c
      ['comma (text ",")]
      [(abstract-atom sym (list))
       (text (symbol->string sym))]
      [(abstract-atom sym args)
       (hc-append
        (text (symbol->string sym))
        (text "(")
        (apply
         hbl-append
         (for/list ([a (add-between args 'comma)])
           (abstract-term->pict a)))
        (text ")"))]
      [(multi patt asc? ic cc fc)
       (text "multi")]))
  (define (gen-or-range->pict rng)
    (match rng
      [(gen nb origin)
       (if origin
           (text (format "generation ~a of family ~a" nb origin))
           empty-pict)]
      [(gen-range fst lst origin asc?)
       (text (format "generations ~a through ~a of family ~a, ~a" fst lst origin (if asc? "ascending" "descending")))]))
  (match gn
    [(gen-node conjunct id rng unf? foldable?)
     (vc-append
      (ht-append
      (id->pict id)
      (abstract-conjunct->pict conjunct))
      (gen-or-range->pict rng)
      (if unf? (text "just unfolded") empty-pict)
      (if foldable? empty-pict (text "cannot fold")))]))
(provide
 (proc-doc/names
  gen-node->pict
  (-> gen-node? pict?)
  (gn)
  @{Produces a human-readable representation of the contents of a @racket[gen-node?].}))