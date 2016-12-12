; MIT License
;
; Copyright (c) 2016 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#lang at-exp racket
(require scribble/srcdoc)
(require (for-doc scribble/manual))

(require "abstract-multi-domain.rkt")
(require (only-in "abstraction-inspection-utils.rkt" maximum-var-index))
(require (only-in "data-utils.rkt" some-v some?))
(require (only-in racket-list-utils/utils map-accumulatel))

(define (ground? elem)
  (cond [(list? elem) (andmap ground? elem)]
        [(abstract-atom? elem) (andmap ground? (abstract-atom-args elem))]
        [(abstract-function? elem) (andmap ground? (abstract-function-args elem))]
        [(g? elem) #t]
        [(a? elem) #f]
        [else (error "element unaccounted for in groundness check")]))

(define (depth elem)
  (cond [(list? elem) (apply max (map depth elem))]
        [(abstract-atom? elem) (apply max (map depth (abstract-atom-args elem)))]
        [(abstract-function? elem)
         (+
          1
          (if
           (null? (abstract-function-args elem))
           0
           (apply max (map depth (abstract-function-args elem)))))]
        [(abstract-variable? elem) 1]
        [else (error "element unaccounted for")]))

(define (abstract elem k)
  (define fresh-g (if (some? (maximum-var-index elem g?)) (+ (some-v (maximum-var-index elem g?)) 1) 1))
  (define fresh-a (if (some? (maximum-var-index elem a?)) (+ (some-v (maximum-var-index elem a?)) 1) 1))
  (define init-acc (list fresh-a fresh-g (hash)))
  
  (define (widen-atom k-aux atom acc)
    (let ([accumulation-args (map-accumulatel (curry widen-term k) acc (abstract-atom-args atom))])
      (cons (abstract-atom (abstract-atom-symbol atom) (car accumulation-args)) (cdr accumulation-args))))

  (define (widen-term k-aux term acc)
    (match
        acc
      [(list fresh-a fresh-g mapping)
       (if (<= (depth term) k-aux)
           (cons term acc)
           (if (equal? k-aux 1)
               (let ([current-mapping (hash-ref mapping term #f)])
                 (if current-mapping
                     (cons current-mapping acc)
                     (if (ground? term)
                         (cons (g fresh-g) (list fresh-a (+ fresh-g 1) (hash-set mapping term (g fresh-g))))
                         (cons (a fresh-a) (list (+ fresh-a 1) fresh-g (hash-set mapping term (a fresh-a)))))))
               (let ([recursion (map-accumulatel (curry widen-term (- k-aux 1)) acc (abstract-function-args term))])
                 (cons (abstract-function (abstract-function-functor term) (car recursion))
                       (cdr recursion)))))]))
  
  (cond [(list? elem)
         ; TODO: update once multi is incorporated
         (car (map-accumulatel (curry widen-atom k) init-acc elem))]
        [(abstract-atom? elem)
         (abstract-atom
          (abstract-atom-symbol elem)
          (car (map-accumulatel (curry widen-term k) init-acc (abstract-atom-args elem))))]
        [(abstract-function? elem)
         (car (widen-term k elem init-acc))]
        [(abstract-variable? elem) elem]))
(provide
 (proc-doc/names
  abstract
  (-> abstract-domain-elem? exact-positive-integer? abstract-domain-elem?)
  (abstract-domain-element k)
  @{Return the most specific abstraction of @racket[abstract-domain-element] whose depth is at most @racket[k].
 Depth is defined as follows: for abstract variables and constants in the abstract domain, it is 1.
 For function terms, it is the depth of the argument with the greatest depth, increased by one.
 For abstract atoms and conjunctions, it is identical to the depth of the argument with the greatest depth.
 This definition is based on the assumption that we only use first-order logic and thus cannot nest atoms.}))

(module+ test
  (require rackunit)
  (check-equal? (abstract (a 1) 1) (a 1))
  (check-equal? (abstract (g 1) 1) (g 1))
  (check-equal? (abstract (a 1) 2) (a 1))
  (check-equal? (abstract (g 1) 2) (g 1))
  (check-equal?
   (abstract (abstract-atom 'foo (list (a 1))) 1)
   (abstract-atom 'foo (list (a 1))))
  (check-equal?
   (abstract (abstract-atom 'foo (list (abstract-function 'bar '()))) 1)
   (abstract-atom 'foo (list (abstract-function 'bar '()))))
  (check-equal?
   (abstract (abstract-atom 'foo (list (abstract-function 'bar (list (a 1) (a 2))))) 1)
   (abstract-atom 'foo (list (a 3))))
  (check-equal?
   (abstract
    (abstract-atom
     'foo
     (list
      (abstract-function
       'bar
       (list
        (abstract-function 'baz (list (a 1)))
        (abstract-function 'baz (list (a 1)))))))
    2)
   (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2)))))
   "abstraction preserves aliasing within an atom")
  (let ([test-atom
         (abstract-atom
          'foo
          (list
           (abstract-function
            'bar
            (list
             (abstract-function 'baz (list (a 1)))
             (abstract-function 'baz (list (a 1)))))))])
    (check-equal?
     (abstract (list test-atom test-atom) 2)
     (list
      (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2)))))
      (abstract-atom 'foo (list (abstract-function 'bar (list (a 2) (a 2))))))
     "abstraction preserves aliasing within an abstract conjunction"))
  (check-equal?
   (abstract
    (abstract-function ; depth is 5 - occurs at level 1
     'foo
     (list
      (abstract-function ; depth is 2 - occurs at level 2
       'bar
       (list
        (g 1) ; depth is 1
        (a 2))) ; depth is 1
      (abstract-function ; depth is 4 - occurs at level 2
       'cons
       (list
        (g 3)
        (abstract-function ; depth is 3 - occurs at level 3
         'cons
         (list
          (abstract-function ; depth is 2 - occurs at level 4
           'bar
           (list
            (g 1) ; depth is 1 - occurs at level 5
            (a 2))) ; depth is 1 - occurs at level 5
          (abstract-function
           'nil ; depth is 1 - occurs at level 5
           '())))))))
    4)
   (abstract-function ; depth is at most 4
    'foo
    (list ; elements have depth at most 3
     (abstract-function
      'bar
      (list ; elements have depth at most 2
       (g 1)
       (a 2)))
     (abstract-function
      'cons
      (list ; elements have depth at most 2
       (g 3)
       (abstract-function
        'cons
        (list ; elements have depth 1
         (a 3)
         (abstract-function
          'nil
          '())))))))
   "aliasing can be lost if aliased terms occur at different levels"))