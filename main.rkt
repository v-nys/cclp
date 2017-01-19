#lang racket
(module reader racket
  (require (only-in "cclp-reader.rkt" read-syntax))
  (provide read-syntax))