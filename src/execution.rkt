#lang racket
(struct priority (atom1 atom2))
(provide (struct-out priority))