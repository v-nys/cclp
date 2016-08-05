#lang typed/racket

(require "abstract-multi-domain.rkt" "abstract-substitution.rkt")
(struct FullAIRule ([input-pattern : abstract-atom] [output-substitution : AbstractSubstitution]))