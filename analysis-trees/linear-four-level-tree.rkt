#lang reader "../at.rkt"
(1.*foo* [?]
 (2.*bar*,baz [?]
  {} foo :- bar,baz.
  (3.*quux*,baz [?]
   {} bar :- quux.
   (narf,baz [?]))))