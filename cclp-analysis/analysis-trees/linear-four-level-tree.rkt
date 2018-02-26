#lang cclp/at
(.1.*foo*
 (.2.*bar*,baz [bar < baz]
  {} foo :- bar,baz.
  (.3.*quux*,baz [quux < baz]
   {} bar :- quux.
   (.narf,baz))))