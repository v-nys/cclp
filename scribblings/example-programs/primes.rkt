; MIT License
;
; Copyright (c) 2016-2017 Vincent Nys
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
  
#lang cclp
{PROGRAM}
primes(N,Primes) :- integers(2,I),sift(I,Primes),length(Primes,N).

integers(N,[]).
integers(N,[N|I]) :- plus(N,1,M),integers(M,I).
            
sift([N|Ints],[N|Primes]) :- filter(N,Ints,F),sift(F,Primes).
sift([],[]).
               
filter(N,[M|I],F) :- divides(N,M), filter(N,I,F).
filter(N,[M|I],[M|F]) :- does_not_divide(N,M), filter(N,I,F).
filter(N,[],[]).

length([],0).
length([H|T],N) :- minus(N,1,M),length(T,M).

{FULL EVALUATION}
plus(g1,g2,a1) -> a1/g3.
minus(g1,g2,a1) -> a1/g3.
divides(g1,g2).
does_not_divide(g1,g2).

{PARTIAL ORDER}
filter(g1,[],a1) > sift(a1,a2)
filter(g1,[],a1) > length(a1,g1)
filter(g1,[],a1) > filter(g1,a1,a2)
sift([g1|a1],a2) > integers(g1,a1)
sift([g1|a1],a2) > length(a1,g1)
integers(g1,a1) > sift(a1,a2)
integers(g1,a1) > length(a1,g1)
integers(g1,a1) > filter(g1,a1,a2)
filter(g1,[g2|a1],a2) > sift(a1,a2)
filter(g1,[g2|a1],a2) > integers(g1,a1)
filter(g1,[g2|a1],a2) > length(a1,g1)
filter(g1,[g2|a1],a2) > filter(g1,a1,a2)
length([g1|a1],g2) > sift(a1,a2)
length([g1|a1],g2) > integers(g1,a1)
length([g1|a1],g2) > filter(g1,a1,a2)
sift([],a1) > length(a1,g1)

{QUERY}
primes(g1,a1)
