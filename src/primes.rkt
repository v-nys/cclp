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

#lang reader "cclp-reader.rkt"
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
plus(γ1,γ2,α1) -> α1/γ3.
minus(γ1,γ2,α1) -> α1/γ3.
divides(γ1,γ2).
does_not_divide(γ1,γ2).
integers(γ1,γ2).
length(γ1,γ2).
% this only occurs for the empty list
% program analysis should be correct this way
% widening or more intelligent use of concrete constants would be better, though5
filter(γ1,α1,γ2) -> α1/γ2.

{PREPRIOR}
integers(γ1,α1),filter(γ1,α1,α2)
integers(γ1,α1),sift(α1,α2)
integers(γ1,α1),length(α1,γ1)
sift([γ1|α1],α2),integers(γ1,α1)
sift(γ1,α1),length(α1,γ1)
filter(γ1,[γ2|α1],α2),integers(γ1,α1)
length([γ1|α1],γ2),integers(γ1,γ2)
length(γ1,γ2),integers(γ1,[γ2|γ3])

{QUERY}
primes(γ1,α1)