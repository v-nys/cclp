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
queens(N,Q) :- genlist(N,L), perm(L,Q), safe(Q).

perm([],[]).
perm([X|Y],[U|V]) :- del(U,[X|Y],W), perm(W,V).

safe([]).
safe([X]).
safe([X,Y|Z]) :- ndiag(X,1,[Y|Z]), safe([Y|Z]).

ndiag(X,T,[]).
ndiag(X,T,[Y|Z]) :- test(X,T,Y), plus(T,1,S), ndiag(X,S,Z).

{FULL EVALUATION}
plus(γ1,γ2,α1) -> α1/γ3.
minus(γ1,γ2,α1) -> α1/γ3.
abs(γ1,α1) -> α1/γ2.
is(γ1,γ2).
neq(γ1,γ2).
gteq(γ1,γ2).
gt(γ1,γ2).
genlist(γ1,α1) -> α1/γ2.
test(γ1,γ2,γ3).
del(α1,γ1,α2) -> α1/γ2,α2/γ3.

{PREPRIOR}
perm(γ1,α1),safe([γ1|α1])
perm(γ1,α1),ndiag(γ1,γ2,α1)
safe([γ1,γ2|α1]),perm(γ1,α1)
ndiag(γ1,γ2,[γ3|α1]),perm(γ1,α1)
ndiag(γ1,γ2,[γ3|α1]),safe([γ1,γ2|α1])
ndiag(γ1,γ2,[]),safe([γ1])

{CONCRETE CONSTANTS}
nil

{QUERY}
queens(γ1,α1)