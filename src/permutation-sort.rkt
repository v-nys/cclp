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
PROGRAM:
% this program should evaluate to a list of Knowledge instances
sort(X,Y) :- perm(X,Y),ord(Y).

perm([],[]).
perm([X|Y],[U|V]) :- del(U,[X|Y],W),perm(W,V).

del(X,[X|Y],Y).
del(X,[Y|U],[Y|V]) :- del(X,U,V).

ord([]).
ord([X]).
ord([X,Y|Z]) :- lte(X,Y),ord([Y|Z]).

FULL EVALUATION:
lte(γ1,γ2) -> γ1/γ1.
del(α1,[γ1|γ2],α2) -> α1/γ3,α2/γ4.

PREPRIOR:
% note that aliasing between atoms in pairs has no meaning here
% also note that atoms which appear alone do not need to be here
perm(γ1,α1),ord(α1)
perm(γ1,α1),ord([γ2|α1]) % may be necessary (priority over more general instance does not imply priority over this one), leaving it in for now
ord([γ1,γ2|α1]),perm(α1,α2)