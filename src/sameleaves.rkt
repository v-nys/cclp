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
sameleaves(T1,T2) :- collect(T1,T1L),collect(T2,T2L),eq(T1L,T2L).
eq([],[]).
eq([H|T1],[H|T2]) :- eq(T1,T2).
collect(node(X),[X]).
collect(tree(L,R),C) :- collect(L,CL),collect(R,CR),append(CL,CR,C).
append([],L,L).
append([H|T],L,[H|TR]) :- append(T,L,TR).

{PREPRIOR}
collect(γ1,α1),eq(α1,α2)
collect(γ1,α1),append(α1,α2,α3)
append(γ1,α1,α2),collect(γ1,α1)
append(γ1,α1,α2),append([γ1|α1],α2,α3)
append([γ1|α1],α2,α3),collect(γ1,α1)
eq([γ1|α1],α2),collect(γ1,α1)
eq(α1,[]),collect(γ1,α1)
append(α1,α2,[γ1|α3]),collect(γ1,α1)
collect(γ1,[]),collect(γ1,[γ2|α1])
append(α1,α2,[]),collect(γ1,[γ2|α1])
collect(γ1,[]),append(α1,α2,[])

{CONCRETE CONSTANTS}
nil

{QUERY}
sameleaves(γ1,γ2)