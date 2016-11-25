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
% this shouldn't end up with a closed analysis
% on some branches, we will get a seemingly random sequence of appendA/appendB atoms (interspersed with filters)
{PROGRAM}
sameleaves(T1,T2) :- collect(T1,T1L),collect(T2,T2L),eq(T1L,T2L).
eq([],[]).
eq([H|T1],[H|T2]) :- eq(T1,T2).
collect(node(X),[X]).
collect(tree(L,R),C) :- collect(L,CL),collect(R,CR),appendA(CL,CR,C).
collect(tree(L,R),C) :- collect(L,CL),collect(R,CR),appendB(CL,CR,C).
appendA([],L,L).
appendA([H|T],L,[H|TR]) :- appendA(T,L,TR).
appendB([],L,L).
appendB([H|T],L,[H|TR]) :- appendB(T,L,TR).

{PREPRIOR}
collect(γ1,α1),eq(α1,α2)
collect(γ1,α1),appendA(α1,α2,α3)
collect(γ1,α1),appendB(α1,α2,α3)
appendA(γ1,α1,α2),collect(γ1,α1)
appendA(γ1,α1,α2),appendA([γ1|α1],α2,α3)
appendA([γ1|α1],α2,α3),collect(γ1,α1)
eq([γ1|α1],α2),collect(γ1,α1)
eq(γ1,α1),collect(γ1,[γ2|α1])
appendA([],α1,α2),eq([γ1|α1],α2)
eq(α1,[]),collect(γ1,α1)
appendA(α1,α2,[γ1|α3]),collect(γ1,α1)
collect(γ1,[]),collect(γ1,[γ2|α1])
collect(γ1,[]),collect(γ1,[γ2])
appendA(α1,α2,[]),collect(γ1,[γ2|α1])
appendA(α1,α2,[]),collect(γ1,[])

{CONCRETE CONSTANTS}
nil

{QUERY}
sameleaves(γ1,γ2)