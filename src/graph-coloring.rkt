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
safe_coloring(Ns,Cs) :- coloring(Cs),safe(Ns,Cs).

coloring([]).
coloring([C|Cs]) :- color(C),coloring(Cs).

safe([],[]).
safe([N],[C]).
safe([N_1,N_2|Ns],[C_1,C_2|Cs]) :-
    allsafe(N_1,C_1,[N_2|Ns],[C_2|Cs]),
    safe([N_2|Ns],[C_2|Cs]).

allsafe(N,C,[],[]).
allsafe(N_1,C_1,[N_2|Ns],[C_2|Cs]) :-
    test(N_1,C_1,N_2,C_2),
    allsafe(N_2,C_2,Ns,Cs).

{FULL EVALUATION}
color(α1) -> α1/γ1.
test(γ1,γ2,γ3,γ4).

{PREPRIOR}
coloring(α1),safe([γ1|γ2],[γ2|α1])
coloring(α1),allsafe(γ1,γ2,γ3,α1)
safe(γ1,[γ2,γ3|α1]),coloring(α1)
allsafe(γ1,γ2,γ3,[γ4|α1]),coloring(α1)
allsafe(γ1,γ2,γ3,[γ4|α1]),safe([γ1|γ2],[γ3,γ4|α1])

{QUERY}
safe_coloring(γ1,α1)