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
% an atypical program which requires more than one target atom to be "active" at the same time
% this can probably be made to work, but not with generational trees that simply assume a target
% and assign numbers to descendants
% it would probably work if generations were something like 1/UID-target-atom
% s-similarity would also have to be modified a bit
% more precisely, the "context" would not consist of all atoms outside of the family
% only the initial and final levels of other families would be relevant
{PROGRAM}
root :- b, a.
b :- d, e.
a :- f, g.
e :- b.
f :- a.

{PREPRIOR}
b,a
a,d
a,e
e,f
e,g
e,d
f,d
f,b
f,g

{CONCRETE CONSTANTS}
nil

{QUERY}
root