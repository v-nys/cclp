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

#lang reader "lp-reader.rkt"
% this program is a "warm-up" exercise for the expander
% the result of running it is a list, which is printed in the interactions panel
% ideally, we should be able to make this list available to other modules
iam.
ithink.
idosomeotherthingsaswell.
idoevenmorethings.
idolotsofthings.
man(john).
emptylist([]).
varfact(X).
woman(mary).
onelementlist([X]).
twoelementlist([X,Y]).
threeelementlist([X,Y,Z]).
tailedlist([X|Y]).
multielementtailedlist([X,Y|Z]).
x(y).
a(b(c(d))).
e(f,g).
%married(john,mary).