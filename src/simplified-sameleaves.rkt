#lang reader "lp-reader.rkt"
% this is not the full sameleaves program
% it is a test program which generates a single infinite branch
% this branch can be used to check (an initial version) of the implementation of s-similarity

sameleaves(T1,T2) :- collect(T1,T1L), collect(T2,T2L), eq(T1L,T2L).
collect(tree(L,R),C) :- collect(L,CL), collect(R,CR), append(CL,CR,C).