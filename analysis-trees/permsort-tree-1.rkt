#lang reader "../at.rkt"
(1.*permsort(g1,a1)*
 (2.*perm(g1,a1)*,ord(a1)
  {} permsort(X,Y) :- perm(X,Y),ord(Y).
  (3.*ord([])*
   {a1/[], g1/[]} perm([],[]).
   (□
    {} ord([]).))
  (4.*select(a2,[g2|g3],a4)*,perm(a4,a3),ord([a2|a3])
   {g1/[g2|g3], a1/[a2|a3]} perm([X|Y],[U|V]) :- select(U,[X|Y],W),perm(W,V).)))