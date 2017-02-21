#lang cclp/at
(.1.*permsort(g1,a1)*
 (.2.*perm(g1,a1)*,ord(a1) [perm(g1,a1) < ord(a1)]
  {} permsort(X,Y) :- perm(X,Y),ord(Y).
  (.3.*ord([])*
   {a1/[], g1/[]} perm([],[]).
   (.□
    {} ord([]).))
  (.4.*select(a2,[g2|g3],a4)*,perm(a4,a3),ord([a2|a3])
   {g1/[g2|g3], a1/[a2|a3]} perm([X|Y],[U|V]) :- select(U,[X|Y],W),perm(W,V).
   (.5.*perm(g5,a3)*,ord([g4|a3]) [perm(g1,a1) < ord([g1|a1])]
    {a2/g4, a4/g5} select(a1,[g1|g2],a2) -> {a1/g3, a2/g4}
    (.6.*ord([g2])*
     {a1/[], g1/[]} perm([],[]).
     (.□
      {} ord([X]).))
    (.7.*select(a2,[g3|g4],a4)*,perm(a4,a3),ord([g2,a2|a3])
     {g1/[g3|g4], a1/[a2|a3]} perm([X|Y],[U|V]) :- select(U,[X|Y],W),perm(W,V).
     (.8.perm(g6,a3),*ord([g2,g5|a3])* [ord([g1,g2|a1]) < perm(g1,a1)]
      {a2/g5, a4/g6} select(a1,[g1|g2],a2) -> {a1/g3, a2/g4}
      (.9.perm(g6,a3),*leq(g2,g5)*,ord([g5|a3])
       {} ord([X,Y|Z]) :- lte(X,Y),ord([Y|Z]).
       (.10.perm(g6,a3),ord([g5|a3])
        {} select(a1,[g1|g2],a2) -> {a1/g3, a2/g4}
        (.!CY 5)))))))))