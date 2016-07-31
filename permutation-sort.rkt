#lang reader "abstractlp-reader.rkt"
% this is a comment
% this is another comment
sort(X,Y) ← perm(X,Y), ord(Y).

perm([],[]).
perm([X|Y],[U|V]) ← del(U,[X|Y],W),perm(W,V).

del(X,[X|Y],Y).
del(X,[Y|U],[Y|V]) ← del(X,U,V).

ord([]).
ord([X]).
ord([X,Y|Z]) ← X=<Y,ord([Y|Z]).