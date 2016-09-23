before(a,b).
before(a,c).
% before(b,a). % introduces loop A-B-A or A-C-B-A
before(b,d).
before(c,b).
before(c,e).
before(d,e).

reaches_without_encountering(X,Y,Path) :-
  before(X,Y),
  \+member(Y,Path).
reaches_without_encountering(X,Z,Path) :-
  before(X,Y),
  \+member(Y,Path),
  reaches_without_encountering(Y,Z,[Y|Path]).
  
reaches(X,Y) :- reaches_without_encountering(X,Y,[X]).

violates_po :- reaches(X,Y), reaches(Y,X), X \== Y.

reaches_all(X,[]).
reaches_all(X,[Destination|Ds]) :- X == Destination, reaches_all(X,Ds).
reaches_all(X,[Destination|Ds]) :- reaches(X,Destination), reaches_all(X,Ds).

% remaining knowledge is difficult to test here, as it interacts with Racket