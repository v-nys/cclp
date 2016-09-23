member_new(X,[X|Y]).
member_new(X,[A|B]) :- member_new(X,B).