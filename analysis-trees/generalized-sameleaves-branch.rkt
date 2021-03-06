#lang cclp/at
(.1.*sameleaves(g1,g2)*
 (.2.*collect(g1,a1)*,collect(g2,a2),eq(a1,a2) [collect(g1,a1) < eq(a1,a2)]
  {} sameleaves(X,Y) :- collect(X,XL),collect(Y,YL),eq(XL,YL).
  (.3.*collect(g3,a3)*,collect(g4,a4),append(a3,a4,a1),collect(g2,a2),eq(a1,a2) [collect(g1,a1) < append(a1,a2,a3)]
   {g1/t(g3,g4)} collect(t(X,Y),L) :- collect(X,XL),collect(Y,YL),append(XL,YL,L).
   (.4.*collect(g5,a5)*,collect(g6,a6),append(a5,a6,a3),collect(g4,a4),append(a3,a4,a1),collect(g2,a2),eq(a1,a2)
    {g3/t(g5,g6)} collect(t(X,Y),L) :- collect(X,XL),collect(Y,YL),append(XL,YL,L).
    (.5.*collect(g7,a7)*,collect(g8,a8),append(a7,a8,a5),collect(g6,a6),append(a5,a6,a3),collect(g4,a4),append(a3,a4,a1),collect(g2,a2),eq(a1,a2)
     {g5/t(g7,g8)} collect(t(X,Y),L) :- collect(X,XL),collect(Y,YL),append(XL,YL,L).
     (.collect(g9,a9),collect(g10,a10),append(a9,a10,a7),collect(g8,a8),append(a7,a8,a5),collect(g6,a6),append(a5,a6,a3),collect(g4,a4),append(a3,a4,a1),collect(g2,a2),eq(a1,a2)
      {g7/t(g9,g10)} collect(t(X,Y),L) :- collect(X,XL),collect(Y,YL),append(XL,YL,L).
      (.!GEN collect(g9,a9),collect(g10,a10),append(a9,a10,a7),multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g8,a<1,1,1>/a8,a<1,1,2>/a7,a<1,1,3>/a5},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}),collect(g2,a2),eq(a1,a2) ((3 8)))))))))