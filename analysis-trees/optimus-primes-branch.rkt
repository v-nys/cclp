#lang cclp/at
% branch which leads to introduction of unrelated multi abstractions
% skipping plus in integers and minus in length because they are not particularly relevant to analysis
% also skipping does_not_divide for filters
(.1.*oprimes(g1,a1)*
  (.2.*integers(g2,a2)*,siftA(a2,a3),siftB(a3,a1),length(a1,g1)
    (.3.integers(g3,a4),*siftA([g2|a4],a3)*,siftB(a3,a1),length(a1,g1)
      (.4.integers(g3,a4),filterA(g2,a4,a5),siftA(a5,a6),*siftB([g2|a6],a1)*,length(a1,g1)
        (.5.integers(g3,a4),filterA(g2,a4,a5),siftA(a5,a6),filterB(g2,a6,a7),siftB(a7,a8),*length([g2|a8],g1)*
          (.6.*integers(g3,a4)*,filterA(g2,a4,a5),siftA(a5,a6),filterB(g2,a6,a7),siftB(a7,a8),length(a8,g4)
            (.7.integers(g5,a9),*filterA(g2,[g3|a9],a5)*,siftA(a5,a6),filterB(g2,a6,a7),siftB(a7,a8),length(a8,g4)
              (.8.integers(g5,a9),filterA(g2,a9,a10),*siftA([g3|a10],a6)*,filterB(g2,a6,a7),siftB(a7,a8),length(a8,g4)
                (.9.integers(g5,a9),filterA(g2,a9,a10),filterA(g3,a10,a11),siftA(a11,a12),*filterB(g2,[g3|a12],a7)*,siftB(a7,a8),length(a8,g4)
                  (.10.integers(g5,a9),filterA(g2,a9,a10),filterA(g3,a10,a11),siftA(a11,a12),filterB(g2,a12,a13),*siftB([g3|a13],a8)*,length(a8,g4)
                    (.11.integers(g5,a9),filterA(g2,a9,a10),filterA(g3,a10,a11),siftA(a11,a12),filterB(g2,a12,a13),filterB(g3,a13,a14),siftB(a14,a15),*length([g3|a15],g4)*
                      (.12.*integers(g5,a9)*,filterA(g2,a9,a10),filterA(g3,a10,a11),siftA(a11,a12),filterB(g2,a12,a13),filterB(g3,a13,a14),siftB(a14,a15),length(a15,g6)
                        (.13.integers(g7,a16),*filterA(g2,[g5|a16],a10)*,filterA(g3,a10,a11),siftA(a11,a12),filterB(g2,a12,a13),filterB(g3,a13,a14),siftB(a14,a15),length(a15,g6)
                          (.14.integers(g7,a16),filterA(g2,a16,a17),*filterA(g3,[g5|a17],a11)*,siftA(a11,a12),filterB(g2,a12,a13),filterB(g3,a13,a14),siftB(a14,a15),length(a15,g6)
                            (.15.integers(g7,a16),filterA(g2,a16,a17),filterA(g3,a17,a18),*siftA([g5|a18],a12)*,filterB(g2,a12,a13),filterB(g3,a13,a14),siftB(a14,a15),length(a15,g6)
                              (.16.integers(g7,a16),filterA(g2,a16,a17),filterA(g3,a17,a18),filterA(g5,a18,a19),siftA(a19,a20),filterB(g2,[g5|a20],a13),filterB(g3,a13,a14),siftB(a14,a15),length(a15,g6)
                                (.!GEN 17.integers(g7,a16),multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}),filterA(g5,a18,a19),siftA(a19,a20),*filterB(g2,[g5|a20],a13)*,filterB(g3,a13,a14),siftB(a14,a15),length(a15,g6)
                                  (.18.integers(g7,a16),multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}),filterA(g5,a18,a19),siftA(a19,a20),filterB(g2,a20,a21),*filterB(g3,[g5|a21],a14)*,siftB(a14,a15),length(a15,g6)
                                    (.19.integers(g7,a16),multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}),filterA(g5,a18,a19),siftA(a19,a20),filterB(g2,a20,a21),filterB(g3,a21,a22),*siftB([g5|a22],a15)*,length(a15,g6)
                                      (.20.integers(g7,a16),multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}),filterA(g5,a18,a19),siftA(a19,a20),filterB(g2,a20,a21),filterB(g3,a21,a22),filterB(g5,a22,a23),siftB(a23,a24),length([g5|a24],g6)
                                        (.!GEN integers(g7,a16),multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}),filterA(g5,a18,a19),siftA(a19,a20),multi((filterB(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a20,a<2,1,2>/a21},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g3,a<2,L,1>/a21,a<1,L,2>/a22}),filterB(g5,a22,a23),siftB(a23,a24),length([g5|a24],g6))))))))))))))))))))))