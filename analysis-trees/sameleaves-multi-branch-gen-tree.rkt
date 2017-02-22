#lang cclp/gg
NODES
1  sameleaves(g1,g2) 0 #f

2  collect(g1,a1) 0 #f
3  collect(g2,a2) 0 #f
4  eq(a1,a2) 0 #f

5  collect(g3,a3) 1 0
6  collect(g4,a4) 1 0
7  append(a3,a4,a1) 1 0
8  collect(g2,a2) 0 #f
9  eq(a1,a2) 0 #f

10 collect(g5,a5) 2 0
11 collect(g6,a6) 2 0
12 append(a5,a6,a3) 2 0
13 collect(g4,a4) 1 0
14 append(a3,a4,a1) 1 0
15 collect(g2,a2) 0 #f
16 eq(a1,a2) 0 #f

17 collect(g7,a7) 3 0
18 collect(g8,a8) 3 0
19 append(a7,a8,a5) 3 0
20 collect(g6,a6) 2 0
21 append(a5,a6,a3) 2 0
22 collect(g4,a4) 1 0
23 append(a3,a4,a1) 1 0
24 collect(g2,a2) 0 #f
25 eq(a1,a2) 0 #f

26 collect(g7,a7) q+1 0
27 collect(g8,a8) q+1 0
28 append(a7,a8,a5) q+2 0
29 multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/a5,a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) q:1 0
30 collect(g2,a2) 0 #f
31 eq(a1,a2) 0 #f

EDGES
1  -> 2, 3, 4.

2  -> 5, 6, 7.
3  -> 8.
4  -> 9.

5  -> 10, 11, 12.
6  -> 13.
7  -> 14.
8  -> 15.
9  -> 16.

10 -> 17, 18, 19.
11 -> 20.
12 -> 21.
13 -> 22.
14 -> 23.
15 -> 24.
16 -> 25.

17 -> 26.
18 -> 27.
19 -> 28.
20 -> 29.
21 -> 29.
22 -> 29.
23 -> 29.
24 -> 30.
25 -> 31.