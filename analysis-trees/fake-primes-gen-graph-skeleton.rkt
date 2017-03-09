#lang cclp/gg
% a "fake" primes-like program
% this is meant to test the annotation step in some more complex scenarios
% writing out enough levels of the normal primes program would take a lot more work
NODES
1  *fakeprimes(g3,a2)*

2  integers(g1,a1)
3  *sift([g2|a1],a2)*
4  len(a2,g3)

5  integers(g1,a1)
6  filter(g2,a1,a2)
7  *sift([g3|a2],a3)*
8  len(a3,g4)

9  integers(g1,a1)
10  filter(g2,a1,a2)
11 filter(g3,a2,a3)
12 *sift([g4|a3],a4)*
13 len(a4,g5)

14 integers(g1,a1)
15 filter(g2,a1,a2)
16 filter(g3,a2,a3)
17 filter(g4,a3,a4)
18 sift([g5|a4],a5)
19 len(a5,g6)

20 integers(g1,a1)
21 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a2},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a2,a<1,L,2>/a3})
22 filter(g4,a3,a4)
23 *sift([g5|a4],a5)*
24 len(a5,g6)

25 integers(g1,a1)
26 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a2},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a2,a<1,L,2>/a3})
27 filter(g4,a3,a4)
28 filter(g5,a4,a5)
29 sift(a5,a6)
30 len([g5|a6],g6)

31 integers(g1,a1)
32 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a2},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
33 filter(g5,a4,a5)
34 sift(a5,a6)
35 *len([g5|a6],g6)*

36 *integers(g1,a1)*
37 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a2},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
38 filter(g5,a4,a5)
39 sift(a5,a6)
40 len(a6,g7)

41 integers(g8,a8)
42 *multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/[g1|a8],a<1,1,2>/a2},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})*
43 filter(g5,a4,a5)
44 sift(a5,a6)
45 len(a6,g7)

46 integers(g8,a8)
47 *filter(g2,[g1|a8],a2)*
48 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/a2},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
49 filter(g5,a4,a5)
50 sift(a5,a6)
51 len(a6,g7)

52 integers(g8,a8)
53 filter(g2,a8,a9)
54 *multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a9]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})*
55 filter(g5,a4,a5)
56 sift(a5,a6)
57 len(a6,g7)

58 integers(g8,a8)
59 filter(g2,a8,a9)
60 *filter(g10,[g1|a9],a10)*
61 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/a10},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
62 filter(g5,a4,a5)
63 sift(a5,a6)
64 len(a6,g7)

65 integers(g8,a8)
66 filter(g2,a8,a9)
67 filter(g10,a9,a11)
68 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a11]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
69 filter(g5,a4,a5)
70 sift(a5,a6)
71 len(a6,g7)

72 integers(g8,a8)
73 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a8,a<2,1,2>/a9},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g10,a<2,L,1>/a9,a<2,L,2>/a11})
74 *multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a11]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})*
75 filter(g5,a4,a5)
76 sift(a5,a6)
77 len(a6,g7)

78 integers(g8,a8)
79 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a8,a<2,1,2>/a9},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g10,a<2,L,1>/a9,a<2,L,2>/a11})
80 *filter(g12,[g1|a11],a12)*
81 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/a12},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
82 filter(g5,a4,a5)
83 sift(a5,a6)
84 len(a6,g7)

85 integers(g8,a8)
86 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a8,a<2,1,2>/a9},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g10,a<2,L,1>/a9,a<2,L,2>/a11})
87 filter(g12,a11,a13)
88 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a13]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
89 filter(g5,a4,a5)
90 sift(a5,a6)
91 len(a6,g7)

92 integers(g8,a8)
93 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a8,a<2,1,2>/a9},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g12,a<2,L,1>/a11,a<2,L,2>/a13})
94 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a13]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a3,a<1,L,2>/a4})
95 filter(g5,a4,a5)
96 sift(a5,a6)
97 len(a6,g7)

EDGES
1 -> 2, 3, 4.

2 -> 5.
3 -> 6, 7.
4 -> 8.

5 -> 9.
6 -> 10.
7 -> 11, 12.
8 -> 13.

9 -> 14.
10 -> 15.
11 -> 16.
12 -> 17, 18.
13 -> 19.

14 -> 20.
15 -> 21.
16 -> 21.
17 -> 22.
18 -> 23.
19 -> 24.

20 -> 25.
21 -> 26.
22 -> 27.
23 -> 28, 29.
24 -> 30.

25 -> 31.
26 -> 32.
27 -> 32.
28 -> 33.
29 -> 34.
30 -> 35.

31 -> 36.
32 -> 37.
33 -> 38.
34 -> 39.
35 -> 40.

36 -> 41.
37 -> 42.
38 -> 43.
39 -> 44.
40 -> 45.

41 -> 46.
42 -> 47, 48.
43 -> 49.
44 -> 50.
45 -> 51.

46 -> 52.
47 -> 53.
48 -> 54.
49 -> 55.
50 -> 56.
51 -> 57.

52 -> 58.
53 -> 59.
54 -> 60, 61.
55 -> 62.
56 -> 63.
57 -> 64.

58 -> 65.
59 -> 66.
60 -> 67.
61 -> 68.
62 -> 69.
63 -> 70.
64 -> 71.

65 -> 72.
66 -> 73.
67 -> 73.
68 -> 74.
69 -> 75.
70 -> 76.
71 -> 77.

72 -> 78.
73 -> 79.
74 -> 80, 81.
75 -> 82.
76 -> 83.
77 -> 84.

78 -> 85.
79 -> 86.
80 -> 87.
81 -> 88.
82 -> 89.
83 -> 90.
84 -> 91.

85 -> 92.
86 -> 93.
87 -> 93.
88 -> 94.
89 -> 95.
90 -> 96.
91 -> 97.