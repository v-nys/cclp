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
6  filter(g2,a1,a3)
7  *sift([g3|a3],a4)*
8  len(a4,g4)

9  integers(g1,a1)
10 filter(g2,a1,a3)
11 filter(g3,a3,a5)
12 *sift([g4|a5],a6)*
13 len(a6,g5)

14 integers(g1,a1)
15 filter(g2,a1,a3)
16 filter(g3,a3,a5)
17 filter(g4,a5,a7)
18 sift([g5|a7],a8)
19 len(a8,g6)

20 integers(g1,a1)
21 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a3},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a3,a<1,L,2>/a5})
22 filter(g4,a5,a7)
23 *sift([g5|a7],a8)*
24 len(a8,g6)

25 integers(g1,a1)
26 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a3},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a3,a<1,L,2>/a5})
27 filter(g4,a5,a7)
28 filter(g5,a7,a9)
29 sift(a9,a10)
30 len([g5|a10],g6)

31 integers(g1,a1)
32 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a3},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
33 filter(g5,a7,a9)
34 sift(a9,a10)
35 *len([g5|a10],g6)*

36 *integers(g1,a1)*
37 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a1,a<1,1,2>/a3},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
38 filter(g5,a7,a9)
39 sift(a9,a10)
40 len(a10,g7)

41 integers(g1000,a1000)
42 *multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/[g1|a1000],a<1,1,2>/a3},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})*
43 filter(g5,a7,a9)
44 sift(a9,a10)
45 len(a10,g7)

46 integers(g1000,a1000)
47 *filter(g2,[g1|a1000],a3)*
48 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/a3},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
49 filter(g5,a7,a9)
50 sift(a9,a10)
51 len(a10,g7)

52 integers(g1000,a1000)
53 filter(g2,a1000,a1001)
54 *multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a1001]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})*
55 filter(g5,a7,a9)
56 sift(a9,a10)
57 len(a10,g7)

58 integers(g1000,a1000)
59 filter(g2,a1000,a1001)
60 *filter(g10,[g1|a1001],a1002)*
61 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/a1002},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
62 filter(g5,a7,a9)
63 sift(a9,a10)
64 len(a10,g7)

65 integers(g1000,a1000)
66 filter(g2,a1000,a1001)
67 filter(g10,a1001,a1003)
68 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a1003]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
69 filter(g5,a7,a9)
70 sift(a9,a10)
71 len(a10,g7)

72 integers(g1000,a1000)
73 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a1000,a<2,1,2>/a1001},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g10,a<2,L,1>/a1001,a<2,L,2>/a1003})
74 *multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a1003]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})*
75 filter(g5,a7,a9)
76 sift(a9,a10)
77 len(a10,g7)

78 integers(g1000,a1000)
79 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a1000,a<2,1,2>/a1001},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g10,a<2,L,1>/a1001,a<2,L,2>/a1003})
80 *filter(g12,[g1|a1003],a12)*
81 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/a12},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
82 filter(g5,a7,a9)
83 sift(a9,a10)
84 len(a10,g7)

85 integers(g1000,a1000)
86 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a1000,a<2,1,2>/a1001},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g10,a<2,L,1>/a1001,a<2,L,2>/a1003})
87 filter(g12,a1003,a13)
88 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a13]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
89 filter(g5,a7,a9)
90 sift(a9,a10)
91 len(a10,g7)

92 integers(g1000,a1000)
93 multi((filter(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a1000,a<2,1,2>/a1001},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g12,a<2,L,1>/a1003,a<2,L,2>/a13})
94 multi((filter(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{a<1,1,1>/[g1|a13]},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g4,a<1,L,1>/a5,a<1,L,2>/a7})
95 filter(g5,a7,a9)
96 sift(a9,a10)
97 len(a10,g7)

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