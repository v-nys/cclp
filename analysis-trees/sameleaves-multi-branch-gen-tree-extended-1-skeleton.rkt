#lang cclp/gg
NODES
1   *sameleaves(g1,g2)*

2   *collect(g1,a1)*
3   collect(g2,a2)
4   eq(a1,a2)

5   *collect(g3,a3)*
6   collect(g4,a4)
7   append(a3,a4,a1)
8   collect(g2,a2)
9   eq(a1,a2)

10  *collect(g5,a5)*
11  collect(g6,a6)
12  append(a5,a6,a3)
13  collect(g4,a4)
14  append(a3,a4,a1)
15  collect(g2,a2)
16  eq(a1,a2)

% nodes 17 through 19 will disappear before the final level of the graph, so don't increment!
17  collect(g7,a7)
18  collect(g8,a8)
19  append(a7,a8,a5)
20  collect(g6,a6)
21  append(a5,a6,a3)
22  collect(g4,a4)
23  append(a3,a4,a1)
24  collect(g2,a2)
25  eq(a1,a2)

26  *collect(g7,a7)*
27  collect(g8,a8)
28  append(a7,a8,a5)
29  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/a5,a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
30  collect(g2,a2)
31  eq(a1,a2)

32  collect(g8,a8)
33  *append([g9],a8,a5)*
34  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/a5,a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
35  collect(g2,a2)
36  eq(a1,a2)

37  collect(g8,a8)
38  append([],a8,a9)
39  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/[g9|a9],a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
40  collect(g2,a2)
41  eq(a1,a2)

42  collect(g8,a8)
43  *multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/[g9|a8],a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})*
44  collect(g2,a2)
45  eq(a1,a2)

46  collect(g8,a8)
47  collect(g10,a10)
48  *append([g9|a8],a10,a11)*
49  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/a11},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
50  collect(g2,a2)
51  eq(a1,a2)

52  collect(g8,a8)
53  collect(g10,a10)
54  append(a8,a10,a12)
55  *multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a12]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})*
56  collect(g2,a2)
57  eq(a1,a2)

58  collect(g8,a8)
59  collect(g10,a10)
60  append(a8,a10,a12)
61  collect(g13,a13)
62  *append([g9|a12],a13,a14)*
63  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/a14},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
64  collect(g2,a2)
65  eq(a1,a2)

66  collect(g8,a8)
67  collect(g10,a10)
68  append(a8,a10,a12)
69  collect(g13,a13)
70  append(a12,a13,a15)
71  *multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a15]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})*
72  collect(g2,a2)
73  eq(a1,a2)

74  collect(g8,a8)
75  collect(g10,a10)
76  append(a8,a10,a12)
77  collect(g13,a13)
78  append(a12,a13,a15)
79  collect(g16,a16)
80  *append([g9|a15],a16,a17)*
81  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/a17},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
82  collect(g2,a2)
83  eq(a1,a2)

84  collect(g8,a8)
85  collect(g10,a10)
86  append(a8,a10,a12)
87  collect(g13,a13)
88  append(a12,a13,a15)
89  collect(g16,a16)
90  append(a15,a16,a18)
91  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a18]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
92  collect(g2,a2)
93  eq(a1,a2)

94  collect(g8,a8)
95  collect(g10,a10)
96  append(a8,a10,a12)
97  multi((collect(g<2,i,1>,a<2,i,1>),append(a<2,i,2>,a<2,i,1>,a<2,i,3>)),#f,{g<2,1,1>/g13,a<2,1,1>/a13,a<1,1,2>/a12,a<2,1,3>/a15},{a<2,i+1,2>/a<2,i,3>},{g<2,L,1>/g16,a<2,L,1>/a16,a<2,L,2>/a15,a<2,L,3>/a18})
98  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a18]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})
99  collect(g2,a2)
100 eq(a1,a2)

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
39 -> 43.
40 -> 44.
41 -> 45.

42 -> 46.
43 -> 47, 48, 49.
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
54 -> 60.
55 -> 61, 62, 63.
56 -> 64.
57 -> 65.

58 -> 66.
59 -> 67.
60 -> 68.
61 -> 69.
62 -> 70.
63 -> 71.
64 -> 72.
65 -> 73.

66 -> 74.
67 -> 75.
68 -> 76.
69 -> 77.
70 -> 78.
71 -> 79, 80, 81.
72 -> 82.
73 -> 83.

74 -> 84.
75 -> 85.
76 -> 86.
77 -> 87.
78 -> 88.
79 -> 89.
80 -> 90.
81 -> 91.
82 -> 92.
83 -> 93.

84 -> 94.
85 -> 95.
86 -> 96.
87 -> 97.
88 -> 97.
89 -> 97.
90 -> 97.
91 -> 98.
92 -> 99.
93 -> 100.