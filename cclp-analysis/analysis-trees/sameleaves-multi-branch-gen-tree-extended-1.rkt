#lang cclp/gg
NODES
1   *sameleaves(g1,g2)* 0 #f

2   *collect(g1,a1)* 0 #f
3   collect(g2,a2) 0 #f
4   eq(a1,a2) 0 #f

5   *collect(g3,a3)* 1 2
6   collect(g4,a4) 1 2
7   append(a3,a4,a1) 1 2
8   collect(g2,a2) 0 #f
9   eq(a1,a2) 0 #f

10  *collect(g5,a5)* 2 2
11  collect(g6,a6) 2 2
12  append(a5,a6,a3) 2 2
13  collect(g4,a4) 1 2
14  append(a3,a4,a1) 1 2
15  collect(g2,a2) 0 #f
16  eq(a1,a2) 0 #f

% nodes 17 through 19 will disappear before the final level of the graph, so don't increment!
17  collect(g7,a7) 2 2
18  collect(g8,a8) 2 2
19  append(a7,a8,a5) 2 2
20  collect(g6,a6) 2 2
21  append(a5,a6,a3) 2 2
22  collect(g4,a4) 1 2
23  append(a3,a4,a1) 1 2
24  collect(g2,a2) 0 #f
25  eq(a1,a2) 0 #f

26  *collect(g7,a7)* l1 2
27  collect(g8,a8) l1 2
28  append(a7,a8,a5) l1 2
29  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/a5,a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1:1:#f 2
30  collect(g2,a2) 0 #f
31  eq(a1,a2) 0 #f

32  collect(g8,a8) l1 2
33  *append([g9],a8,a5)* l1 2
34  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/a5,a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1:1:#f 2
35  collect(g2,a2) 0 #f
36  eq(a1,a2) 0 #f

37  collect(g8,a8) l1 2
38  append([],a8,a9) l1 2
39  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/[g9|a9],a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1:1:#f 2
40  collect(g2,a2) 0 #f
41  eq(a1,a2) 0 #f

%   !! generation is down again
%   collect(g8,a8) is only remaining descendant of collect(g5,a5)
%   make sure add1 only occurs under "multiple direct live bloodlines" condition!
%   also necessitates changes above!
42  collect(g8,a8) l1 2
43  *multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{g<1,1,1>/g6,a<1,1,1>/a6,a<1,1,2>/[g9|a8],a<1,1,3>/a3},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})* l1:1:#f 2
44  collect(g2,a2) 0 #f
45  eq(a1,a2) 0 #f

46  collect(g8,a8) l1 2
47  collect(g10,a10) l1 2
48  *append([g9|a8],a10,a11)* l1 2
49  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/a11},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1-1:1:#f 2
50  collect(g2,a2) 0 #f
51  eq(a1,a2) 0 #f

52  collect(g8,a8) l1 2
53  collect(g10,a10) l1 2
54  append(a8,a10,a12) l1 2
55  *multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a12]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})* l1-1:1:#f 2
56  collect(g2,a2) 0 #f
57  eq(a1,a2) 0 #f

58  collect(g8,a8) l1 2
59  collect(g10,a10) l1 2
60  append(a8,a10,a12) l1 2 
61  collect(g13,a13) l1-1 2
62  *append([g9|a12],a13,a14)* l1-1 2
63  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/a14},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1-2:1:#f 2
64  collect(g2,a2) 0 #f
65  eq(a1,a2) 0 #f

66  collect(g8,a8) l1 2
67  collect(g10,a10) l1 2
68  append(a8,a10,a12) l1 2
69  collect(g13,a13) l1-1 2
70  append(a12,a13,a15) l1-1 2
71  *multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a15]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1})* l1-2:1:#f 2
72  collect(g2,a2) 0 #f
73  eq(a1,a2) 0 #f

74  collect(g8,a8) l1 2
75  collect(g10,a10) l1 2
76  append(a8,a10,a12) l1 2
77  collect(g13,a13) l1-1 2
78  append(a12,a13,a15) l1-1 2
79  collect(g16,a16) l1-2 2
80  *append([g9|a15],a16,a17)* l1-2 2
81  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/a17},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1-3:1:#f 2
82  collect(g2,a2) 0 #f
83  eq(a1,a2) 0 #f

84  collect(g8,a8) l1 2
85  collect(g10,a10) l1 2
86  append(a8,a10,a12) l1 2
87  collect(g13,a13) l1-1 2
88  append(a12,a13,a15) l1-1 2
89  collect(g16,a16) l1-2 2
90  append(a15,a16,a18) l1-2 2
91  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a18]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1-3:1:#f 2
92  collect(g2,a2) 0 #f
93  eq(a1,a2) 0 #f

94  collect(g8,a8) l1 2
95  collect(g10,a10) l1 2
96  append(a8,a10,a12) l1 2
97  multi((collect(g<2,i,1>,a<2,i,1>),append(a<2,i,2>,a<2,i,1>,a<2,i,3>)),#f,{g<2,1,1>/g13,a<2,1,1>/a13,a<1,1,2>/a12,a<2,1,3>/a15},{a<2,i+1,2>/a<2,i,3>},{g<2,L,1>/g16,a<2,L,1>/a16,a<2,L,2>/a15,a<2,L,3>/a18}) l2:l1-2:#f 2
98  multi((collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)),#f,{a<1,1,2>/[g9|a18]},{a<1,i+1,2>/a<1,i,3>},{g<1,L,1>/g4,a<1,L,1>/a4,a<1,L,2>/a3,a<1,L,3>/a1}) l1-3:1:#f 2
99  collect(g2,a2) 0 #f
100 eq(a1,a2) 0 #f

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