#lang cclp/gg
NODES
1   *oprimes(g1,a1)* 0 #f
  
2   *integers(g2,a2)* 0 #f
3   siftA(a2,a3) 0 #f
4   siftB(a3,a1) 0 #f
5   length(a1,g1) 0 #f

6   integers(g3,a4) 0 #f
7   *siftA([g2|a4],a3)* 0 #f
8   siftB(a3,a1) 0 #f
9   length(a1,g1) 0 #f

10  integers(g3,a4) 0 #f
11  filterA(g2,a4,a5) 1 7
12  siftA(a5,a6) 1 7
13  *siftB([g2|a6],a1)* 0 #f
14  length(a1,g1) 0 #f

15  integers(g3,a4) 0 #f
16  filterA(g2,a4,a5) 1 7
17  siftA(a5,a6) 1 7
18  filterB(g2,a6,a7) 1 13
19  siftB(a7,a8) 1 13
20  *length([g2|a8],g1)* 0 #f

21  *integers(g3,a4)* 0 #f
22  filterA(g2,a4,a5) 1 7
23  siftA(a5,a6) 1 7
24  filterB(g2,a6,a7) 1 13
25  siftB(a7,a8) 1 13
26  length(a8,g4) 0 #f

27  integers(g5,a9) 0 #f
28  *filterA(g2,[g3|a9],a5)* 1 7
29  siftA(a5,a6) 1 7
30  filterB(g2,a6,a7) 1 13
31  siftB(a7,a8) 1 13
32  length(a8,g4) 0 #f

33  integers(g5,a9) 0 #f
34  filterA(g2,a9,a10) 1 7
35  *siftA([g3|a10],a6)* 1 7
36  filterB(g2,a6,a7) 1 13
37  siftB(a7,a8) 1 13
38  length(a8,g4) 0 #f

39  integers(g5,a9) 0 #f
40  filterA(g2,a9,a10) 1 7
41  filterA(g3,a10,a11) 2 7
42  siftA(a11,a12) 2 7
43  *filterB(g2,[g3|a12],a7)* 1 13
44  siftB(a7,a8) 1 13
45  length(a8,g4) 0 #f

46  integers(g5,a9) 0 #f
47  filterA(g2,a9,a10) 1 7
48  filterA(g3,a10,a11) 2 7
49  siftA(a11,a12) 2 7
50  filterB(g2,a12,a13) 1 13
51  *siftB([g3|a13],a8)* 1 13
52  length(a8,g4) 0 #f

53  integers(g5,a9) 0 #f
54  filterA(g2,a9,a10) 1 7
55  filterA(g3,a10,a11) 2 7
56  siftA(a11,a12) 2 7
57  filterB(g2,a12,a13) 1 13
58  filterB(g3,a13,a14) 2 13
59  siftB(a14,a15) 2 13
60  *length([g3|a15],g4)* 0 #f

61  *integers(g5,a9)* 0 #f
62  filterA(g2,a9,a10) 1 7
63  filterA(g3,a10,a11) 2 7
64  siftA(a11,a12) 2 7
65  filterB(g2,a12,a13) 1 13
66  filterB(g3,a13,a14) 2 13
67  siftB(a14,a15) 2 13
68  length(a15,g6) 0 #f

69  integers(g7,a16) 0 #f
70  *filterA(g2,[g5|a16],a10)* 1 7
71  filterA(g3,a10,a11) 2 7
72  siftA(a11,a12) 2 7
73  filterB(g2,a12,a13) 1 13
74  filterB(g3,a13,a14) 2 13
75  siftB(a14,a15) 2 13
76  length(a15,g6) 0 #f

77  integers(g7,a16) 0 #f
78  filterA(g2,a16,a17) 1 7
79  *filterA(g3,[g5|a17],a11)* 2 7
80  siftA(a11,a12) 2 7
81  filterB(g2,a12,a13) 1 13
82  filterB(g3,a13,a14) 2 13
83  siftB(a14,a15) 2 13
84  length(a15,g6) 0 #f

85  integers(g7,a16) 0 #f
86  filterA(g2,a16,a17) 1 7
87  filterA(g3,a17,a18) 2 7
88  *siftA([g5|a18],a12)* 2 7
89  filterB(g2,a12,a13) 1 13
90  filterB(g3,a13,a14) 2 13
91  siftB(a14,a15) 2 13
92  length(a15,g6) 0 #f

93  integers(g7,a16) 0 #f
94  filterA(g2,a16,a17) 1 7
95  filterA(g3,a17,a18) 2 7
96  filterA(g5,a18,a19) 3 7
97  siftA(a19,a20) 3 7
98  filterB(g2,[g5|a20],a13) 1 13
99  filterB(g3,a13,a14) 2 13
100 siftB(a14,a15) 2 13
101 length(a15,g6) 0 #f

102 integers(g7,a16) 0 #f
103 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}) 1:l1:#t 7
104 filterA(g5,a18,a19) l1+1 7
105 siftA(a19,a20) l1+1 7
106 *filterB(g2,[g5|a20],a13)* 1 13
107 filterB(g3,a13,a14) 2 13
108 siftB(a14,a15) 2 13
109 length(a15,g6) 0 #f

110 integers(g7,a16) 0 #f
111 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}) 1:l1:#t 7
112 filterA(g5,a18,a19) l1+1 7
113 siftA(a19,a20) l1+1 7
114 filterB(g2,a20,a21) 1 13
115 *filterB(g3,[g5|a21],a14)* 2 13
116 siftB(a14,a15) 2 13
117 length(a15,g6) 0 #f

118 integers(g7,a16) 0 #f
119 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}) 1:l1:#t 7
120 filterA(g5,a18,a19) l1+1 7
121 siftA(a19,a20) l1+1 7
122 filterB(g2,a20,a21) 1 13
123 filterB(g3,a21,a22) 2 13
124 *siftB([g5|a22],a15)* 2 13
125 length(a15,g6) 0 #f

126 integers(g7,a16) 0 #f
127 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}) 1:l1:#t 7
128 filterA(g5,a18,a19) l1+1 7
129 siftA(a19,a20) l1+1 7
130 filterB(g2,a20,a21) 1 13
131 filterB(g3,a21,a22) 2 13
132 filterB(g5,a22,a23) 3 13
133 siftB(a23,a24) 3 13
134 length([g5|a24],g6) 0 #f

135 integers(g7,a16) 0 #f
136 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18}) 1:l1:#t 7
137 filterA(g5,a18,a19) l1+1 7
138 siftA(a19,a20) l1+1 7
139 multi((filterB(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a20,a<2,1,2>/a21},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g3,a<2,L,1>/a21,a<1,L,2>/a22}) 1:l2:#t 13
140 filterB(g5,a22,a23) l2+1 13
141 siftB(a23,a24) l2+1 13
142 length([g5|a24],g6) 0 #f

EDGES
1 -> 2, 3, 4, 5.

2 -> 6.
3 -> 7.
4 -> 8.
5 -> 9.

6 -> 10.
7 -> 11, 12.
8 -> 13.
9 -> 14.

10 -> 15.
11 -> 16.
12 -> 17.
13 -> 18, 19.
14 -> 20.

15 -> 21.
16 -> 22.
17 -> 23.
18 -> 24.
19 -> 25.
20 -> 26.

21 -> 27.
22 -> 28.
23 -> 29.
24 -> 30.
25 -> 31.
26 -> 32.

27 -> 33.
28 -> 34.
29 -> 35.
30 -> 36.
31 -> 37.
32 -> 38.

33 -> 39.
34 -> 40.
35 -> 41, 42.
36 -> 43.
37 -> 44.
38 -> 45.

39 -> 46.
40 -> 47.
41 -> 48.
42 -> 49.
43 -> 50.
44 -> 51.
45 -> 52.

46 -> 53.
47 -> 54.
48 -> 55.
49 -> 56.
50 -> 57.
51 -> 58, 59.
52 -> 60.

53 -> 61.
54 -> 62.
55 -> 63.
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
71 -> 79.
72 -> 80.
73 -> 81.
74 -> 82.
75 -> 83.
76 -> 84.

77 -> 85.
78 -> 86.
79 -> 87.
80 -> 88.
81 -> 89.
82 -> 90.
83 -> 91.
84 -> 92.

85 -> 93.
86 -> 94.
87 -> 95.
88 -> 96, 97.
89 -> 98.
90 -> 99.
91 -> 100.
92 -> 101.

93 -> 102.
94 -> 103.
95 -> 103.
96 -> 104.
97 -> 105.
98 -> 106.
99 -> 107.
100 -> 108.
101 -> 109.

102 -> 110.
103 -> 111.
104 -> 112.
105 -> 113.
106 -> 114.
107 -> 115.
108 -> 116.
109 -> 117.

110 -> 118.
111 -> 119.
112 -> 120.
113 -> 121.
114 -> 122.
115 -> 123.
116 -> 124.
117 -> 125.

118 -> 126.
119 -> 127.
120 -> 128.
121 -> 129.
122 -> 130.
123 -> 131.
124 -> 132, 133.
125 -> 134.

126 -> 135.
127 -> 136.
128 -> 137.
129 -> 138.
130 -> 139.
131 -> 139.
132 -> 140.
133 -> 141.
134 -> 142.