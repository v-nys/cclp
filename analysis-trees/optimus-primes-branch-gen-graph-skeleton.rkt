#lang cclp/ggs
NODES
1   oprimes(g1,a1)
  
2   integers(g2,a2)
3   siftA(a2,a3)
4   siftB(a3,a1)
5   length(a1,g1)

6   integers(g3,a4)
7   siftA([g2|a4],a3)
8   siftB(a3,a1)
9   length(a1,g1)

10  integers(g3,a4)
11  filterA(g2,a4,a5)
12  siftA(a5,a6)
13  siftB([g2|a6],a1)
14  length(a1,g1)

15  integers(g3,a4)
16  filterA(g2,a4,a5)
17  siftA(a5,a6)
18  filterB(g2,a6,a7)
19  siftB(a7,a8)
20  length([g2|a8],g1)

21  integers(g3,a4)
22  filterA(g2,a4,a5)
23  siftA(a5,a6)
24  filterB(g2,a6,a7)
25  siftB(a7,a8)
26  length(a8,g4)

27  integers(g5,a9)
28  filterA(g2,[g3|a9],a5)
29  siftA(a5,a6)
30  filterB(g2,a6,a7)
31  siftB(a7,a8)
32  length(a8,g4)

33  integers(g5,a9)
34  filterA(g2,a9,a10)
35  siftA([g3|a10],a6)
36  filterB(g2,a6,a7)
37  siftB(a7,a8)
38  length(a8,g4)

39  integers(g5,a9)
40  filterA(g2,a9,a10)
41  filterA(g3,a10,a11)
42  siftA(a11,a12)
43  filterB(g2,[g3|a12],a7)
45  siftB(a7,a8)
46  length(a8,g4)

47  integers(g5,a9)
48  filterA(g2,a9,a10)
49  filterA(g3,a10,a11)
50  siftA(a11,a12)
51  filterB(g2,a12,a13)
52  siftB([g3|a13],a8)
53  length(a8,g4)

54  integers(g5,a9)
55  filterA(g2,a9,a10)
56  filterA(g3,a10,a11)
57  siftA(a11,a12)
58  filterB(g2,a12,a13)
59  filterB(g3,a13,a14)
60  siftB(a14,a15)
61  length([g3|a15],g4)

62  integers(g5,a9)
63  filterA(g2,a9,a10)
64  filterA(g3,a10,a11)
65  siftA(a11,a12)
66  filterB(g2,a12,a13)
67  filterB(g3,a13,a14)
68  siftB(a14,a15)
69  length(a15,g6)

70  integers(g7,a16)
71  filterA(g2,[g5|a16],a10)
72  filterA(g3,a10,a11)
73  siftA(a11,a12)
74  filterB(g2,a12,a13)
75  filterB(g3,a13,a14)
76  siftB(a14,a15)
77  length(a15,g6)

78  integers(g7,a16)
79  filterA(g2,a16,a17)
80  filterA(g3,[g5|a17],a11)
81  siftA(a11,a12)
82  filterB(g2,a12,a13)
83  filterB(g3,a13,a14)
84  siftB(a14,a15)
85  length(a15,g6)

86  integers(g7,a16)
87  filterA(g2,a16,a17)
88  filterA(g3,a17,a18)
89  siftA([g5|a18],a12)
90  filterB(g2,a12,a13)
91  filterB(g3,a13,a14)
92  siftB(a14,a15)
93  length(a15,g6)

94  integers(g7,a16)
95  filterA(g2,a16,a17)
96  filterA(g3,a17,a18)
97  filterA(g5,a18,a19)
98  siftA(a19,a20)
99  filterB(g2,[g5|a20],a13)
100 filterB(g3,a13,a14)
101 siftB(a14,a15)
102 length(a15,g6)

103 integers(g7,a16)
104 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18})
105 filterA(g5,a18,a19)
106 siftA(a19,a20)
107 filterB(g2,[g5|a20],a13)
108 filterB(g3,a13,a14)
109 siftB(a14,a15)
110 length(a15,g6)

111 integers(g7,a16)
112 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18})
113 filterA(g5,a18,a19)
114 siftA(a19,a20)
115 filterB(g2,a20,a21)
116 filterB(g3,[g5|a21],a14)
117 siftB(a14,a15)
118 length(a15,g6)

119 integers(g7,a16)
120 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18})
121 filterA(g5,a18,a19)
122 siftA(a19,a20)
123 filterB(g2,a20,a21)
124 filterB(g3,a21,a22)
125 siftB([g5|a22],a15)
126 length(a15,g6)

127 integers(g7,a16)
128 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18})
129 filterA(g5,a18,a19)
130 siftA(a19,a20)
131 filterB(g2,a20,a21)
132 filterB(g3,a21,a22)
133 filterB(g5,a22,a23)
134 siftB(a23,a24)
135 length([g5|a24],g6)

136 integers(g7,a16)
137 multi((filterA(g<1,i,1>,a<1,i,1>,a<1,i,2>)),#t,{g<1,1,1>/g2,a<1,1,1>/a16,a<1,1,2>/a17},{a<1,i+1,1>/a<1,i,2>},{g<1,L,1>/g3,a<1,L,1>/a17,a<1,L,2>/a18})
138 filterA(g5,a18,a19)
139 siftA(a19,a20)
140 multi((filterB(g<2,i,1>,a<2,i,1>,a<2,i,2>)),#t,{g<2,1,1>/g2,a<2,1,1>/a20,a<2,1,2>/a21},{a<2,i+1,1>/a<2,i,2>},{g<2,L,1>/g3,a<2,L,1>/a21,a<1,L,2>/a22})
141 filterB(g5,a22,a23)
142 siftB(a23,a24)
143 length([g5|a24],g6)

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
37 -> 45.
38 -> 46.

39 -> 47.
40 -> 48.
41 -> 49.
42 -> 50.
43 -> 51.
45 -> 52.
46 -> 53.

47 -> 54.
48 -> 55.
49 -> 56.
50 -> 57.
51 -> 58.
52 -> 59, 60.
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
88 -> 96.
89 -> 97, 98.
90 -> 99.
91 -> 100.
92 -> 101.
93 -> 102.

94 -> 103.
95 -> 104.
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
124 -> 132.
125 -> 133, 134.
126 -> 135.

127 -> 136.
128 -> 137.
129 -> 138.
130 -> 139.
131 -> 140.
132 -> 140.
133 -> 141.
134 -> 142.
135 -> 143.