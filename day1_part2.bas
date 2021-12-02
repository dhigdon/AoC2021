10 REM Day 1 solution, Windowing version
20 dim w(3): open "Day1.txt" for input as #1
30 input#1,w(1),w(2),w(3)
40 s=w(1)+w(2)+w(3)
50 w(1)=w(2) : w(2)=w(3) : input#1,w(3)
60 n=w(1)+w(2)+w(3)
70 if s < n then c = c + 1
80 s = n
90 if not eof(1) then 50
100 close#1: print "increase:",c
