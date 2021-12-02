10 REM Day 1 solution
20 open "Day1.txt" for input as #1
30 input#1, x 
40 input#1, y
50 if x < y then c = c + 1
60 x = y
70 if not eof(1) goto 40
80 print "Increases = ";c
