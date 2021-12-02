10 REM Day 2
20 OPEN "day2.txt" FOR INPUT AS #1
30 LINE INPUT #1, l$: GOSUB 100
40 IF C$="forward" THEN p=p+v
50 IF c$="down" THEN d=d+v
60 IF c$="up" THEN d=d-v
70 IF NOT EOF(1) THEN 30
80 PRINT "Area=";p*d
90 END
100 REM split input line
110 i=INSTR(l$," ")
120 C$=LEFT$(l$,i-1)
130 V=VAL(MID$(l$,i+1))
140 RETURN
