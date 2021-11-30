/* 1 */

DATA anova;
INPUT brand $ weartime @@;
DATALINES;
A 8 A 10 A 9 A 11 A 10 A 10 A 8 A 12
N 4 N 7 N 5 N 5 N 6 N 7 N 6 N 4
T 12 T 8 T 10 T 10 T 11 T 9 T 9 T 12
;
PROC ANOVA DATA=anova;
CLASS brand;
MODEL weartime=brand;
MEANS brand/SNK;
RUN;

/* 2 */

DATA cholesterol;
INPUT treat $ values @@;
DATALINES;
A 220 A 190 A 180 A 185 A 210 A 170 A 178 A 200 A 177 A 189
B 160 B 168 B 178 B 200 B 172 B 155 B 159 B 167 B 185 B 199
P 240 P 220 P 246 P 244 P 198 P 238 P 277 P 255 P 190 P 188
;
PROC ANOVA DATA=cholesterol;
CLASS treat;
MODEL values=treat;
MEANS treat/tukey;
RUN;

/* 3 */

DATA df;
INPUT Age $ count brand;
DATALINES;
New 67 w
New 72 w
New 74 w
New 82 w
New 81 w
old 46 w
old 44 w
old 45 w
old 51 w
old 43 w
New 75 p
New 76 p
New 80 p
New 72 p
New 73 p
old 63 p
old 62 p
old 66 p
old 62 p
old 60 p
;
DATA df;
   DO brand = 'w','p';
      DO age = 'n','o';
         DO cell = 1 TO 5;
            INPUT count @;
            OUTPUT;
        END;
      END;
   END;

DATALINES;
67 72 74 82 81 46 44 45 51 43 75 76 80 72 73 63 62 66 62 60
;
PROC ANOVA DATA=df;
CLASS brand age;
MODEL count = brand|age;
MEANS brand|age;
RUN;