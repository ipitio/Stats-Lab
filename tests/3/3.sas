/* 1 */

/* 1 */
DATA class;
INPUT  Name $ Sex $ Date_of_birth DATE9. Height Weight;
IF Date_of_birth < '01JAN2001'd;
DATALINES;
Alfred M 25OCT2001 72.0 122.5
Alice F 06APR2000 63.5 112.0
Carol F 14MAY1998 62.8 99.5
James M 21MAR2000 66.3 99.5
Jane F 02DEC2000 61.8 94.5
Janet F 15JUL1999 68.5 118.5
;
PROC PRINT DATA = class;
FORMAT Date_of_birth MMDDYY8.;
TITLE 'class data';
RUN;
/* 2 */
PROC PRINT DATA = class;
WHERE Weight < 100;
FORMAT Date_of_birth MMDDYY8.;
TITLE 'class data where weight'
RUN;
/* 3 */
DATA class_all;
INPUT  Name $ Sex $ Date_of_birth DATE9. Height Weight;
IF Date_of_birth < 01Jan2001;
DATALINES;
Alfred M 25OCT2001 72.0 122.5
Alice F 06APR2000 63.5 112.0
Carol F 14MAY1998 62.8 99.5
James M 21MAR2000 66.3 99.5
Jane F 02DEC2000 61.8 94.5
Janet F 15JUL1999 68.5 118.5
;
PROC SORT;
    DESCENDING Weight Name;
PROC PRINT DATA = class_all;
TITLE 'class data desc weight';
RUN;
/* 4 */
PROC MEANS NOPRINT DATA = class_all; 
    BY Sex; 
    VAR Height Weight; 
    OUTPUT OUT = newclass MEAN(Height Weight) = mheight mweight
        SUM(Height Weight) = mheight mweight;
PROC PRINT DATA = newclass; 
TITLE 'new class data'; 
FORMAT mheight mweight 2. ; 
RUN ; 


/* 2 */

/* 1 */
DATA samples;
INPUT rating @@;
diff = rating - 87; 
DATALINES;
87.5 86.9 86.6 87.3 87.9 88.0 86.7 87.5 87.2 87.0 88.1 87.5 86.5 87.7 88.0 87.1 87.0 87.6 87.5 88.3
;
PROC UNIVARIATE DATA = samples Normal; 
VAR diff;
RUN;
/* 2 */
PROC TTEST DATA = samples SIDES = U ALPHA = 0.1;
VAR diff; 
RUN;

/* 3 */

DATA tobacco;
input leaf virus1 virus2;
diff=virus1-virus2;
DATALINES;
1 31 18
2 20 17
3 18 14
4 17 11
5 09 10
6 08 07
7 10 05
8 07 06
9 11 09
;
PROC UNIVARIATE data = tobacco normal;
VAR diff;
RUN;

/* 4 */

DATA speeds;
INPUT cody smith; 
Diff = cody - smith; 
DATALINES;
500 355
450 388
505 440
404 600
555 510
567 501
588 502
577 489
566 499
644 489
;
PROC UNIVARIATE DATA = speeds ALPHA = 0.01 NORMAL; 
VAR Diff;
RUN;
