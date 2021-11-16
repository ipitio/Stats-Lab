# Q1

DATA sportscars;
INFILE 'U:\home\ams394\hw\7\cars.txt'; INPUT Model $ Year Make $
Seats Color $;
IF Year < 1975 THEN Status = 'classic';
IF Model = 'Corvette' OR Model = 'Camaro' THEN Make = 'Chevy';
IF Model = 'Miata' THEN DO;
Make = 'Mazda';
Seats = 2;
END;
PROC PRINT DATA = sportscars;
TITLE "Eddy's Excellent Emporium of Used Sports Cars";
RUN;

# Q2

DATA homeimprovements;
INPUT Owner $ 1-7 Description $ 9-33 Cost;
IF Cost = . THEN CostGroup = 'missing';
ELSE IF Cost < 3000 THEN CostGroup = 'low';
ELSE IF Cost < 9000 THEN CostGroup = 'medium';
ELSE CostGroup = 'high';

DATALINES;
Bob kitchen cabinet face-lift 1253.00
Shirley bathroom addition 11350.70
Silvia paint exterior .
Al backyard gazebo 3098.63
Norm paint interior 647.77
Kathy second floor addition 75362.93
;

PROC PRINT DATA = homeimprovements;
TITLE 'Home Improvement Cost Groups';
RUN;

# Q3

DATA comedy;
INPUT Title $ 1-26 Year Type $;
IF Type = 'tragedy' OR Type = 'romance' OR Type = 'history' THEN DELETE;
IF Type = 'tragedy' OR Type = 'romance' THEN DELETE;
IF Type = 'tragedy';

DATALINES;
A Midsummer Night’s Dream 1595 comedy
Comedy of Errors 1590 comedy
Hamlet 1600 tragedy
Macbeth 1606 tragedy
Richard III 1594 history
Romeo and Juliet 1596 tragedy
Taming of the Shrew 1593 comedy 
Tempest 1611 romance
;

PROC PRINT DATA = comedy;
TITLE 'Shakespearean Comedies';
RUN;

# Q4

DATA Students
INPUT Name $ Exam1 Exam2 Exam3;
Average = (Exam1 + Exam2 +Exam3)/3;

DATALINES;
Joe 75 86 90
Mary 88 88 97
Jim 65 75 100
Jane 74 98 76
Mike 89 88 90
Sue 30 80 60
;

PROC SORT DATA = Students;
BY DESCENDING Average;
RUN;
PROC PRINT DATA = Students;
TITLE 'Exam Ranking'
RUN;