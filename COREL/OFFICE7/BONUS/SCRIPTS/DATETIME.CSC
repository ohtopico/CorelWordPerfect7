REM This script displays the current date and time
REM DateTime.csc July 21, 1995

DIM Now AS DATE, Day AS DATE, Time AS DATE
Now = CURRDATE

REM Day is the integer part of the current Date, Time is the fraction part
Day = INT(Now)
Time = Now - Day

MESSAGEBOX "The Date is" + STR(Day) + CHR(13) + CHR(10) + "The Time is" + STR(Time), "Current Time", 64


