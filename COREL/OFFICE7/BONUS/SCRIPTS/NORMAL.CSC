REM Extracts day and year from a date variable.
REM Normal.csc 12 July, 1995

REM This script takes the number value of a date variable, and extracts the year, month, day, hour, minute, seconds and weekday.
REM It basically counts years from 1900 until it gets to the right year, then it counts months - etc.
REM Changing the Testdate variable will change the day being normalized

DECLARE SUB normalizedate(BYVAL d AS DATE, BYREF i%, BYREF count&, BYREF year&)  'normalizedate for extracting month, day, year
DECLARE SUB normalizetime(BYVAL td AS DATE, BYREF h%, BYREF m%, BYREF s%)  'normalizedate for extracting month, day, year
DECLARE SUB getweekday(BYVAL wkd AS DATE, BYREF wkday%)

GLOBAL daysinmonth%(12)		'Global Array TO hold the days in each month.


REM This is the date that gets passed
DIM testdate as date		'The date being translated
testdate = 34890.879			'the date being translated

DIM month$(12), weekday$(7)  'arrays to hold values
DIM info%, days&, yr&		'Variables for passing be reference

month(1) = "January"		'stores the strings for the months
month(2) = "February"
month(3) = "March"
month(4) = "April"
month(5) = "May"
month(6) = "June"
month(7) = "July"
month(8) = "August"
month(9) = "September"
month(10) = "October"
month(11) = "November"
month(12) = "December"

weekday(1) = "Monday"		'stores the strings for the weekday
weekday(2) = "Tuesday"
weekday(3) = "Wednesday"
weekday(4) = "Thursday"
weekday(5) = "Friday"
weekday(6) = "Saturday"
weekday(7) = "Sunday"

daysinmonth(1) = 31			'stores the number of days in each month
daysinmonth(2) = 28			'febuary leap years taken into account later
daysinmonth(3) = 31
daysinmonth(4) = 30
daysinmonth(5) = 31
daysinmonth(6) = 30
daysinmonth(7) = 31
daysinmonth(8) = 31
daysinmonth(9) = 30
daysinmonth(10) = 31
daysinmonth(11) = 30
daysinmonth(12) = 31


normalizedate testdate, info, days, yr		'calls the translate function
normalizetime testdate, hour%, minute%, second%
getweekday testdate, wday%

Hourstring = CSTR(hour)
minutestring = CSTR(minute)
Secondstring = CSTR(second)

IF LEN(Hourstring) = 1 THEN Hourstring = "0" + Hourstring
IF LEN(Minutestring) = 1 THEN Minutestring = "0" + Minutestring
IF LEN(Secondstring) = 1 THEN Secondstring = "0" + Secondstring

MESSAGE "The Time works out to be " + Hourstring + ":" + Minutestring + ":" + Secondstring
MESSAGE "The Test date works out to be " + month(info) + STR(days) + "," + STR(yr)
MESSAGE "The Test Date is a " + weekday(wday)

SUB normalizedate(d AS DATE, i%, count&, year&)			'translate SUB
			'notice that values are changed by reference
	DIM b AS BOOLEAN, leapyear AS BOOLEAN, numday%		'local variables
	count& = d - 1     'temporary counter variable (-1 because dates count from dec. 31, 1899)
	year = 1900		'initialise the year counter to 1900 (date 2)
	numday = 365		'initiase number of days in a year
	leapyear = false	'boolean for whether or not a leap year

	IF count <= numday THEN 		'if count < numbers of days in this year, end loop
		b = true	
	END if

	DO WHILE b = false		'determines the year, by subtracting numday from count, and incrementing year
		count = count - numday
		year = year + 1
		numday = 365		'reinitialise numday
		leapyear = false	'reinitialise leapyear

		IF (year MOD 4 = 0 AND year MOD 100 <> 0) OR year MOD 400 = 0 THEN 		'if leap year
			numday = 366			'set numday and leapyear
			leapyear = true
		END IF
		IF count <= numday THEN 		'if count < numbers of days in this year, end loop
			b = true	
		END IF
	LOOP
i% = 1		'initialise index
b = false		'reset loop variable
IF leapyear THEN		'set days in month for feb.
	daysinmonth(2) = 29
END IF

	DO WHILE b = false	'determines the month, again by counting.
		IF count <= daysinmonth(i) THEN 
			b = true
			EXIT DO
		END IF
		count = count - daysinmonth(i)
	i = i + 1
	LOOP
END SUB

SUB normalizetime(td AS DATE, h%, m%, s%)
	CONST ONEHOUR# = 1.0 /24
	CONST ONEMINUTE# = 1.0/(24*60)
	CONST ONESECOND# = 1.0/(24*60*60)
	time1& = td - 0.5
	td = td - time1		'remove the integer part of d
	h% = (td / onehour) - 0.5
	td = (td - h * onehour)
	m% = (td / oneminute) - 0.5
	td = td - m * oneminute
	s% = (td /onesecond) - 0.5
END SUB

SUB getweekday(wkd AS DATE, wkday%)
	CONST offset = 6
	weektemp& = wkd - 0.5 + offset
	wkday = (weektemp MOD 7) + 1
END SUB
