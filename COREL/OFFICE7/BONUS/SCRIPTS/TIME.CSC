' Convert Date and time to any format
' Time.csc August 8, 1995
' This script shows how to get different formats of
' dates and times using DLL calls
'#########################################################################

' DLL functions
'Parts of can easily be pasted into other scripts to format dates in those scripts.
DECLARE FUNCTION SCRIPTDATETODOSDATE LIB "oleaut32" ( BYVAL time AS DATE, BYREF dosdate AS INTEGER, BYREF dostime AS INTEGER) AS LONG ALIAS "VariantTimeToDosDateTime"
DECLARE FUNCTION DOSDATETOFILETIME LIB "kernel32" ( BYVAL dosdate AS INTEGER, BYVAL dostime AS INTEGER, BYREF FILETIME AS STRING) AS INTEGER ALIAS "DosDateTimeToFileTime"
DECLARE FUNCTION FILETIMETOSYSTEMTIME LIB "kernel32" ( BYREF filetime AS STRING, BYREF systemtime AS STRING) AS INTEGER ALIAS "FileTimeToSystemTime"
DECLARE FUNCTION GETDATEFORMAT LIB "kernel32" (BYVAL Locale AS LONG, BYVAL Flags AS LONG, BYREF datetime AS STRING, BYREF format AS STRING, datestr as STRING, BYVAL bufsize AS LONG ) AS LONG ALIAS "GetDateFormatA"
DECLARE FUNCTION GETTIMEFORMAT LIB "kernel32" (BYVAL Locale AS LONG, BYVAL Flags AS LONG, BYREF datetime AS STRING, BYREF format AS STRING, timestr as STRING, BYVAL bufsize AS LONG ) AS LONG ALIAS "GetTimeFormatA"

' Local functions
DECLARE FUNCTION formatdate ( BYVAL datetime AS DATE, BYVAL dateformat AS STRING, BYVAL timeformat AS STRING ) AS STRING


' Constants and globals
CONST OK=1
CONST NUMTIMEFORMAT = 5
CONST NUMDATEFORMAT = 11
CONST NUMFORMAT = NUMTIMEFORMAT * NUMDATEFORMAT

' Main local
DIM DateFormat(NUMFORMAT) AS STRING
DIM TimeFormat(NUMFORMAT) AS STRING
DIM FormatList(NUMFORMAT) AS STRING
DIM DateChoice AS DATE


' Fill date format array
FOR i = 1 TO NUMTIMEFORMAT
	DateFormat( i + NUMTIMEFORMAT * 0 ) = "dddd',' MMMM dd yyyy"	
	DateFormat( i + NUMTIMEFORMAT * 1 ) = "ddd',' MMM d y"	
	DateFormat( i + NUMTIMEFORMAT * 2 ) = "dd'/'MM'/'yy"
	DateFormat( i + NUMTIMEFORMAT * 3 ) = "d'/'M'/'y"
	DateFormat( i + NUMTIMEFORMAT * 4 ) = "MM'/'dd'/'yy"
	DateFormat( i + NUMTIMEFORMAT * 5 ) = "M'/'d'/'y"
	DateFormat( i + NUMTIMEFORMAT * 6 ) = "yyyy, MMMM dd"
	DateFormat( i + NUMTIMEFORMAT * 7 ) = "yyyy'/'MMM'/'dd"
	DateFormat( i + NUMTIMEFORMAT * 8 ) = "yy'/'MM'/'dd"
	DateFormat( i + NUMTIMEFORMAT * 9 ) = "y'/'M'/'d"
	DateFormat( i + NUMTIMEFORMAT * 10) = ""
NEXT i

' Fill time format array
FOR i = 1 TO NUMFORMAT STEP NUMTIMEFORMAT
	TimeFormat( i     ) = "HH':'mm':'ss"
	TimeFormat( i + 1 ) = "H':'m':'s"
	TimeFormat( i + 2 ) = "hh':'mm':'ss tt"
	TimeFormat( i + 3 ) = "h':'m':'s t"
	TimeFormat( i + 4 ) = ""
NEXT i

' Fill dialog list
FOR i = 1 TO NUMFORMAT
	FormatList(i) = formatdate( CURRDATE, DateFormat(i), TimeFormat(i) )	
NEXT i

' Initialize dialog
UserDatetime$= CURRDATE
FormatListValue%=1

' Definition of our format selection dialog
BEGIN DIALOG datetimedlg 257, 172, "Date and Time conversions"
	OKBUTTON  193, 14, 56, 16
	CANCELBUTTON  193, 37, 56, 16
	TEXT  8, 87, 101, 9, "Desired Format:"
	LISTBOX  8, 97, 175, 80, FormatList$,FormatListValue%
	GROUPBOX  8, 7, 175, 71, "Date and Time"
	OPTIONGROUP TypeofDate%
		OPTIONBUTTON  18, 23, 89, 10, "Current"
		OPTIONBUTTON  18, 40, 58, 10, "User defined"
	TEXTBOX  32, 53, 141, 16, UserDatetime$
END DIALOG

' Only convert if we pressed OK
IF DIALOG(datetimedlg)=OK THEN
	IF TypeofDate=0 THEN
		DateChoice = CURRDATE
	ELSE
		DateChoice = UserDatetime
	END IF
	messagebox formatdate( DateChoice, DateFormat(FormatListValue), TimeFormat(FormatListValue) ), "Selected Date and Time"
END IF


'#########################################################################
'# Format DATE var to specied date and time format
FUNCTION formatdate( datetime, dateformat, timeformat )
	formatteddate$=space(255)
	formattedtime$=space(255)
	filetime$ = space(20)
	systemtime$ = space(20)
	SCRIPTDATETODOSDATE datetime, dosdate%, dostime% 
	DOSDATETOFILETIME dosdate%, dostime%, filetime$
	FILETIMETOSYSTEMTIME filetime$, systemtime$
	GETDATEFORMAT 0, 0, systemtime$, dateformat, formatteddate$, 254
	GETTIMEFORMAT 0, 0, systemtime$, timeformat$, formattedtime$, 254
	IF formatteddate$<>"" then
		formatdate = formatteddate$+" "+formattedtime$	
	ELSE
		formatdate = formattedtime$
	END IF
END FUNCTION
