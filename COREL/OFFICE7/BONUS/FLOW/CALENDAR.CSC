REM Calendar Wizard
REM Calendar.csc

DECLARE SUB GetWeekDay (BYVAL Wkd as DATE, BYREF WkDay%)
DECLARE SUB SetColour(choice%)

DIM FirstDay as DATE

GLOBAL red%
GLOBAL green%
GLOBAL blue%

DIM Weekday$(8)
Weekday(1) = "Sun"	
Weekday(2) = "Mon"
Weekday(3) = "Tues"
Weekday(4) = "Wed"
Weekday(5) = "Thur"
Weekday(6) = "Fri"
Weekday(7) = "Sat"
Weekday(8) = "Sun"		

DIM Month$(12)
Month(1) = "January"		
Month(2) = "February"
Month(3) = "March"
Month(4) = "April"
Month(5) = "May"
Month(6) = "June"
Month(7) = "July"
Month(8) = "August"
Month(9) = "September"
Month(10) = "October"
Month(11) = "November"
Month(12) = "December"

DIM DaysInMonth%(12) 
DaysInMonth(1) = 31			
DaysInMonth(2) = 28		
DaysInMonth(3) = 31
DaysInMonth(4) = 30
DaysInMonth(5) = 31
DaysInMonth(6) = 30
DaysInMonth(7) = 31
DaysInMonth(8) = 31
DaysInMonth(9) = 30
DaysInMonth(10) = 31
DaysInMonth(11) = 30
DaysInMonth(12) = 31

GLOBAL Colour$(21)
Colour(1)="White"
Colour(2)="Black"
Colour(3)="Dark blue"
Colour(4)="Blue"
Colour(5)="Dark green"
Colour(6)="Med green"
Colour(7)="Lime green"
Colour(8)="Moss green"
Colour(9)="Rust"
Colour(10)="Red"
Colour(11)="Dark purple"
Colour(12)="Rose"
Colour(13)="Orange"
Colour(14)="Yellow"
Colour(15)="Pale yellow"
Colour(16)="Pale blue"
Colour(17)="Grey"
Colour(18)="Purple"
Colour(19)="Mauve"
Colour(20)="Med blue"
Colour(21)="Light blue"
Errcount% = 0
DIM FlowDir$

Retry:
Errcount = ErrCount + 1
ON ERROR GOTO Retry
IF (Errcount = 4) THEN
	MESSAGE("Internal error.  Please Re-run script.")
ELSE
	WITHOBJECT FLOW
		FlowDir$=.GetFlowDir()
	END WITHOBJECT
ENDIF
ON ERROR EXIT

CalImage1$=FlowDir + "\Cal1.bmp"
CalImage2$=FlowDir + "\Cal2.bmp"
CalImage3$=FlowDir + "\Cal3.bmp"
CalImage4$=FlowDir + "\Cal4.bmp"
CalImage5$=FlowDir + "\Cal5.bmp"
CalImage6$=FlowDir + "\Cp1.bmp"
CalImage7$=FlowDir + "\Cp2.bmp"
CalImage8$=FlowDir + "\Cp3.bmp"
CalImage9$=FlowDir + "\Cp4.bmp"


'//////////////////////////////////////////////////////////////
BEGIN DIALOG IntroDialog 288, 192, "Calendar Wizard "
	IMAGE  2, 2, 130, 187, CalImage6$
	IMAGE  25, 45, 100, 130, CalImage4$
	TEXT  140, 11, 136, 22, "Welcome to CorelFLOWs Calendar Wizard!"
	TEXT  140, 35, 136, 32, "This Wizard will help you create personalized calenders for any month of any year!"
	TEXT  140, 75, 136, 22, "Please press NEXT to begin!"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'Return value 3
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'//////////////////////////////////////////////////////////////
dim sTodayDate as string
dim sLocateDate as string
dim iLocateDate as integer
dim strMonth as string
dim iLocateMonth as integer

sTodayDate = CURRDATE

iLocateDate = instr(sTodayDate, " ", 1)
sTodayDate = mid$(sTodayDate, 1, iLocateDate - 1)
lenTodayDate = len(sTodayDate)

'extract month
iLocateMonth = instr(sTodayDate, "/", 1)
strMonth = mid$(sTOdayDate, 1, iLocateMonth - 1)

'extract year
iLocateYear = instr(sTodayDate, "/", iLocateMonth + 1)
strYear = mid$(sTodayDate, iLocateYear + 1)

if len(strYear) = 2 then
	strYear = "19" + strYear
end if

Year& = strYear
mth = val(strMonth)

FontColour%=3	'dark blue
BoxColour%=17	'grey
PageColour%=20	'med blue

BEGIN DIALOG SpecsDialog 288, 192, "Calendar Wizard "
	IMAGE  2, 2, 130, 186, CalImage7$
	IMAGE  25, 45, 100, 130, CalImage2$
	TEXT  150, 11, 32, 9, "&Month:"
	DDLISTBOX  178, 9, 90, 50, Month$, mth
	TEXT  150, 33, 26, 12, "&Year:"
	SPINCONTROL  177, 32, 40, 12, Year&
	GROUPBOX  148, 60, 129, 81, "Color Options"
	TEXT  155, 80, 30, 10, "&Font:"
	DDLISTBOX  189, 79, 82, 46, Colour$, FontColour%
	TEXT  155, 100, 30, 10, "&Box:"
	DDLISTBOX  189, 99, 82, 46, Colour$, BoxColour%
	TEXT  155, 120, 30, 10, "&Page:"
	DDLISTBOX  189, 119, 82, 46, Colour$, PageColour%
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Return value 
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'Return value 
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

	
'//////////////////////////////////////////////////////////////
GLOBAL ImageName$
ImageName$=CalImage4$
NoImage%=0

BEGIN DIALOG BitmapDialog 288, 192, "Calendar Wizard "
	IMAGE  2, 2, 130, 186, CalImage8$
	IMAGE  25, 45, 100, 130, CalImage1$
	GROUPBOX  142, 4, 136, 99, "Image Preview"
	IMAGE  153, 20, 115, 72, ImageName$
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'Returns 3
	TEXTBOX  142, 108, 136, 13, ImageName$
	CHECKBOX  142, 140, 50, 13, "No &Image", NoImage%
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Returns 4
	PUSHBUTTON  238, 126, 40, 12, "B&rowse"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'///////////////////////////////////////////////////////////////////////
REM Finish Dialog
BEGIN DIALOG FinishDialog 288, 192, "Calendar Wizard "
	IMAGE  2, 2, 130, 187, CalImage9$
	IMAGE  25, 45, 100, 130, ImageName$
	TEXT  140, 11, 142, 35, "We are now ready to make your Calendar. Press FINISH and watch FLOW and SCRIPT  do all the work!"
	TEXT  140, 50, 142, 35, "Just imagine all of the things that you can with the combined power of FLOW3 and Corel SCRIPT."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Return value 3
	PUSHBUTTON  186, 171, 40, 12, "&Finish"	 'Return value 4
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


Page1:
IntroReturn%=DIALOG(IntroDialog)
IF CANCEL THEN STOP
	
Page2:
SpecsReturn%=DIALOG(SpecsDialog)
IF CANCEL THEN STOP
IF SpecsReturn%=3 THEN GOTO Page1	'Back
IF SpecsReturn%=4 THEN GOTO Page3	'Next

Page3:
BitmapReturn%=DIALOG(BitmapDialog)
IF CANCEL THEN STOP
IF BitmapReturn%=4 THEN GOTO Page2	'Back
IF BitmapReturn%=3 THEN GOTO Page4	'Next
IF BitmapReturn%=5 THEN GOTO Browse	'Browse

Browse:
CurrFolder = FlowDir
ImageName=GETFILEBOX("*.BMP", "Choose your favorite Bitmap")

IF ImageName="" THEN ImageName=CalImage4
GOTO Page3

Page4:
FinishReturn% = DIALOG(FinishDialog)
IF CANCEL THEN STOP
IF FinishReturn%=3 THEN GOTO Page3


REM Change DaysInMonth for February if leap year selected
IF (Year MOD 4 = 0 AND Year MOD 100 <> 0) OR Year MOD 400 = 0 THEN 		'if leap year
	DaysInMonth(2) = 29
END IF

REM Determine the day of the week for the first day of month
FirstDay = "1 " + Month(mth) + " " + STR(Year)	'Assemble first day of that month into a date variable
GetWeekDay FirstDay, Offby%


REM Create the calendar

Errcount = 0
Retry2:
Errcount = ErrCount + 1
ON ERROR GOTO Retry2
IF (Errcount = 4) THEN
	MESSAGE("Internal error.  Please Re-run script.")
ELSE
	WITHOBJECT FLOW

	.SetRefresh FALSE
	.FileNew
	.SetUnits 2, FileID%
	.SetPageLayout -1, 8.5, 11.0, 0, 0, 0, 0
	
	REM Create the month name
	.CreateTextString Month(mth), 0, 1.0
	.FormatSelectedFont 40, "Technical"
	.FormatTextBold
	.FormatTextJustify 1
	
	REM Create the year
	.CreateTextString STR(Year), 0, -5.25
	.FormatSelectedFont 40, "Technical"
	.FormatTextBold
	.FormatTextJustify 1
	
	REM Create the days of the week
	FOR loopvar% = 1 TO 7
		Col% = FIX(loopvar MOD 7)+ 1
		leftedge# = (-4.45 + (col * 1))
		topedge# = (0.75)
		.CreateRectangle  leftedge, topedge, leftedge+(0.8), topedge-(0.4)
		.SetObjectText Weekday(loopvar+1)
		.FormatSelectedFont 18, "Technical"
		.FormatTextJustify 1	
		.FormatTextBold
	NEXT loopvar
	
	REM Create the boxes for each day with their corresponding numbers
	FOR i% = 1 to DaysInMonth(mth)
		IF OFFBY=6 OR OFFBY=7 THEN
			Row% = FIX((OFFBY +i)/7)' + 1
		ELSE
			Row% = FIX((OFFBY +i)/7) + 1
		ENDIF
		Col% = FIX((OFFBY +i) MOD 7)+ 1
		leftedge# = (-4.45 + (col * 1))
		topedge# = (1 - (row * 0.9))
		.CreateRectangle  leftedge, topedge, leftedge+(0.8), topedge-(0.7)
		SetColour(BoxColour)
		.ApplyUniformFillColor red, green, blue
		.CreateTextString STR(i), leftedge+(0.8), topedge-(0.7)
		.MovePinPosition leftedge+(0.8), topedge-(0.7)

		IF i<10 THEN 
			.ObjectMove leftedge+(0.5), topedge-(0.5)
		ELSEIF i>9 AND i<20 THEN
			.ObjectMove leftedge+(0.4), topedge-(0.5)
		ELSE
			.ObjectMove leftedge+(0.3), topedge-(0.5)
		ENDIF
		.FormatSelectedFont 30, "Technical"
		SetColour(FontColour)
		.FormatTextColor red, green, blue
		.FormatTextBold
	NEXT i
	
	REM Set the paper colour
	SetColour(PageColour)
	.CreateRectangle -4.25, 5.5, 4.25, -5.5
	.ApplyUniformFillColor red, green, blue
	.ArrangeToBack
	
	REM Import the Bitmap
	IF NoImage = 0 THEN
		.FileImport ImageName
		.ObjectResize 9, 7.5, 3.7
		.ObjectMove 0, 3.4
	END IF
	
	.ObjectSelectNone
	.WindowActivate
	.SetRefresh TRUE

	END WITHOBJECT
ENDIF
ON ERROR EXIT

'*********************************Getting Weekday************************************************************
REM GetWeekday - This subroutine determines which day of the week a given date falls on
REM
SUB GetWeekDay(Wkd as DATE, WkDay%)
	CONST Offset = 4
	WeekTemp& = INT(Wkd) + Offset
	WkDay = (WeekTemp MOD 7) + 1
END SUB


'***********************************************************************************************************
REM SetColour - This subroutine sets the red, green and blue colour components for the selected colour
REM 
SUB SetColour(choice%)

	SELECT CASE(choice)
		CASE 1		'white	
			red=255
			green=255
			blue=255
	
		CASE 2		'black
			red=0
			green=0
			blue=0
	
		CASE 3		'dark blue
			red=51
			green=102
			blue=153
	
		CASE 4		'blue
			red=0
			green=153
			blue=255
	
		CASE 5		'dark green
			red=51
			green=102
			blue=51
	
		CASE 6		'med green
			red=51
			green=153
			blue=0
	
		CASE 7		'lime green
			red=51
			green=255
			blue=51
	
		CASE 8		'moss green
			red=51
			green=204
			blue=51
	
		CASE 9		'rust
			red=204
			green=0
			blue=51
	
		CASE 10		'red
			red=255
			green=0
			blue=0
	
		CASE 11		'dark purple
			red=204
			green=51
			blue=255
	
		CASE 12		'rose
			red=255
			green=153
			blue=204
	
		CASE 13		'orange
			red=255
			green=153
			blue=51
	
		CASE 14		'yellow
			red=255
			green=255
			blue=0
	
		CASE 15		'pale yellow
			red=255
			green=255
			blue=204
	
		CASE 16		'pale blue
			red=204
			green=255
			blue=255
	
		CASE 17		'grey
			red=235
			green=235
			blue=235
	
		CASE 18		'purple
			red=255
			green=0
			blue=255
	
		CASE 19		'mauve
			red=204
			green=204
			blue=255
	
		CASE 20		'med blue
			red=0
			green=204
			blue=255
	
		CASE 21		'light blue
			red=0
			green=255
			blue=255
	END SELECT
	
END SUB
