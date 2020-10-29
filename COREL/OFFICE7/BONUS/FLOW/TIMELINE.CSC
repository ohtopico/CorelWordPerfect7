REM Time Line Wizard
REM TimeLine.csc

DECLARE SUB HaveFile()
DECLARE SUB CustomDesign()
DECLARE SUB DrawChart()
DECLARE SUB GetTimeFrame()
DECLARE SUB SetTime()
DECLARE SUB NewData()
DECLARE SUB EditData()
DECLARE SUB DeleteData()
DECLARE SUB SaveData()
DECLARE SUB GetData()
DECLARE SUB GetNewData()
DECLARE SUB SetTaskString()
DECLARE SUB SetColour(choice%)
DECLARE FUNCTION CHECKDATA() as integer

GLOBAL FlowUnits%
GLOBAL TaskName$(100)		'array of strings identifying the tasks
GLOBAL Duration$(100)		'array of strings with each task's duration
GLOBAL Dependencies$(100)	'array of strings with each task's dependencies
GLOBAL task%				'array index for tasks
GLOBAL Time$(100)			'array of strings indicating the time reference for each task
GLOBAL FoundError%			'error flag
GLOBAL GotFile%			'file error flag
GotFile=0					'file error flag initialized to no file
GLOBAL OutputFile$			'variable for output file name used in SaveData sub
GLOBAL Width#				'user input page width
GLOBAL Height#				'user input page height
GLOBAL LeftMarg#			'user input left margin
GLOBAL RightMarg#			'user input right margin
GLOBAL TopMarg#			'user input top margin
GLOBAL BottomMarg#			'user input bottom margin

GLOBAL red%				'red colour component for fills
GLOBAL green%				'green colour component for fills
GLOBAL blue%				'blue colour component for fills
GLOBAL headercolour%		'colour for the headers
headercolour%=17			'initialize headers to be grey
Global FolderName$			'the name of the current directory	

Errcount% = 0

Retry:

Errcount = ErrCount + 1
On error goto Retry
if (Errcount = 4) then
	Message("Internal error.  Please Re-run script.")
else
	Withobject FLOW
		FolderName$=.GetFlowDir()
	End WithObject
end if
On Error Exit

GLOBAL FontName$(20)
FontName(1)  = "AmeriGarmnd BT"
FontName(2)  = "Bodoni BT"
FontName(3)  = "Futura Hv BT"
FontName(4)  = "Lydian BT"
FontName(5)  = "News702 BT"
FontName(6)  = "Schneidler BT"
FontName(7)  = "Swis721 BT"
FontName(8)  = "Technical"
FontName(9)  = "TrumpetLite"
FontName(10) = "ZapfHumnst BT"
FontName(11) = "Arial"

GLOBAL Units$(3)
Units(1)= "Inches"
Units(2)= "Centimeters"
Units(3)= "Points"
'Units(4)= "Ciceros"
'Units(5)= "Didots"
'Units(6)= "Picas"

GLOBAL TimeFrame$(0 to 5)
TimeFrame(0)= "Hours"
TimeFrame(1)= "Days"
TimeFrame(2)= "Weeks"
TimeFrame(3)= "Bi-weekly"
TimeFrame(4)= "Months"
TimeFrame(5)= "Years"

GLOBAL Month$(100)
Month(1)= "January"
Month(2)= "February"
Month(3)= "March"
Month(4)= "April"
Month(5)= "May"
Month(6)= "June"
Month(7)= "July"
Month(8)= "August"
Month(9)= "September"
Month(10)= "October"
Month(11)= "November"
Month(12)= "December"

GLOBAL Day$(100)
Day(1)= "Sunday"
Day(2)= "Monday"
Day(3)= "Tuesday"
Day(4)= "Wednesday"
Day(5)= "Thursday"
Day(6)= "Friday"
Day(7)= "Saturday"

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

GLOBAL TaskBarSize$(3)
TaskBarSize(1)= "Large"
TaskBarSize(2)= "Medium"
TaskBarSize(3)= "Small"

GLOBAL TaskBarSpacing$(3)
TaskBarSpacing(1)= "Small"
TaskBarSpacing(2)= "Medium"
TaskBarSpacing(3)= "Large"

GLOBAL TimelineImage$(15)
TimeLineImage(1) = FolderName + "\TLP1.bmp"
TimeLineImage(2) = FolderName + "\TLP2.bmp"
TimeLineImage(3) = FolderName + "\TLP3.bmp"
TimeLineImage(4) = FolderName + "\TLP4.bmp"
TimeLineImage(5) = FolderName + "\TLP5.bmp"
TimeLineImage(6) = FolderName + "\TLP6.bmp"
TimeLineImage(7) = FolderName + "\TLP7.bmp"
TimeLineImage(8) = FolderName + "\TLP8.bmp"
TimeLineImage(9) = FolderName + "\TLP9.bmp"
TimeLineImage(10) = FolderName + "\TLP10.bmp"
TimeLineImage(11) = FolderName + "\TLP11.bmp"
TimeLineImage(12) = FolderName + "\TLP12.bmp"
TimeLineImage(13) = FolderName + "\TLP13.bmp"


'//////////////////////////////////////////////////////////////
BEGIN DIALOG IntroDialog 288, 192, "Time Line Wizard "
	IMAGE  2, 2, 130, 187, TimeLineImage(1)
	TEXT  140, 11, 136, 22, "Welcome to CorelFLOW's Time Line Wizard!"
	TEXT  140, 35, 136, 32, "This Wizard will help you make Time Lines with ease.  You can load data from a text file or work your way through the wizard."
	TEXT  140, 75, 136, 22, "Please press NEXT to begin!"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'Return value 4
	CANCELBUTTON  233, 171, 40, 12
 	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'///////////////////////////////////////////////////////////////
BEGIN DIALOG Intro2Dialog 288, 192, "Time Line Wizard "
	IMAGE  2, 2, 130, 187, TimeLineImage(2)
	OPTIONGROUP DataSelect%
		OPTIONBUTTON  144, 40, 130, 10, "&Through the Wizard"
		OPTIONBUTTON  144, 60, 130, 10, "&From an existing text file"
	TEXT  140, 11, 144, 20, "Select method to create your diagram:"
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Return value 3
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'Return value 4
	CANCELBUTTON  233, 171, 40, 12
 	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'/////////////////////////////////////////////////////////////////
REM File Selection Dialog
GLOBAL InputFile$		'name of the input file
'InputFile$= FolderName + "\Timeline.txt"	'initialize input file to current directory

BEGIN DIALOG FileSelectDialog 288, 192, "Time Line Wizard "
	IMAGE  2, 2, 130, 186, TimeLineImage(3)
	TEXT  140, 11, 136, 36, "Enter the name of the text file."
	TEXT  142, 38, 50, 8, "File Location :"
	TEXTBOX  142, 48, 139, 13, InputFile$
	PUSHBUTTON  241, 68, 40, 13, "B&rowse"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'Return value 4
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Return value 3
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'/////////////////////////////////////////////////////////////////
REM Specifications Dialog - NumTasks, TimePeriod, TimeFrame, Title
GLOBAL NumTasks%		'number of tasks
GLOBAL TimePeriod%		'time period(ie. days, weeks, mths, etc)
GLOBAL frame%			'index for time period array
GLOBAL ChartTitle$		'title of the chart
NumTasks%=10			'initialize number of tasks to 10
TimePeriod%=15			'initialize time period to 15
frame%=1				'initialize time frame to days
ChartTitle="Time Line 1"	

BEGIN DIALOG SpecsDialog 288, 192, "Time Line Wizard"	 'Number of tasks and title
	IMAGE  2, 2, 130, 186, TimeLineImage(4)
	GROUPBOX  136, 4, 147, 37, "Title"
	TEXTBOX  144, 19, 132, 13, ChartTitle$
	GROUPBOX  136, 96, 147, 55, "Ti&me Frame"
	TEXT  145, 53, 58, 8, "N&umber of Tasks:"
	SPINCONTROL  224, 51, 40, 13, NumTasks%
	TEXT  145, 72, 66, 8, "&Duration of Project:"
	SPINCONTROL  224, 71, 40, 13, TimePeriod%
	DDLISTBOX  144, 111, 132, 53, TimeFrame$, frame%
	PUSHBUTTON  146, 171, 40, 12, "< &Back "
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	PUSHBUTTON  218, 130, 56, 14, "&Customize "
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'//////////////////////////////////////////////////////////////////
REM Time specs dialog
GLOBAL StartTime$(100)	'array of strings of the starting times based on time frame
GLOBAL EndTime$(100)	'array of strings of the ending times based on time frame
GLOBAL Init%			'flag indicating a change of time frames
GLOBAL st%			'index for starting time
GLOBAL et%			'index for ending time
st%=2				'initialize start time to Monday
et%=1				'initialize end time to Sunday
Init=0				'initialize flag to no change

BEGIN DIALOG TimeDialog 288, 192, "Time Line Wizard"
	IMAGE  2, 2, 130, 186, TimeLineImage(5)
	TEXT  136, 12, 151, 19, "If you want the chart to start at a particular time this is where you can set it. "
	TEXT  146, 47, 39, 8, "&Start Time:"
	DDLISTBOX  192, 45, 84, 101, StartTime$, st%
	TEXT  146, 77, 38, 8, "&End Time:"
	DDLISTBOX  192, 76, 84, 101, EndTime$, et%
	OKBUTTON  233, 157, 40, 12
	CANCELBUTTON  233, 171, 40, 12
END DIALOG


'////////////////////////////////////////////////////////////////////
REM Edit dialog box

BEGIN DIALOG EditDialog 288, 192, "Time Line Wizard"	 'Instructions for using the Edit Boxes
	IMAGE  2, 2, 130, 186, TimeLineImage(6)
	TEXT  140, 12, 136, 51, "The Wizard will now provide you with a task list from which your Time Line will be generated."
	TEXT  140, 40, 136, 51, "On the next page you will have the option to create new tasks, edit existing ones and save the information to a file."
	TEXT  140, 75, 136, 51, "You may also specify dependencies, if you require a task to be completed before another task may be started."
	TEXT  140, 105, 136, 61, "If you prefer, you may use the information as provided and edit the diagram directly in FLOW."
	PUSHBUTTON  146, 171, 40, 12, "< &Back "
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'//////////////////////////////////////////////////////////////////
REM Customize Data Dialog - Edit, New, Delete, Save
GLOBAL TaskString$(100)	'array of strings of task name, duration and dependencies

BEGIN DIALOG CustomizeDataDialog 288, 192, "Time Line Wizard"
	IMAGE  2, 2, 280, 15, TimeLineImage(7)
	TEXT  11, 23, 189, 10, "Task Name \\  Duration \\ Dependencies"
	LISTBOX  8, 37, 230, 113, TaskString$, task%
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'returns 3
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'returns 4
	PUSHBUTTON  242, 37, 40, 12, "Ne&w"	 'returns 5
	PUSHBUTTON  242, 58, 40, 12, "&Edit"	 'returns 6
	PUSHBUTTON  242, 80, 40, 12, "&Delete"	 'returns 7
	PUSHBUTTON  242, 102, 40, 12, "&Save"	 'returns 8
	CANCELBUTTON  233, 171, 40, 12
	TEXT 135, 148, 106, 8,"Remember to save your changes."
 	GROUPBOX  3, 156, 278, 5,""
END DIALOG


'////////////////////////////////////////////////////////////////////
REM Data Entry Dialog -  NewTask, NewDuration, NewDependencies
GLOBAL NewTask$			'new task name for editing task list
GLOBAL NewDuration$			'new duration for editing task list
GLOBAL NewDependencies$		'new dependenciy for editing task list
GLOBAL TimeUnits$			'the selected time frame
NewTask=TaskName(1)			
NewDuration=Duration(1)
NewDependencies=Dependencies(1)

BEGIN DIALOG DataEntryDialog 288, 192, "Time Line Wizard"
	IMAGE  2, 2, 130, 186, TimeLineImage(8)
	TEXT  149, 15, 43, 8, "&Task Name:"
	TEXTBOX  148, 28, 122, 13, NewTask$
	TEXT  149, 49, 52, 10, "T&ask Duration:"
	TEXTBOX  148, 61, 56, 13, NewDuration$
	TEXT  208, 64, 38, 8, TimeUnits
	TEXT  149, 84, 49, 8, "&Dependencies:"
	DDCOMBOBOX  149, 97, 122, 94, TaskName$, NewDependencies$
	OKBUTTON  233, 157, 40, 12
	CANCELBUTTON  233, 171, 40, 12
END DIALOG


'//////////////////////////////////////////////////////////////////////
REM TaskBar Specifications Dialog - Style, SpecGap, ChartType, Colour, selectedcolour
GLOBAL ChartType%		'the type of chart (ie. horizontal or vertical)
GLOBAL TaskBarColour%	'colour of the task bars
GLOBAL tbsize%			'size of the task bars
GLOBAL tbspace%		'size of the spacing between task bars
TaskBarColour=11		'initialize task bar colour to dark purple
tbsize=1				'initialize task bar size to large
tbspace=1				'initialize task bar spacing to small

BEGIN DIALOG TaskBarSpecsDialog 288, 192, "Time Line Wizard"	 'Task bars Styles 
	IMAGE  2, 2, 130, 186, TimeLineImage(9)
	GROUPBOX  136, 7, 145, 44, "Style of Chart"
	OPTIONGROUP ChartType%
		OPTIONBUTTON  153, 18, 102, 15, "&Horizontal Task Bars"
		OPTIONBUTTON  153, 34, 110, 10, "&Vertical Task Bars"
	PUSHBUTTON  182, 139, 98, 12, "&Text Options"
	TEXT  137, 116, 39, 8, "Bar &Color:"
	DDLISTBOX  182, 114, 98, 37, Colour$, TaskBarColour%
	TEXT  138, 63, 32, 8, "Bar &Size:"
	DDLISTBOX  182, 60, 98, 40, TaskBarSize$, tbsize%
	TEXT  138, 88, 45, 8, "Bar &Spacing:"
	DDLISTBOX  182, 87, 98, 40, TaskBarSpacing$, tbspace%
	PUSHBUTTON  146, 171, 40, 12, "< &Back "
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'////////////////////////////////////////////////////////////////////
REM Text Options Dialog - FontName, BoldOptn, ItalicOptn, UnderlinedOptn, Justification
GLOBAL Justification%		'text justification (ie. left, center, right)
GLOBAL BoldOptn%			'text formating bold
GLOBAL ItalicOptn%			'text formating italic
GLOBAL UnderlineOptn%		'text formating underline
GLOBAL FontColour%			'text colour
GLOBAL selectedfont%		'index for font array
selectedfont%=8			'initialize selected font to Technical
Justification=0			'initialize justifuiaction to left
FontColour=3				'initialize font colour to dark blue

BEGIN DIALOG TextDialog 288, 192, "Time Line Wizard"
	IMAGE  2, 2, 130, 186, TimeLineImage(10)
	TEXT  140, 16, 19, 8, "&Font:"
	DDLISTBOX  170, 14, 102, 145, FontName$, selectedfont%
	TEXT  140, 38, 30, 11, "&Colour"
	DDLISTBOX  170, 36, 102, 115, Colour$, FontColour%
	GROUPBOX  143, 111, 138, 56, "Text Options"
	CHECKBOX  159, 124, 50, 10, "&Bold", BoldOptn%
	CHECKBOX  159, 137, 50, 10, "&Italic", ItalicOptn%
	CHECKBOX  159, 150, 50, 11, "&Underlined", UnderlineOptn%
	GROUPBOX  143, 55, 138, 51, "Justification"
	OPTIONGROUP Justification%
		OPTIONBUTTON  159, 67, 26, 10, "&Left"
		OPTIONBUTTON  159, 79, 46, 10, "&Center"
		OPTIONBUTTON  159, 91, 32, 10, "&Right"
	OKBUTTON  193, 171, 40, 12
	CANCELBUTTON  233, 171, 40, 12
END DIALOG


'////////////////////////////////////////////////////////////////////
REM Page Setup Dialog - PageOrientation, PageHeight, PageWidth, Units,
'                       Colour, PageColour
GLOBAL PageOrientation%		'page orientation(ie. portrait or landscape)
GLOBAL PageWidth#			'width of page for dialogs
GLOBAL PageHeight#			'height of page for dialogs
GLOBAL unts%				'page setup units (ie. picas, centimeters, inches, etc.)
GLOBAL PageColour%			'paper colour
PageColour=19				'initialize paper colour to mauve
PageWidth =8.5				'initialize page width to 21 centimeters (8.5 inches)
PageHeight =11				'initialize page height to 29 centimeters (11 inches)
unts=1					'initialize units to centimeters

BEGIN DIALOG PageDialog 288, 192, "Time Line Wizard"
	IMAGE  2, 2, 130, 186, TimeLineImage(11)
	GROUPBOX  138, 6, 142, 50, "Page Orientation"
	OPTIONGROUP PageOrientation%
		OPTIONBUTTON  154, 20, 63, 10, "&Landscape"
		OPTIONBUTTON  154, 36, 63, 10, "&Portrait"
	GROUPBOX  138, 59, 142, 93, "Page Size"
	TEXT  157, 90, 23, 8, "&Height:"
	TEXTBOX  188, 88, 44, 12, PageHeight
	TEXT  157, 71, 21, 8, "&Width:"
	TEXTBOX  188, 68, 44, 13, PageWidth
	TEXT  156, 112, 18, 8, "&Units:"
	DDLISTBOX  186, 110, 84, 70, Units$, unts
	TEXT  157, 133, 18, 8, "&Color:"
	DDLISTBOX  186, 131, 84, 58, Colour$, PageColour%
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'returns 3
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'returns 4
	PUSHBUTTON  224, 34, 40, 13, "&Margins"	 'returns 5
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


'////////////////////////////////////////////////////////////////////
REM Page Margins Dialog - LeftMargin, RightMargin, TopMargin, BottomMargin
GLOBAL LeftMargin#			'left margin size for dialogs
GLOBAL RightMargin#			'right margin size for dialogs
GLOBAL TopMargin#			'top margin size for dialogs
GLOBAL BottomMargin#		'bottom margin size for dialogs
LeftMargin=0.4				'initialize left margin to .4 inch
RightMargin=0.4				'initialize right margin to .4 inch
TopMargin=.4				'initialize top margin to .4 inch
BottomMargin=.4			'initialize bottom margin to .4 inch

BEGIN DIALOG PageMarginsDialog 288, 192, "Time Line Wizard"
	IMAGE  2, 2, 130, 186, TimeLineImage(12)
	GROUPBOX  138, 8, 140, 70, "Margins"
	TEXT  148, 25, 15, 8, "&Left:"
	TEXTBOX  170, 24, 28, 14, LeftMargin#
	TEXT  148, 51, 19, 8, "&Right:"
	TEXTBOX  170, 48, 28, 14, RightMargin#
	TEXT  217, 25, 16, 8, "&Top:"
	TEXTBOX  243, 24, 28, 14, TopMargin#
	TEXT  216, 51, 25, 8, "&Bottom:"
	TEXTBOX  243, 48, 28, 14, BottomMargin#
	OKBUTTON  233, 157, 40, 12
	CANCELBUTTON  233, 171, 40, 12
END DIALOG


'///////////////////////////////////////////////////////////////////////
REM Finish Dialog

BEGIN DIALOG FinishDialog 288, 192, "Time Line Wizard "
	IMAGE  2, 2, 130, 186, TimeLineImage(13)
	TEXT  140, 11, 142, 35, "We are now ready to make your Time Line diagram.  Press Finish and watch FLOW and SCRIPT do all the work!"
	TEXT  140, 50, 142, 35, "Just imagine all of the things that you can do with the combined power of FLOW3 and Corel SCRIPT."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Return value 3
	PUSHBUTTON  186, 171, 40, 12, "&Finish"	 'Return value 4
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG


Page1:
IntroReturn%=DIALOG(IntroDialog)
IF CANCEL THEN STOP
	
Page2:
Intro2Return%=DIALOG(Intro2Dialog)
IF CANCEL THEN STOP
IF Intro2Return%=3 THEN GOTO Page1 			'Back Button
IF DataSelect = 1 THEN						'File provided				
FS:	FileSelectReturn%=DIALOG(FileSelectDialog)
	IF CANCEL THEN STOP
	IF FileSelectReturn%=5 THEN GOTO Page2 				'Back Button
	IF FileSelectReturn%=4 AND GotFile=1 THEN GOTO Page3	'Next Button	
	IF FileSelectReturn%=4 AND GotFile=0 THEN 			'Next Button, but no file	
		HaveFile
		GOTO FS	
	ENDIF
	IF FileSelectReturn%=3 THEN HaveFile 				'Browse Button
	GOTO FS
ENDIF

Page3:
GetTimeFrame	
fr%=frame
SpecsReturn% = DIALOG(SpecsDialog)
IF CANCEL THEN STOP
IF SpecsReturn%=3 AND DataSelect=0 THEN GOTO Page2 	'Back Button
IF SpecsReturn%=3 AND DataSelect=1 THEN GOTO FS 		'Back Button
IF SpecsReturn%=4 THEN GOTO Page5 					'Next Button
IF SpecsReturn%=5 THEN 							'Customize Button
	IF fr<>frame THEN Init=0						'IF new time frame is chosen, re-init times
	GOTO Page4 					
ENDIF

Page4:
GetTimeFrame
TimeReturn% = DIALOG(TimeDialog)
IF CANCEL THEN GOTO Page3
Init=1
GOTO Page3


Page5:
'SetTaskString
IF DataSelect% = 0 THEN CustomDesign	
EditDialogReturn%=DIALOG(EditDialog)
IF CANCEL THEN STOP
IF EditDialogReturn%=3 THEN GOTO Page3 		'Back Button
IF EditDialogReturn%=4 THEN GOTO Page6		'Next Button
GOTO Page8


Page6:
SetTaskString
TimeUnits=TimeFrame(frame)
GetTimeFrame
CustomizeDataReturn%=DIALOG(CustomizeDataDialog)
IF CANCEL THEN STOP
IF CustomizeDataReturn%=3 THEN GOTO Page5 		'Back Button
IF CustomizeDataReturn%=4 THEN GOTO Page8 		'Next Button
IF CustomizeDataReturn%=5 THEN GOTO Page7new		'New Button
IF CustomizeDataReturn%=6 THEN GOTO Page7edit 	'Edit Button
IF CustomizeDataReturn%=7 THEN GOTO Page7delete 	'Delete Button
IF CustomizeDataReturn%=8 THEN GOTO Page7save 	'Save Button

Page7new:
FoundError=0
GetNewData
DataEntryReturn%=DIALOG(DataEntryDialog)
IF CANCEL THEN GOTO Page6
NewData
IF FoundError=0 THEN GOTO Page6
IF FoundError=1 THEN GOTO Page7new
GOTO Page6

Page7edit:
FoundError=0
GetData
IF FoundError=1 THEN GOTO Page6
DataEntryReturn%=DIALOG(DataEntryDialog)
IF CANCEL THEN GOTO Page6
EditData
IF FoundError=0 THEN GOTO Page6
IF FoundError=1 THEN GOTO Page7edit

Page7delete:
DeleteData
GOTO Page6

Page7save:
SaveData
GOTO Page6

Page8:
'check for the dependencies based on the existing task name
iRetError = CheckData()
if iRetError <> 0 then
	MESSAGE "Invalid dependencies... " + CHR(13) + "Take a look at " + TaskName(iRetError)
	goto Page6
end if

TaskBarSpecsReturn% = DIALOG(TaskBarSpecsDialog)
IF CANCEL THEN STOP
IF TaskBarSpecsReturn%=4 THEN GOTO Page6 	'Back Button
IF TaskBarSpecsReturn%=4 THEN GOTO Page5 	'Back Button
IF TaskBarSpecsReturn%=5 THEN GOTO Page10	'Next
IF TaskBarSpecsReturn%=3 THEN GOTO Page9	'Text Button

Page9:
TextReturn%=DIALOG(TextDialog)
IF CANCEL THEN GOTO Page8
GOTO Page8

Page10:
PageReturn%=DIALOG(PageDialog)
IF CANCEL THEN STOP
IF PageReturn%=3 THEN GOTO Page8 	'Back Button
IF PageReturn%=4 THEN GOTO Page12	'Next
IF PageReturn%=5 THEN GOTO Page11	'Margins Button

Page11:
PageMarginsReturn%=DIALOG(PageMarginsDialog)
IF CANCEL THEN GOTO Page10
GOTO Page10

Page12:
FinishReturn% = DIALOG(FinishDialog)
IF CANCEL THEN STOP
IF FinishReturn%=3 THEN GOTO Page10 	'Back Button

DrawChart



'**************************************************************************************
REM HaveFile - This subroutine reads the task data (task name, duration and dependency)
               'from a comma-delimited text file. Provides minimal error checking
SUB HaveFile
DIM AccumDur#(100)	'integer array of accumulted durations (ie. task duration + task's dependency's duration...)

	InputFile$ = GETFILEBOX("*.TXT", "OPEN", 0)

	IF InputFile="" THEN 		'unable to open input file
		InputFile=CURRFOLDER
		GotFile=0				'set file flag to failure	
		FoundError=1			'set error flag to found error
	ELSE						'file opened successfully
		TimePeriod=1			'initialize time period of 1
		task=1
		OPEN InputFile$ FOR INPUT AS 1

		DO UNTIL EOF(1)		'read the file
			INPUT #1, TaskName$(task%), Duration(task), Dependencies(task)	
			if taskname(task) <> "" and dependencies(task) = "" then 
				dependencies(task) = "none"
			end if

			RTRIM(TaskName$(task%))
			RTRIM(Duration(task))
			RTRIM(Dependencies(task))
			TaskName(task)=UCASE(TaskName(task))
			Dependencies(task)=UCASE(Dependencies(task))

			IF UCASE$(Dependencies(task))="NONE" THEN 	'task doesn't have any dependencies
				AccumDur(task)=CDBL(Duration(task))
			ELSE									'task has dependencies, assign AccumDur -1
				AccumDur(task)=-1
			ENDIF
			IF TaskName(task) <> "" THEN task=task+1
		LOOP			
		task=task-1
		NumTasks%=task	

		'check for dependency same as task name 
		FOR k%=1 TO NumTasks
			IF UCASE(TaskName(k))=UCASE(Dependencies(k)) THEN MESSAGE "ERROR!" + CHR(13) + "Task can't depend on itself"
		NEXT k

		'check for the dependencies based on the existing task name
		iRetError = CheckData()
		if iRetError <> 0 then
		MESSAGE "Invalid dependencies..." + CHR(13) + "Please fix it and re-run the Wizard!"
			stop
		end if

		'determine accumulated duration for each task
		DO 
			Flag=FALSE
			FOR k% = 1 TO NumTasks
				IF AccumDur(k) <> -1 THEN
					FOR x%=1 TO NumTasks
						IF UCASE(Dependencies(x)) = UCASE(TaskName(k)) THEN AccumDur(x)=CDBL(Duration(x))+CDBL(AccumDur(k))				
						IF TimePeriod<CDBL(AccumDur(x)) THEN TimePeriod=CDBL(AccumDur(x))
					NEXT x
				ELSEIF AccumDur(k) = -1 THEN
					Flag=TRUE			
				ENDIF
			NEXT k
		LOOP WHILE Flag=TRUE

		GotFile=1		'set file flag to success
		FoundError=0	'set error flag to no errors 
	ENDIF
	CLOSE(1)
END SUB



'*****************************************************************************************
REM Custom Design - This subroutine sets up the arrays for task name, duration and dependencies 
				'for a cutom designed chart, ie. one that does not read data from a file
SUB CustomDesign 
	FOR task%= 1 TO NumTasks
		TaskName(task)="TASK" + STR(task)		'name the tasks
		Duration(task)=1					'each task has a duration of 1
		IF task=1 THEN						'first task doesn't have any dependencies
			Dependencies(task)="NONE"
		ELSE								'all other tasks are dependent on the previous task
			Dependencies(task)=TaskName(task-1)
		ENDIF
		TaskName(task)=UCASE(TaskName(task))	
		Dependencies(task)=UCASE(Dependencies(task))
	NEXT task
END SUB


'*****************************************************************************************
REM SetTaskString - This subroutine creates the task string used for editing the data
				'Format of string: Task Name \\ Duration \\ Dependencies
SUB SetTaskString

	spacer$=SPACE(3) + "\\" + SPACE(3)		'initialize a spacer to separate the fields
	FOR k%= 1 TO NumTasks
		TaskString(k)= TaskName(k) + spacer + Duration(k)+ spacer + Dependencies(k)
	NEXT k
	TaskString(k)= ""
END SUB



'*****************************************************************************************
REM NewData - This subroutine adds new tasks to an existing task list checking for unique task names
SUB NewData
	FOR j%=1 TO NumTasks
		IF UCASE(NewDependencies) = UCASE(TaskName(j)) THEN PrevDur#=CDBL(Duration(j))
		IF UCASE(NewTask) = UCASE(TaskName(j)) THEN 
			MESSAGE "ERROR" + CHR(13) + "That task is already used"
			GOTO Invalid
		ENDIF
	NEXT j

	if NewTask <> "" and NewDependencies = "" then
		NewDependencies = "NONE"
	end if

	if NewDuration = "" then
		MESSAGE "Task has no duration... Please re-enter the data!"
		goto invalid
	end if

	IF (PrevDur + CDBL(NewDuration)) > TimePeriod THEN
		MESSAGE "ERROR!" + CHR(13) + "Dependency too long"
		FoundError=1
	ELSE
		NumTasks=NumTasks+1
		task=NumTasks
		TaskName(task)=UCASE(NewTask)
		Duration(task)=NewDuration
		Dependencies(task)=UCASE(NewDependencies)
	ENDIF
Invalid:
END SUB



'*****************************************************************************************
REM GetNewData - This subroutine clears the stings for new data to be entered
SUB GetNewData
	NewTask=""
	NewDuration=""
	NewDependencies=""
END SUB



'*****************************************************************************************
REM GetData - This subroutine sets up the editing window with the selected task name, duration and dependencies
			'If no task is selected, a message prompts the user to select a task before proceeding
SUB GetData
	IF task=0 THEN 		
		MESSAGE "Please select a task to edit"
		FoundError=1
	ELSE
		NewTask=TaskName(task)
		NewDuration=Duration(task)
		NewDependencies=Dependencies(task)
	ENDIF
END SUB



'*****************************************************************************************
REM EditData - This subroutine modifies the existing task list - names, durations  and  dependencies
			'Minimal error checking is providee
SUB EditData
DIM AccumDur#(100)		'array of accumulated task durations

	FOR j%=1 TO NumTasks
		IF UCASE(NewDependencies) = UCASE(TaskName(j)) THEN PrevDur#=CDBL(Duration(j))
	NEXT j

	if NewTask <> "" and NewDependencies = "" then
		NewDependencies = "NONE"
	end if

	IF UCASE(NewDependencies)=UCASE(TaskName(task)) THEN		'task dependency can't be the same as task name
		MESSAGE "ERROR!" + CHR(13) + "Task can't depend on itself"
		FoundError=1
	ELSEIF (NewDuration = "") THEN
		MESSAGE "ERROR!" + CHR(13) + "Task has no duration"
		FoundError=1
	ELSEIF (PrevDur + CDBL(NewDuration)) > TimePeriod THEN		'duration can't exceed time period
		MESSAGE "ERROR!" + CHR(13) + "Duration too long"
		FoundError=1
	ELSE												'enter new data in task list
		TaskName(task)=UCASE(NewTask)
		Duration(task)=UCASE(NewDuration)
		Dependencies(task)=UCASE(NewDependencies)
	ENDIF
END SUB



'*****************************************************************************************
REM DeleteData - This subroutine removes the selected task from the existing task list
SUB DeleteData

	indx%=task
	IF task=0 THEN 		'no task selected to delete
		MESSAGE "Please select a task to delete"
		FoundError=1
	ELSE					'delete the selected task
		FOR task%= indx TO NumTasks-1		'reorder the remaining tasks
			TaskName(task)=TaskName(task+1)
			Duration(task)=Duration(task+1)
			Dependencies(task)=Dependencies(task+1)
		NEXT task
		TaskName(task)=""		'empty deleted task strings
		Duration(task)=""
		Dependencies(task)=""
		NumTasks=NumTasks-1		'decrement number of tasks
	ENDIF
END SUB



'*****************************************************************************************
REM SaveData - This subroutine writes the existing task list to a comma-delimited text file
SUB SaveData 

 	OutputFile$ = GETFILEBOX("*.TXT", "SAVE", 1)
	IF OutputFile$ = "" THEN 					'unable to open output file
		MESSAGE "ERROR!" + CHR(13) + "Unable to save data"
		FoundError=1
	ELSE										'write data to output file
		OPEN OutputFile$ FOR OUTPUT AS 1
		FOR task%= 1 TO NumTasks
			RTRIM(TaskName$(task%))
			RTRIM(Duration(task))
			RTRIM(Dependencies(task))		
			WRITE #1, TaskName(task), Duration(task), Dependencies(task)
		NEXT task
	ENDIF
	CLOSE(1)
END SUB



'******************************************************************************
REM DrawChart - This subroutine creates the time line chart in CorelFLOW
SUB DrawChart

DIM TitleSpace#			
TitleSpace=1

	'determine the units to convert from

'FromUnits 'ToUnits
'1 = inches
'2 = centimeters
'3 = points
'4 = ciceros
'5 = didots
'6 = picas

'FlowUnits
'0 = pixels
'1 = picas, points
'2 = inches
'3 = centimeters
'4 = meters
'5 = ciceros, didots

Select case UNTS
	case 1
		FlowUnits = 2
	case 2
		FlowUnits = 3
	case 3 ', 6
		FlowUnits = 1
'	case 4, 5
'		FlowUnits = 5
end select

	'make inches the page setup units
	FromUnits% = unts
	ToUnits%=1
	PageWdth# = LENGTHCONVERT(FromUnits, ToUnits, PageWidth)
	PageHght# = LENGTHCONVERT(FromUnits, ToUnits, PageHeight)
	LeftMgn# = LENGTHCONVERT(FromUnits, ToUnits, LeftMargin)
	RightMgn# = LENGTHCONVERT(FromUnits, ToUnits, RightMargin)
	TopMgn# = LENGTHCONVERT(FromUnits, ToUnits, TopMargin)
	BottomMgn# = LENGTHCONVERT(FromUnits, ToUnits, BottomMargin)

	'make centimeters the drawing unit 
	ToUnits%=2
	Width# = LENGTHCONVERT(FromUnits, ToUnits, PageWidth)
	Height# =	LENGTHCONVERT(FromUnits, ToUnits, PageHeight)
	LeftMarg# = LENGTHCONVERT(FromUnits, ToUnits, LeftMargin)
	RightMarg# = LENGTHCONVERT(FromUnits, ToUnits, RightMargin)
	TopMarg# = LENGTHCONVERT(FromUnits, ToUnits, TopMargin)
   	BottomMarg# = LENGTHCONVERT(FromUnits, ToUnits, BottomMargin)

	'set multiplication factor for task bar sizing
	IF tbsize=3 THEN			'Small
		Width=Width*0.50
		Height=Height*0.50		
	ELSEIF tbsize=2 THEN		'Medium
		Width=Width*0.75
		Height=Height*0.75
	ELSE 					'Large
		Width=Width*1.0
		Height=Height*1.0
	ENDIF

	'set multiplication factor for task bar space sizing
	IF tbspace%=1 THEN			'Small
		Gap#=.50
	ELSEIF tbspace%=2 THEN		'Medium
		Gap#=.65
	ELSE 					'Large		
		Gap#=.80
	ENDIF

Errcount% = 0

Retry2:

Errcount = ErrCount + 1
On error goto Retry2
if (Errcount = 4) then
	Message("Internal error.  Please Re-run script.")
else
	WITHOBJECT FLOW
		FileID%=.FileNew()		'new file
		.SetRefresh FALSE		'turn refresh off to make it draw faster
		.SetUnits 2			'set drawing units to centimeters

		IF PageOrientation = 0 THEN  		'Landscape
			.SetPageLayout 0, PageHght, PageWdth, LeftMgn, RightMgn, TopMgn, BottomMgn	
			temp#=Width
			Width=Height
			Height=temp
		ELSE							'Portrait
			.SetPageLayout -1, PageWdth, PageHght, LeftMgn, RightMgn, TopMgn, BottomMgn
		ENDIF

		.SetUnits 3			'set drawing units to centimeters
		.SetRuler 3, 3

		'////////VERTICAL CHART///////////
		IF ChartType=1 THEN  				
			Header#=3
			TaskOffset#= 0.2*Height
			StartCol# = -(Width/2) + LeftMarg
			EndCol# =  (Width/2) - RightMarg
			TaskRow# = -(Height/2) + BottomMarg
			StartRow# = -(Height/2) + BottomMarg+TaskOffset
			EndRow# = (Height/2)-TopMarg-TitleSpace
			ColSize# = (Width-LeftMarg-RightMarg-(NumTasks*Gap)-Header)/NumTasks
			RowSize# = (Height-TopMarg-BottomMarg-TitleSpace-TaskOffset)/TimePeriod

			'create time schedule along left edge
			FontSize%=LENGTHCONVERT(2, 3, (0.3*RowSize))
			topedge# = StartRow
			FOR i%=1 TO TimePeriod
				loweredge# = (topedge + RowSize)
				.CreateRectangle StartCol, topedge, StartCol+Header, loweredge, Time(i)
				SetColour(headercolour)
				.ApplyUniformFillColor red, green, blue
				.SetObjectText Time(i)
				.FormatSelectedFont FontSize, FontName(selectedfont)
				.FormatTextJustify Justification
				SetColour(FontColour)
				.FormatTextColor red, green, blue
				IF BoldOptn=1 THEN .FormatTextBold 
				IF ItalicOptn=1 THEN .FormatTextItalic 
				IF UnderlineOptn=1 THEN .FormatTextUnderline 
				.GetObjectRectangle lft#, tp#, rght#, btm#, Time(i)
				.CreateLine rght#, tp#, EndCol, tp#, -1, -1
				.ApplyOutline 2
				.ArrangeToBack
				topedge=loweredge
			NEXT i%
	
			'create task names and task bars
			FontSize%=LENGTHCONVERT(2, 3, (0.3*ColSize))
			leftedge# =  StartCol+Header+(0.5*Gap)
			FOR i%=1 TO NumTasks
				StartCol=leftedge+ColSize
				rightedge# = (leftedge+ColSize-(0.5*Gap))	
				.CreateRectangle StartCol, TaskRow+ColSize+(0.5*Gap), StartCol+TaskOffset,TaskRow
				.SetObjectText TaskName(i)
				.FormatSelectedFont FontSize, FontName(selectedfont)
				.FormatTextJustify Justification
				SetColour(FontColour)
				.FormatTextColor red, green, blue
				IF BoldOptn=1 THEN .FormatTextBold 
				IF ItalicOptn=1 THEN .FormatTextItalic 
				IF UnderlineOptn=1 THEN .FormatTextUnderline 
				.MovePinPosition StartCol, TaskRow
				.RotateObject 90, 0
				SetColour(headercolour)
				.ApplyUniformFillColor red, green, blue

				'task bars starting point are based on its dependencies
				IF UCASE(Dependencies(i)) <> "NONE" THEN 		'task has dependencies
					.GetObjectRectangle lft#, tp#, rght#, btm#, Dependencies(i)
					.CreateRectangle leftedge, tp#, rightedge, (tp#+(CDBL(Duration(i))*RowSize) - (0.003 * RowSize)), TaskName(i)
				ELSE	'task does not have dependencies
					.CreateRectangle leftedge, StartRow, rightedge, (StartRow+(CDBL(Duration(i))*RowSize)), TaskName(i)
				ENDIF
				.SelectObject TaskName(i)		
				SetColour(TaskBarColour)
				.ApplyUniformFillColor red, green, blue
				leftedge=rightedge+Gap
			NEXT i%

			'create the title and centere at the top of the page 
			.CreateTextString ChartTitle$, 0, EndRow+TitleSpace
			.FormatSelectedFont 30, FontName(selectedfont)
			.FormatTextJustify 1
			SetColour(FontColour)
			.FormatTextColor red, green, blue


		'////////HORIZONTAL CHART///////////
		ELSE			
			TimeLength#=3
			TaskOffset#= 3
			TaskCol#  = -(Width/2)+LeftMarg
			StartCol# = -(Width/2)+LeftMarg+TaskOffset
			EndCol# =  (Width/2)-RightMarg
			ColSize# = (Width-LeftMarg-RightMarg-TaskOffset)/(TimePeriod+1)
			IF ColSize > 2.75 THEN ColSize=2.5
			TaskRowSize#=((Height-TopMarg-BottomMarg-TitleSpace-TimeLength-ColSize)/NumTasks) 
			RowSize# = ((Height-TopMarg-BottomMarg-TitleSpace-(NumTasks*Gap)-TimeLength-ColSize)/NumTasks) 
			StartRow# = (Height/2)-TopMarg-TitleSpace
			EndRow# = -(Height/2)+BottomMarg

			'create time schedule along top edge
			leftedge=StartCol
			FontSize=LENGTHCONVERT(2, 3, (0.3*ColSize))
			StartRow=StartRow
			FOR i%=1 TO TimePeriod
				rightedge# = (leftedge+TimeLength)	
				.CreateRectangle leftedge, StartRow, rightedge, StartRow-ColSize, Time(i)
				SetColour(headercolour)
				.ApplyUniformFillColor red, green, blue
				.SetObjectText Time(i)
				.FormatSelectedFont FontSize, FontName(selectedfont)
				.FormatTextJustify Justification	
				SetColour(FontColour)
				.FormatTextColor red, green, blue
				IF BoldOptn=1 THEN .FormatTextBold 
				IF ItalicOptn=1 THEN .FormatTextItalic 
				IF UnderlineOptn=1 THEN .FormatTextUnderline 
				.MovePinPosition rightedge, StartRow-ColSize
				.RotateObject 90, 0
				.GetObjectRectangle lft#, tp#, rght#, btm#, Time(i)
				.CreateLine rght#, btm#, rght#, EndRow#, -1, -1
				.ApplyOutline 2
				.ArrangeToBack
				IF i=1 THEN StartCol=lft
				leftedge=leftedge+ColSize
			NEXT i%

			'create task names and task bars
			topedge# = btm - (0.5*Gap)
			Tasktopedge# = btm
			FontSize=LENGTHCONVERT(2, 3, (0.3*TaskRowSize))
			FOR i%=1 TO NumTasks
				loweredge# = (topedge - RowSize-(0.5*Gap))
				Taskloweredge=Tasktopedge-TaskRowSize
				.CreateRectangle TaskCol, Tasktopedge, StartCol, Taskloweredge
				SetColour(headercolour)
				.ApplyUniformFillColor red, green, blue
				.SetObjectText TaskName(i)
				.FormatSelectedFont FontSize, FontName(selectedfont)  
				.FormatTextJustify Justification
				SetColour(FontColour)
				.FormatTextColor red, green, blue
				IF BoldOptn=1 THEN .FormatTextBold 
				IF ItalicOptn=1 THEN .FormatTextItalic 
				IF UnderlineOptn=1 THEN .FormatTextUnderline 

				'task bars starting point are based on its dependencies
				IF Dependencies(i) <> "NONE" THEN 				'task has dependencies
					.GetObjectRectangle lft#, tp#, rght#, btm#, Dependencies(i)
'					.CreateRectangle rght#, topedge, (rght+(CDBL(Duration(i))*ColSize)- (0.02 * ColSize)), (loweredge+ (0.5*Gap)), TaskName(i)
					.CreateRectangle rght#, topedge, (rght+(CDBL(Duration(i))*ColSize)- (0.002 * ColSize)), (loweredge+ (0.5*Gap)), TaskName(i)
				ELSE	'task does not have dependencies
					.CreateRectangle StartCol, topedge, (StartCol+(CDBL(Duration(i))*ColSize)), (loweredge+ (0.5*Gap)), TaskName(i)
				ENDIF
				.SelectObject TaskName(i)
				SetColour(TaskBarColour)
				.ApplyUniformFillColor red, green, blue
				topedge=loweredge-(0.5*Gap)
				Tasktopedge# = Taskloweredge
			NEXT i%

			.CreateTextString ChartTitle$, 0, StartRow
			.FormatSelectedFont 30, FontName(selectedfont)
			.FormatTextJustify 1
			SetColour(FontColour)
			.FormatTextColor red, green, blue
		ENDIF

		SetColour(PageColour)
		.SetPaperColor red, green, blue
		.ObjectSelectNone
		.SetUnits FlowUnits
		.SetRuler FlowUnits, FlowUnits

		FromUnits = 2
		ToUnits = UNTS

		Width# = LENGTHCONVERT(FromUnits, ToUnits, PageWidth)
		Height# =	LENGTHCONVERT(FromUnits, ToUnits, PageHeight)
		LeftMarg# = LENGTHCONVERT(FromUnits, ToUnits, LeftMargin)
		RightMarg# = LENGTHCONVERT(FromUnits, ToUnits, RightMargin)
		TopMarg# = LENGTHCONVERT(FromUnits, ToUnits, TopMargin)
	   	BottomMarg# = LENGTHCONVERT(FromUnits, ToUnits, BottomMargin)

		IF PageOrientation = 0 THEN  		'Landscape
			.SetPageLayout 0, PageHght, PageWdth, LeftMgn, RightMgn, TopMgn, BottomMgn	
		ELSE							'Portrait
			.SetPageLayout -1, PageWdth, PageHght, LeftMgn, RightMgn, TopMgn, BottomMgn
		ENDIF

		.SetRefresh TRUE
		.WindowActivate
	END WITHOBJECT
end if
On Error Exit
END SUB



'********************************************************************************************
REM GetTimeFrame - This subroutine sets the starting and ending times as well as the Time array 
			    'for the the specifed time frame
SUB GetTimeFrame	

	SELECT CASE (frame)
		CASE 0							'hours
			FOR i= 1 TO 24
				StartTime(i)=STR(i)+":00"
				EndTime(i)=STR(i)+":00"
			NEXT i
			IF Init=0 THEN 
				StartTime(i)=""
				EndTime(i)=""
				st=12 : et=24
			ENDIF
			hr%=st
			FOR t= 1 TO TimePeriod
				Time(t)= StartTime(hr)
				hr=hr+1 
			NEXT t

		CASE 1							'days
			FOR i= 1 TO 7
				StartTime(i)=Day(i)
				EndTime(i)=Day(i)
			NEXT i
			IF Init=0 THEN 
				StartTime(i)=""
				EndTime(i)=""
				st=1 : et=7
			ENDIF
			dy%=st
			FOR t= 1 TO TimePeriod
				IF dy > 7 THEN
					dy=dy-7
				ENDIF
				Time(t)= Day(dy)
				dy=dy+1
			NEXT t

		CASE 2							'weeks
			FOR i= 1 TO TimePeriod
				StartTime(i)="Week"+STR(i)
				EndTime(i)="Week"+STR(i)
			NEXT i
			IF Init=0 THEN 
				StartTime(i)=""
				EndTime(i)=""
				st=1 : et=TimePeriod
			ENDIF
			wk%=st
			FOR t= 1 TO TimePeriod
				Time(t)= StartTime(wk)
				wk=wk+1 
			NEXT t
	
		CASE 3							'bi-weekly
			FOR i= 1 TO TimePeriod 
				StartTime(i)="Week"+STR(i)
				EndTime(i)="Week"+STR(i)
			NEXT i
			IF Init=0 THEN 
				StartTime(i)=""
				EndTime(i)=""
				st=1 : et=TimePeriod
			ENDIF

			wk%=st
			FOR t= 1 TO TimePeriod
				Time(t)= StartTime(wk)
				wk=wk+1 
			NEXT t

		CASE 4							'monthly
			FOR i= 1 TO TimePeriod
				StartTime(i)= "Jan" : EndTime(i)= "Jan" : i=i+1
				StartTime(i)= "Feb" : EndTime(i)= "Feb" : i=i+1
				StartTime(i)= "Mar" : EndTime(i)= "Mar" : i=i+1
				StartTime(i)= "Apr" : EndTime(i)= "Apr" : i=i+1
				StartTime(i)= "May" : EndTime(i)= "May" : i=i+1
				StartTime(i)= "Jun" : EndTime(i)= "Jun" : i=i+1
				StartTime(i)= "Jul" : EndTime(i)= "Jul" : i=i+1
				StartTime(i)= "Aug" : EndTime(i)= "Aug" : i=i+1
				StartTime(i)= "Sep" : EndTime(i)= "Sep" : i=i+1
				StartTime(i)= "Oct" : EndTime(i)= "Oct" : i=i+1
				StartTime(i)= "Nov" : EndTime(i)= "Nov" : i=i+1
				StartTime(i)= "Dec" : EndTime(i)= "Dec" 
			NEXT i
			mth%=st
			FOR t= 1 TO TimePeriod
				IF mth > 12 THEN
					mth=mth-12
				ENDIF
				Time(t)= Month(mth)
				mth=mth+1
			NEXT t
			IF Init=0 THEN 
				StartTime(i)=""
				EndTime(i)=""
				st=1 : et=12
			ENDIF

		CASE 5							'yearly
			FOR i= 1 TO 99
				StartTime(i)=STR(1969 + i)
				EndTime(i)=STR(1969 + i)
			NEXT i
			IF Init=0 THEN 
				StartTime(i)=""
				EndTime(i)=""
				st=26 : et=st+TimePeriod
			ENDIF
			yr%=st
			FOR t=1 TO TimePeriod
				Time(t)= StartTime(yr)
				yr=yr+1
			NEXT t

	END SELECT
END SUB


'***************************************************************************
REM SetColour - This subroutine assigns the RGB colour components for selecting fill colours
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
		red=240
		green=240
		blue=240

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


function CheckData as integer
	dim s as integer
	dim t as integer

	for s = 1 to NumTasks
		for t = 1 to NumTasks
			if ucase(Dependencies(s)) <> "NONE" then
				if UCASE(Dependencies(s)) = UCASE(TaskName(t)) then goto ExitFor
				if t = NumTasks then 
					CheckData = s
					goto ExitFunction
				end if
			end if
		next
		ExitFor:
	next
	CheckData = 0
	EXITFUNCTION:
end function 'CheckData
