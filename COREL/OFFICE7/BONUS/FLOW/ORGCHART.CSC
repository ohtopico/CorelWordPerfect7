REM Organization Chart Wizard
REM OrgChart.csc

REM Declares

Declare SUB AddToReportToCBArr(RepName$)
Declare SUB DelFromReportToCBArr(RepToName$)
Declare FUNCTION TextToText% (FileName$, Num%)
Declare SUB TransformOrgChart(NumRec%)

Declare FUNCTION PageAndFinish% ()
Declare Function CheckDescription% ()

Declare SUB Stevesub ()
Declare SUB SetColorRGB(col%)

DECLARE SUB ConnectObjects()
DECLARE FUNCTION FindFirstUnSetChild(iNode%) as integer
DECLARE FUNCTION FindLeftUnSet(iNode%) as integer
DECLARE FUNCTION GetMaxNum() as integer
DECLARE FUNCTION GetNameAndLevel(sReadData$, iCount%, iCurrentLevel%) as integer
DECLARE FUNCTION MidPoint(iNode%, dBoxWidth#) as double
DECLARE FUNCTION NumChildren(iNode%) as integer
DECLARE SUB CreateOrgChart(dBoxWidth#, dBoxDistance#)
DECLARE SUB DrawOrgChart(dPaperHeight#, dPaperWidth#, dStartTop#, dBoxHeight#, dBoxWidth#, dBoxDistance#)
DECLARE SUB ReadOrgChart(sTextFile$)

REM Globals

Global ReportToComboBoxArr$
Global ReportToName$
Global ListBoxVal%
Global NewPersonName$
Global RemarkText$
Global ListBoxItemsArr$
Global LBSelectedPerson%
Global NumPersons%
Global ReportToNum%
Global RemarksArr$
Global ReportArr$
Global PersonsArr$
Global CONST Dimension% = 100
Global Ogroup%
Global Departments%
Global Levels%
Global DeptName$
Global Level$
Global Check%
Global Maxlength% 		'number of different people in org chart
Global TopMark$ 		'variable for who the top person reports to
Global Names$			'var for list of names
Global SortedNames$		'var for sorted list of names (with levels)
Global TopPerson% 
Global TitleText$
Global LegendText1$
Global LegendText2$
Global ErrLine%
Global NoEditKey%
NoEditKey%=0

Global FontsArr$
Global FontNameVal%
Global ColorsArr$
Global TextColor%
Global BoxColor%
Global BoldOptn%
Global ItalicOptn%
Global UnderlinedOptn%
Global JustifyArr$
Global JustifyType%

Dim FontsArr(11)
Dim ColorsArr(22)
Dim JustifyArr(3)

BoldOptn = 0
ItalicOptn = 0
UnderlinedOptn = 0

Dim ReportToComboBoxArr(2*Dimension) 
Dim RemarksArr(Dimension)
Dim ReportArr(Dimension)
Dim PersonsArr(Dimension)
Dim ListBoxItemsArr(Dimension)
Topmark = "-"		             'symbol for who the top person reports to
Dim Names$(Dimension,3)  	   'dimension array to maxlength with name, person to report to, level
Dim SortedNames$(Dimension,4) 'dimension array for sorted names with levels and remarks

GLOBAL CONST MAX_ELT = Dimension

' indicates whether the dept being looked at has all of its children already drawn
GLOBAL IsSet(MAX_ELT) as integer

' stores the left coordinate for each entry from the text file.  
GLOBAL Left_Coordinate(MAX_ELT) as double

' stores each number entry from the org chart text file
GLOBAL ST_Level(MAX_ELT) as integer
GLOBAL Label(MAX_ELT) as string
GLOBAL ST_Remark(MAX_ELT) as string

GLOBAL NumDeptOnEachLevel(MAX_ELT) as integer

GLOBAL ChildX(MAX_ELT)  as double
GLOBAL ChildY(MAX_ELT)  as double
GLOBAL ParentX(MAX_ELT) as double
GLOBAL ParentY(MAX_ELT) as double

' maximum size of arrays
GLOBAL gMaxVirtualBox as integer
GLOBAL gMaxLevel      as integer
GLOBAL dPaperHeight as double
GLOBAL dPaperWidth  as double
GLOBAL dStartTop    as double
GLOBAL dTop         as double
GLOBAL dBoxHeight   as double
GLOBAL dBoxDistance as double
GLOBAL dBoxWidth    as double
dBoxHeight   = 1
dBoxDistance = 1
dBoxWidth    = 2
GLOBAL OptionPaperSize%
GLOBAL OptionBoxProperties%
OptionBoxProperties=1

REM ***********************************************************************

CurrentLevel=0
CurrentPerson=""
CurrentPersonRep=""
CurrPos=1
Departments=2
Levels=3
DeptName = "Dep."
LevelName$ = "Name"

FontsArr(1)  = "AmeriGarmnd BT"
FontsArr(2)  = "Bodoni BT"
FontsArr(3)  = "Futura Hv BT"
FontsArr(4)  = "Lydian BT"
FontsArr(5)  = "News702 BT"
FontsArr(6)  = "Schneidler BT"
FontsArr(7)  = "Swis721 BT"
FontsArr(8)  = "Technical"
FontsArr(9)  = "TrumpetLite"
FontsArr(10) = "ZapfHumnst BT"
FontNameVal=1

ColorsArr(1)="White"
ColorsArr(2)="Black"
ColorsArr(3)="Dark Blue"
ColorsArr(4)="Blue"
ColorsArr(5)="Dark Green"
ColorsArr(6)="Med Green"
ColorsArr(7)="Lime Green"
ColorsArr(8)="Moss Green"
ColorsArr(9)="Rust"
ColorsArr(10)="Red"
ColorsArr(11)="Dark Purple"
ColorsArr(12)="Rose"
ColorsArr(13)="Orange"
ColorsArr(14)="Yellow"
ColorsArr(15)="Pale Yellow"
ColorsArr(16)="Pale Blue"
ColorsArr(17)="Grey"
ColorsArr(18)="Purple"
ColorsArr(19)="Mauve"
ColorsArr(20)="Med Blue"
ColorsArr(21)="Light Blue"

GLOBAL red%
GLOBAL green%
GLOBAL blue%

TextColor = 2
BoxColor = 1
JustifyArr(1)="Left"
JustifyArr(2)="Center"
JustifyArr(3)="Right"

JustifyType=2
TitleText="Org Chart 1"
LegendText1="Name"
LegendText2="Remark"

Errcount% = 0

Retry:

Errcount = ErrCount + 1
On error goto Retry
if (Errcount = 4) then
	Message("Internal error.  Please Re-run script.")
else
	Withobject Flow
		FlowDir$=.GetFlowDir()
	End WithObject
end if
On Error Exit

Global BMPName$
Global BMPExt$
BMPName=FlowDir + "\NOrgP"
BMPExt = ".bmp"


REM ************************************Dialog Definitions*****************************************************
BEGIN DIALOG OrgChart_Page1 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, 	BMPName + LTRIM$(STR(1)) + BMPExt 
	TEXT  140, 11, 136, 30, "Welcome to CorelFLOW's Organizational Chart Wizard!"
	TEXT  140, 35, 136, 32, "This Wizard will help you make Org charts, with ease.  You can load data from a text file, or work your way through the wizard."
	TEXT  140, 75, 136, 22, "Please, press NEXT to begin."
	PUSHBUTTON  186, 171, 40, 12, "&Next >"    	'Three
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_Page2 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(2)) + BMPExt
	TEXT  140, 11, 150, 30, "Select a method to create your chart:"
	OPTIONGROUP ogroup%
		OPTIONBUTTON  143, 39, 126, 12, "&Through the wizard"
		OPTIONBUTTON  143, 59, 126, 12, "&From an existing text file"
	PUSHBUTTON  146, 171, 40, 12, "< &Back"			'three
	PUSHBUTTON  186, 171, 40, 12, "&Next >"			'four (default)
	TEXT  144, 111, 125, 35, Description$
	CANCELBUTTON  233, 171, 40, 12
 	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_PageBlank 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(3)) + BMPExt
	TEXT  140, 11, 136, 36, "Enter the number of departments and the number of levels in each department."
	TEXT  140, 41, 136, 36, "You can enter the information directly into your FLOW diagram later."
	TEXT  148, 70, 50, 8, "&Departments :"
	SPINCONTROL  198, 69, 30, 12, Departments%
	TEXT  148, 94, 50, 8, "&Levels :"
	SPINCONTROL  198, 93, 30, 12, Levels%
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_PageFile 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(4)) + BMPExt
	TEXT  140, 11, 136, 36, "Please indicate the location of the text file."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	TEXT  141, 38, 50, 8, "&File Location :"
	TEXTBOX  142, 48, 139, 13, TextBox1Txt$
	PUSHBUTTON  240, 68, 40, 13, "&Browse"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_PageWizard 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(5)) + BMPExt
	TEXT  140, 12, 136, 51, "The Wizard will now provide you with a list from which your Org Chart will be generated."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	TEXT  140, 42, 136, 51, "On the next page you will have the option to create new positions that will be entered into your Org Chart, edit existing ones and save the information to a file!"
	TEXT  140, 82, 136, 51, "If you prefer, you may use the information as provided, and edit the chart directly in FLOW."
	TEXT  140, 110, 136, 51, "NOTE: The person at the top of the chart reports to '-'."
	CANCELBUTTON  233, 171, 40, 12
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_PageWizardDisplay 285, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 280, 15, BMPName + LTRIM$(STR(6)) + BMPExt
	LISTBOX  8, 37, 230, 113, ListBoxItemsArr$, LBSelectedPerson%
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Button 3
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'button 4
	PUSHBUTTON  242, 37, 40, 12, "Ne&w"	 'button 5
	PUSHBUTTON  242, 58, 40, 12, "&Edit"	 'button 6
	PUSHBUTTON  242, 80, 40, 12, "&Delete"	 'button 7
	PUSHBUTTON  242, 102, 40, 12, "&Save"	 'button 8
	TEXT  11, 25, 100, 8, "Positions / Remark / Reports to"
	TEXT 135, 148, 106, 8, "Remember to save your changes."
	CANCELBUTTON  233, 171, 40, 12	 'Cancel button
	GROUPBOX  3, 156, 278, 5, ""
END DIALOG

BEGIN DIALOG OrgChart_PageWizardNew 285, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(7)) + BMPExt
	TEXT  148, 37, 34, 8, "&Position"
	TEXTBOX  148, 48, 125, 13, NewPersonName$
	TEXT  148, 65, 34, 8, "&Remark"
	TEXTBOX  148, 77, 125, 13, RemarkText$
	TEXT  148, 94, 40, 10, "R&eports To"
	DDCOMBOBOX  148, 108, 125, 67, ReportToComboBoxArr$, ReportToName$
	TEXT  145, 9, 125, 26, "Add the name and remark, and then select to whom the person reports to."
	OKBUTTON  233, 157, 40, 12
	CANCELBUTTON  233, 171, 40, 12	 'Cancel button

END DIALOG

BEGIN DIALOG OrgChart_TitleSettings 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(8)) + BMPExt
	TEXT  140, 11, 142, 20, "You may give your chart a title and legend."
	GROUPBOX  140, 33, 136, 36, "Title"
	TEXTBOX  151, 45, 117, 13, TitleText$
	GROUPBOX  139, 77, 136, 73, "Legend"
	TEXT  149, 102, 15, 8, "&Top:"
	TEXTBOX  181, 99, 77, 13, LegendText1$
	TEXT  149, 120, 23, 8, "&Bottom:"
	TEXTBOX  181, 118, 77, 13, LegendText2$
	GROUPBOX  174, 87, 90, 52,""
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_PageSettings 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(9)) + BMPExt
	TEXT  135, 11, 151, 22, "Select your page settings." 
	GROUPBOX  153, 31, 107, 122, "Page Settings"
	OPTIONGROUP OptionPaperSize%
		OPTIONBUTTON  166, 45, 36, 10, "&Best fit"
		OPTIONBUTTON  166, 62, 80, 10, "&Portrait (8.5 x 11)"
		OPTIONBUTTON  166, 78, 80, 10, "&Landscape (11 x 8.5)"
		OPTIONBUTTON  166, 95, 39, 10, "&Custom"	 'Custom Values
	TEXT  175, 111, 24, 9, "&Width:"
	TEXTBOX  200, 108, 32, 13, dPaperWidth
	TEXT  175, 131, 24, 9, "&Height:"
	TEXTBOX  200, 129, 32, 13, dPaperHeight
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_BoxSettings 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(10)) + BMPExt
	TEXT  140, 11, 142, 20, "Choose a box size, color and text option."
	GROUPBOX  160, 36, 92, 96, "Box Settings"
	OPTIONGROUP OptionBoxProperties%
		OPTIONBUTTON  172, 49, 50, 12, "&Fit to page"
		OPTIONBUTTON  172, 62, 36, 12, "&Custom"
	TEXT  184, 76, 22, 8, "&Height:"
	TEXTBOX  208, 74, 32, 13, dBoxHeight
	TEXT  184, 94, 19, 8, "&Width:"
	TEXTBOX  208, 92, 32, 13, dBoxWidth
	TEXT  184, 111, 22, 8, "&Space:"
	TEXTBOX  208, 109, 32, 13, dBoxDistance
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	PUSHBUTTON  168, 139, 75, 12, "&Text and Color Options"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_TextSettings 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(11)) + BMPExt
	OKBUTTON  186, 171, 40, 12
	GROUPBOX  137, 6, 144, 96, "Text Options"
	GROUPBOX  137, 104, 144, 48, "Colors"
	TEXT  147, 22, 15, 8, "&Font:"
	DDLISTBOX  172, 22, 101, 110, FontsArr$, FontNameVal%
	TEXT  147, 44, 21, 8, "&Justify:"
	DDLISTBOX  172, 42, 101, 61, JustifyArr$, JustifyType%
	CHECKBOX  172, 62, 50, 10, "&Bold", BoldOptn%
	CHECKBOX  172, 75, 50, 10, "&Italic", ItalicOptn%
	CHECKBOX  172, 87, 50, 10, "&Underlined", UnderlinedOptn%
	TEXT  148, 136, 14, 8, "&Box:"
	DDLISTBOX  172, 135, 101, 69, ColorsArr$, BoxColor%
	TEXT  148, 116, 15, 8, "&Text:"
	DDLISTBOX  172, 116, 101, 56, ColorsArr$, TextColor%
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG OrgChart_PageFinish 288, 192, "Organization Chart Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(12)) + BMPExt
	TEXT  140, 11, 142, 35, "We are now ready to create your Org Chart.  Press Finish and watch FLOW and SCRIPT do all the work!"
	TEXT  140, 50, 142, 35, "Just imagine all of the things that you can do with the combined power of FLOW3 and Corel SCRIPT."
	PUSHBUTTON  146, 171, 40, 12, "< Back"
	PUSHBUTTON  186, 171, 40, 12, "Finish"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

REM ************************ MAIN PROGRAM ******************************

CONST FromFile = 1
CONST FromWizard=2
DIM FromWhere%

Page1:
REM ====================================================================
REM Dialog One - Welcome Dialog for Wizard
ret = DIALOG(OrgChart_Page1)

REM If this is a Cancel button just Exit The Wizard
if ret=2 then Stop

Ogroup=0 'Default type - ThroughWizard

Page2:
REM ====================================================================
REM Dialog 2 - Type of the Chart
ret = DIALOG(OrgChart_Page2)
if ret=2 then Stop 'If this is a Cancel button just Exit The Wizard
If ret=3 then goto Page1 'Back Button

REM We are now in Next Button
if ogroup=0 then goto ThroughWizard
if ogroup=1 then goto ExistingTextFile
Message "What Are U Doing Here?"
STOP

ExistingTextFile:
REM ====================================================================

ret = dialog(OrgChart_PageFile)
If ret= 2 then Stop
IF ret= 3 then Goto Page2
IF ret= 4 then 'Next Button
	If TextBox1Txt = "" then 
		Message "No file has been entered."
		Goto ExistingTextFile	
	Else
 		If TexttoText(TextBox1Txt,0) = 0 Then Goto ExistingTextFile
		ReportToComboBoxArr(1)="-"
		ReportToNum=1
		For i=1 To MaxLength
			PersonsArr(i) = Names(i,1)
			ReportArr(i)  = Names(i,2)
			RemarksArr(i) = Names(i,3)
			Call AddToReportToCBArr(PersonsArr(i))
			Call AddToReportToCBArr(ReportArr(i))
			ListBoxItemsArr(i)=PersonsArr(i)+"/"+RemarksArr(i)+"/"+ReportArr(i)
		Next i
		LBSelectedPerson=1
		NumPersons=MaxLength
		ListBoxItemsArr(NumPersons+1)=""
		FromWhere = FromFile
		Goto Again
	EndIf
End IF

If ret= 5 then 'Browse Button 
	Filename$=GetFileBox ("*.txt","Open") 
	TextBox1Txt$=Filename$
	Goto ExistingTextFile
End IF

Message "What Are U Doing Here?"


ThroughWizard:
REM ====================================================================
REM Creating Departments - Levels
ret = DIALOG(OrgChart_PageBlank)
If ret=2 then Stop 'If this is a Cancel button just Exit The Wizard
If ret=3 then goto Page2 'Back Button

if Departments < 0 OR Levels < 0 then 
	Message "We can not accept negative values."
	if Departments < 0 then Departments=1
	if Levels < 0 then Departments=1
	BEEP
	goto ThroughWizard
EndIf

if Departments*(Levels+1) >= MAX_ELT-1 then 
	Message "There are too many fields!"
	BEEP
	goto ThroughWizard
EndIf

REM This is The Real Stuff - Writing Directly Arrays
	ReportToComboBoxArr(1)="-"
	ReportToNum=1
	PersonsArr(1) = "Company"
	ReportArr(1)  = "-"
	RemarksArr(1) = ""
	For i=1 To Departments
		PersonsArr((i-1)*(Levels+1)+2) = DeptName + Str(i)
		ReportArr((i-1)*(Levels+1)+2)  = PersonsArr(1)
		RemarksArr((i-1)*(Levels+1)+2) = ""
		For j=1 To Levels
			PersonsArr((i-1)*(Levels+1)+j+2) = LevelName+Str(i)+Str(j)
 			If j=1 then 
				ReportArr((i-1)*(Levels+1)+j+2) = DeptName + Str(i)
 			else ReportArr((i-1)*(Levels+1)+j+2) = LevelName+Str(i)+Str(j-1)
			EndIf
			RemarksArr((i-1)*(Levels+1)+j+2)  = ""
		Next j
	Next i

	LBSelectedPerson=1
	NumPersons=Departments*(Levels+1)+1

	For i=1 to NumPersons
		ListBoxItemsArr(i)=PersonsArr(i)+"/"+RemarksArr(i)+"/"+ReportArr(i)
		Call AddToReportToCBArr(PersonsArr(i))
		Call AddToReportToCBArr(ReportArr(i))
	Next i
		
	ListBoxItemsArr(NumPersons+1)=""


REM ====================================================================
FromWhere=FromWizard

ThroughWizard2:
ret = DIALOG(OrgChart_PageWizard)
If ret=2 then Stop               'If this is a Cancel button just Exit The Wizard
If ret=3 then goto ThroughWizard 'Back Button
'ListBoxItemsArr(1)=""
LBSelectedPerson=1
'NumPersons=0
'ReportToNum=1
'ReportToComboBoxArr(1)="-"

Again:
ret = DIALOG(OrgChart_PageWizardDisplay)
If ret=2 then Stop               'If this is a Cancel button just Exit The Wizard
If ret=3 then 'Back Button
	If FromWhere = FromWizard then goto ThroughWizard2
	If FromWhere = FromFile   then goto ExistingTextFile
	Message "What is goin' on in here?!"
	STOP
EndIf

If ret=4 then                    'Next Button
	If NumPersons<=0 then 
		Message "The chart is empty."
		goto Again
	EndIf

	If NoEditKey then goto NoCheck

	ret=CheckDescription()
	If ret=-1 then
		Message "There is no top person defined."
		goto Again
	EndIf

	If ret=-2 then
		Message "There is somebody who reports to himself!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto Again
	EndIf

	If ret=-3 then
		Message "There more than one person who reports to top person!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto Again
	EndIf

	If ret>0 then
		Message "There is someone who reports to somebody who is not in the chart!"+CHR(13)+"Take a look at "+PersonsArr(ret)+"."
		goto Again
	EndIf

NoCheck:
	ret = PageAndFinish()
	if ret = 2 then Stop
	if ret = 3 then goto Again

	ret = TexttoText("",NumPersons)
	TransformOrgChart(NumPersons)
	Stevesub
	Stop
EndIf
If ret=5 then            'New Button
	If NumPersons >= Dimension-1 then
		Message "Sorry, there are too many records in this chart!"
		BEEP
	EndIf

	NewPersonName = ""
	If NumPersons > 0 then 
          ReportToName  = ""
	else 
		ReportToName  = "-"
	EndIf
	RemarkText = ""
NewAgain:
	ret = DIALOG(OrgChart_PageWizardNew)
	If ret=2 then goto Again
     REM New Person is added!!!
	if NewPersonName = ReportToName then
		Message "A person cannot report to themselves."
		goto NewAgain
	EndIf
	If NumPersons > 0 then
		For j=1 To NumPersons 
			if NewPersonName=PersonsArr(j) then
				Message "There is already such person!"
				goto NewAgain
			EndIf
		Next j
	EndIf

	NoEditKey=0

	NumPersons=NumPersons+1
	PersonsArr(NumPersons)=NewPersonName
	ReportArr(NumPersons) = ReportToName
	RemarksArr(NumPersons) = RemarkText
	ListBoxItemsArr(NumPersons)=NewPersonName + " / " + RemarkText + " / " + ReportToName
	IF ReportToName <> "" then Call AddToReportToCBArr(ReportToName)
	IF NewPersonName <> "" then Call AddToReportToCBArr(NewPersonName)
	Goto Again
EndIf 'End of New Button

If ret=6 then 'Edit Button
	If LBSelectedPerson <= NumPersons then
		NewPersonName = PersonsArr(LBSelectedPerson)
		ReportToName  = ReportArr(LBSelectedPerson)
		RemarkText    = RemarksArr(LBSelectedPerson)
	else 
		goto Again
	EndIf
EditAgain:
	ret = DIALOG(OrgChart_PageWizardNew)
	If ret=2 then goto Again
     REM This Person is edited!!!
	if NewPersonName = ReportToName then
		Message "A person cannot report to themself."
		goto EditAgain
	EndIf

	If NumPersons > 0 then
		For j=1 To NumPersons 
		   If j <> LBSelectedPerson then
			if NewPersonName=PersonsArr(j) then
				Message "There is already such person!"
				goto NewAgain
			EndIf
             EndIf   
		Next j
	EndIf

	NoEditKey=0

	PName = PersonsArr(LBSelectedPerson)
	Repname = ReportArr(LBSelectedPerson)

 	PersonsArr(LBSelectedPerson) = NewPersonName
	ReportArr(LBSelectedPerson)  = ReportToName
	RemarksArr(LBSelectedPerson) = RemarkText
	ListBoxItemsArr(LBSelectedPerson)=NewPersonName + " / " + RemarkText + " / " + ReportToName

	'try to delete from ComboBox arr
	Call DelFromReportToCBArr(PName)
	Call	DelFromReportToCBArr(RepName)

	Call AddToReportToCBArr(NewPersonName)
	Call AddToReportToCBArr(ReportToName)

	Goto Again
EndIf 'End of Edit Button

If ret=7 then 'Delete Button
	If LBSelectedPerson <= 0 then goto Again 'If there is no selection in the LB
	If LBSelectedPerson > NumPersons then goto Again
	If NumPersons <= 0 then goto Again 'Yes, yes, I know that this is stupid!
	'first remember the names for deleting
	PName = PersonsArr(LBSelectedPerson)
	RepName = ReportArr(LBSelectedPerson)
	'the real stuff
	For j=LBSelectedPerson To NumPersons-1
		PersonsArr(j) = PersonsArr(j+1)
		ReportArr(j)  = ReportArr(j+1)
		RemarksArr(j) = RemarksArr(j+1)
		ListBoxItemsArr(j)=PersonsArr(j) + " / " + RemarksArr(j) + " / " + ReportArr(j)
	Next j
	ListBoxItemsArr(NumPersons)=""
	NumPersons=NumPersons-1	
	'try to delete from ComboBox arr
	Call DelFromReportToCBArr(PName)
	Call	DelFromReportToCBArr(RepName)

	If LBSelectedPerson > NumPersons then LBSelectedPerson = NumPersons
	Goto Again
EndIf 'End of Delete Button

If ret=8 then 'Save Button
	'Some checks
	If NumPersons<=0 then 
		Message "The chart is empty."
		goto Again
	EndIf

	ret=CheckDescription()
	If ret=-1 then
		Message "There is no top person!"
		goto Again
	EndIf

	If ret=-2 then
		Message "There is somebody who reports to himself!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto Again
	EndIf

	If ret=-3 then
		Message "There is more than one top person!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto Again
	EndIf

	If ret>0 then
		Message "There is someone who reporting  to somebody who is not in the chart!"+CHR(13)+"Take a look at "+PersonsArr(ret)+"."
		goto Again
	EndIf

	'It looks like everything is OK
	Filename$=GetFileBox ("*.txt","Save",1) 
	REM This is The Real Stuff - Writing to file
	IF Filename="" then goto Again
	Open Filename For Output As 1
	For i=1 To NumPersons
		Write #1, PersonsArr(i), RemarksArr(i), ReportArr(i)
	Next i
	Close #1

	NoEditKey=1

	Goto Again
EndIf


Message "What Are U Doing Here?"

Stop


FUNCTION PageAndFinish 

TitleSettings:
	ret = DIALOG(OrgChart_TitleSettings)

	if ret = 2 OR ret = 3 then ' Cancel or Prev Buttons
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf

WizardPageSettings:
	REM =============================================================
	REM The last step before complete final???

	ret = DIALOG(OrgChart_PageSettings)

	if ret = 2 then ' Cancel Button
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf

	IF ret=3 then Goto TitleSettings

	if OptionPaperSize = 4 then
		if dPaperHeight <= 0 or dPaperWidth <= 0 then
			message "Paper Size must be greater than 0"
			GOTO WizardPageSettings
		end if
	end if

WizardBoxSettings:
	ret = DIALOG(OrgChart_BoxSettings)

	if ret = 2 then ' Cancel Button
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf

	IF ret=3 then Goto WizardPageSettings

	If ret=5 then 
		ret = DIALOG(OrgChart_TextSettings)
		Goto WizardBoxSettings
	Endif

	if OptionBoxProperties = 1 then
		if dBoxHeight = 0 or dBoxWidth = 0 or dBoxDistance = 0 then
			message "Box properties cannot be set to zero"
			GOTO WizardBoxSettings
		end if
	end if


	REM =============================================================

	ret = DIALOG(OrgChart_PageFinish)

	if ret = 2 then ' Cancel Button
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf
	
	IF ret=3 then Goto WizardBoxSettings
	

	PageAndFinish% = ret

EndFunc:
END FUNCTION



REM ***********************************************************************
SUB AddToReportToCBArr (RepToName)

	If RepToName="" then goto EndOfThisSub
	For j=1 To ReportToNum 
		if RepToName = ReportToComboBoxArr(j) then goto EndOfThisSub
	Next j
	ReportToNum=ReportToNum+1
	ReportToComboBoxArr(ReportToNum)=RepToName

EndOfThisSub:
End SUB




REM ***********************************************************************
SUB DelFromReportToCBArr (RepToName)


	If RepToName="" then goto EndOfThisSub
	If ReportToNum < 2 then goto EndOfThisSub

	'We will delete him/her only if he/she is not into any array
	For i=1 to NumPersons
		If ReportArr(i) = RepToName then goto EndOfThisSub
		If PersonsArr(i) = RepToName then goto EndOfThisSub
	Next i

	For i=2 to ReportToNum
		If ReportToComboBoxArr(i)=RepToName then
			j=i
			goto et1
		EndIf
	Next i
	' This string is not in the array
	goto EndOfThisSub	
Et1:
	For i=j To ReportToNum-1		
		ReportToComboBoxArr(i)=ReportToComboBoxArr(i+1)
	Next i
	ReportToComboBoxArr(ReportToNum)=""
	ReportToNum = ReportToNum-1		
EndOfThisSub:
End SUB


REM ***********************************************************************

FUNCTION TexttoText (File_Name$, NumLines%)

If File_Name <> "" then Goto OpenFile

REM Tranfer directly the data between arrays

	For i=1 To NumLines
		Names(i,1) = PersonsArr(i)
		Names(i,2) = ReportArr(i)
		Names(i,3) = RemarksArr(i)
	Next i

	MaxLength=NumLines

Goto FindTopPerson 

REM Working with file
OpenFile:
	Open File_Name for input as 1

Goto CheckLength

REM ***********************************************************************
CheckLength:

Temp$=""				'dummy var			
MaxLength = 0 			'number of people; reset to zero

	While not EOF(1)
		Input #1,Temp$
    	
	MaxLength=MaxLength+1
		
	Wend

	If MaxLength MOD 3 > 0 then
		Message "This file is corrupt."+CHR(13)+"Some lines may contain more or less than three fields."
		TextToText = 0
		Goto EndTTT				
	EndIf

	MaxLength=MaxLength/3
 
	Close #1

	If MaxLength > Dimension then
		Message "This file is too big for this Wizard!"
		TextToText = 0
		Goto EndTTT				
	EndIf

Goto EnterIntoArray

REM ***********************************************************************
EnterIntoArray:
	
Cc% = 0					'counter reset

	Open File_Name for input as 1
	
	While not EOF(1)
		Cc=Cc+1
		Input #1, Names(Cc,1), Names(Cc,3), Names(Cc,2)
	Wend
	
	Close #1

	TextToText = 1 'Temporary
 	Goto EndTTT	'Temporary			

Goto FindTopPerson

REM ***********************************************************************
FindTopPerson: 

c%=1				'counter reset
TopPerson%=0

	Do until Names(c,2) = Topmark

	C=C+1
	If C > MaxLength then
 		Message "There is no top person in this file!"+CHR(13)+"Nobody reports to '-' (top person) in this file!"
 		TextToText = 0
 		Goto EndTTT				
	EndIf

	Loop

	TopPerson = C	

	SortedNames(1,1) = Names(TopPerson,1)
	
Goto FindOthersSim:

REM ***********************************************************************
FindOthersSim:

REM  Define the stack

REM  Init the Stack

Dim Stack$(Dimension)

Rem Init

Boss$=SortedNames(1,1)

StackLevel%=1
'SortedNames(1,1)=Boss
SortedNames(1,3)=StackLevel		
SortedNames(1,4)=Names(TopPerson,3)		
Num=2
Stack(StackLevel)=Boss


Again:

For i = 1 to MaxLength
	If Names(i,2)=Boss then
		Boss=Names(i,1) 			
		StackLevel=StackLevel+1

		Stack(StackLevel)=Boss

		SortedNames(Num,1)=Boss
		SortedNames(Num,3)=StackLevel		
		SortedNames(Num,4)=Names(i,3)
          Num=Num+1

		Names(i,2)=""

		Goto Again		

	EndIf
Next


If StackLevel > 1 then
	Stacklevel=Stacklevel-1
	Boss=Stack(StackLevel)
	
	Goto Again

EndIf

TextToText = 1

REM ***********************************************************************
EndTTT:
End FUNCTION


REM This function checks wheather description in arrays
REM PersonsArr and ReportArr is coorect
REM Return Value: 0     - if everything is OK
REM               Index - where is the first error
REM               -1    - if there is no somebody who report to top DUMMY person
REM 			   -2    - if there is somebody who report to himself
REM               -3    - if there more than one person who report to top DUMMY person

Function CheckDescription%
Dim Marks%(Dimension)
Dim Ver%(Dimension)
NumList=0
Num%=0
Dim Key%

	For i=1 To NumPersons
		If PersonsArr(i)=ReportArr(i) then 
			CheckDescription = -2
			ErrLine=i
			Goto EndLab
		EndIf
		If ReportArr(i)="-" then 
       		Num=Num+1
			ErrLine=i
		EndIf
	Next i

	If Num=0 then 
		CheckDescription = -1
		Goto EndLab
	EndIf

	If Num > 1 then 
		CheckDescription = -3
		Goto EndLab
	EndIf

	'Let start at the end!
	For i=1 To NumPersons
		If ReportArr(i)="-" then 
			   Marks(i) = 1
			   Ver(i) = 0
   		   else Marks(i) = 0
			   Ver(i)=0
		EndIf
	Next i


'There is somebody who report to top DUMMY person

DoItAgain:
	Key = 0
	For i=1 To NumPersons
		If Marks(i)=1 AND Ver(i)=0 then
  		    For j=1 To NumPersons
  			 If PersonsArr(i)=ReportArr(j) then 
				Marks(j)=1				
				Ver(i)=1				
				Key=1
			 EndIf
		    Next j
		EndIf
	Next i	
If Key=1 then goto DoItAgain

	For i=1 To NumPersons
		If Marks(i)=0 then
			CheckDescription = i
			Goto EndLab
		EndIf
	Next i

	CheckDescription = 0
EndLab:
End Function


SUB Stevesub 

if OptionPaperSize = 0 and OptionBoxProperties = 0 then
	dBoxHeight   = 1
	dBoxWidth    = 2
	dBoxDistance = 1

	dPaperHeight = (gMaxLevel+2)*dBoxHeight + (gMaxLevel+1)*dBoxDistance + 2
	dPaperWidth  = gMaxVirtualBox*dBoxWidth + (gMaxVirtualBox-1)*dBoxDistance + 2
elseif OptionPaperSize = 0 then
	dPaperHeight = (gMaxLevel+2)*dBoxHeight + (gMaxLevel+1)*dBoxDistance + 2
	dPaperWidth  = gMaxVirtualBox*dBoxWidth + (gMaxVirtualBox-1)*dBoxDistance + 2
elseif OptionPaperSize = 1 then
	dPaperHeight = 11
	dPaperWidth  = 8.5
elseif OptionPaperSize = 2 then
	dPaperHeight = 8.5
	dPaperWidth  = 11
end if

if OptionBoxProperties = 0 and OptionPaperSize <> 0 then
	dBoxHeight = dPaperHeight/(2*(gMaxLevel+2)-1)
	dBoxWidth  = dPaperWidth/(2*gMaxVirtualBox-1)
	If dBoxHeight < dBoxWidth then 
              dBoxDistance = dBoxHeight
   	    else dBoxDistance = dBoxWidth
	EndIf 
end if

dStartTop = dPaperHeight/2-(dPaperHeight-(gMaxLevel+2)*dBoxHeight-(gMaxLevel+1)*dBoxDistance)/2
dStartTop = dStartTop-dBoxHeight-dBoxDistance 'Because of Title

CreateOrgChart dBoxWidth, dBoxDistance
DrawOrgChart dPaperHeight, dPaperWidth, dStartTop, dBoxHeight, dBoxWidth, dBoxDistance
ConnectObjects 

WITHOBJECT FLOW
REM Making Legend
	If LegendText1 <> "" OR LegendText2 <> "" then 
		.CreateTextString LegendText1+CHR(13)+LegendText2, dPaperWidth/2-dBoxWidth,-dStartTop-dBoxDistance,dPaperWidth/2,-dStartTop-dBoxDistance-dBoxHeight
		.ApplyOutline 1, 1, 0,0,0, 2,0
	EndIf

	.SelectAllObjects
 	.FormatTextFont FontsArr(FontNameVal)
	If BoldOptn then .FormatTextBold
	If ItalicOptn then .FormatTextItalic
	If UnderlinedOptn then .FormatTextUnderline
	.FormatTextJustify JustifyType-1
	Call SetColorRGB(TextColor)
	.FormatTextColor red,green,blue
	'Set Box color
	Call SetColorRGB(BoxColor)
	.ApplyUniformFillColor red,green,blue
	.ObjectSelectNone

REM Making Title
	If TitleText <> "" then	
		.CreateTextString TitleText, -dPaperWidth/4, dStartTop+dBoxHeight+dBoxDistance,dPaperWidth/4,dStartTop
	 	.FormatTextFont FontsArr(FontNameVal)
		If BoldOptn then .FormatTextBold
		If ItalicOptn then .FormatTextItalic
		If UnderlinedOptn then .FormatTextUnderline
		.FormatTextJustify 1
		.FormatTextColor 0,0,0
	EndIf

	.SetRefresh TRUE
	.ObjectSelectNone
	.WindowActivate
END WITHOBJECT
END SUB 'END Steve's MAIN


SUB ConnectObjects
	DIM iNewNode as integer
	DIM iNode as integer

	iNode = 2

WITHOBJECT FLOW
	.SetAutoRouting TRUE, 0.05, 0.05, 50
	while ST_level(iNode) > 1
		if ST_level(iNode) > ST_level(iNode-1) then
			iNewNode = iNode
		else
			iNewNode = iNode
			while ST_level(iNode) <= ST_level(iNewNode-1)
				iNewNode = iNewNode - 1
			wend
		end if
	
		x1& = ParentX(iNewNode-1)
		y1& = ParentY(iNewNode-1)
		x2& = ChildX(iNode)
		y2& = ChildY(iNode)

		.CreateRoutedLine ParentX(iNewNode-1), ParentY(iNewNode-1), ChildX(iNode), ChildY(iNode), -1, 0
		.EndLineCurveAtPoint x2, y2
		iNode = iNode + 1
	wend
END WITHOBJECT

END SUB 'ConnectObjects


' CreateOrgChart traverses the array starting the children of a node first i.e. the left most node on 
' a tree.  It then traverses back to its siblings, and its parents.

SUB CreateOrgChart(BoxWidth#, BoxDistance#)
	DIM dRightMostCoordinate as double
	DIM iNode as integer
	DIM iTmpNode as integer

	done = false
	iNode = 1

	' right edge of the rightmost box in the org chart
	dRightMostCoordinate = -1*(gMaxVirtualBox*dBoxWidth + (gMaxVirtualBox-1)*dBoxDistance)/2 - dBoxDistance
	if NumChildren(iNode) > 0 then
		while done = false
			if ST_level(iNode+1) > ST_level(iNode) and IsSet(iNode+1) = false then
				iNode=iNode+1
			' no kids
			elseif ST_level(iNode+1) <= ST_level(iNode) and IsSet(iNode) = false then
				IsSet(iNode) = true
				left_coordinate(iNode) = dRightMostCoordinate + BoxDistance
				dRightMostCoordinate = left_coordinate(iNode) + BoxWidth

				if iNode = 1 then
					done=true
				else
					iNode = findLeftUnset(iNode)
				end if
			elseif (ST_level(iNode+1) > ST_level(iNode)) and (IsSet(iNode+1) <> false) then
				iTmpNode=findFirstUnsetChild(iNode)
				' kids are drawn
				if iTmpNode < 0 then
					IsSet(iNode) = true
					' find mid of kids and draw
					left_coordinate(iNode) = MidPoint(iNode, BoxWidth)

					if iNode = 1 then
						done = true
					else
						iNode = findLeftUnset(iNode)
					end if
				else
					iNode = iTmpNode
				end if
			end if
		wend
	else
		left_coordinate(iNode) = -1*BoxWidth/2
	end if

REM Make the Boss centered
	i=1
	while ST_level(i) <> 1
		i=i+1
	wend
	left_coordinate(i)=-BoxWidth/2	
END SUB 'CreateOrgChart


'DrawOrgChart sets up the environment and draws the entire OrgChart based on the ST_level arrays

SUB DrawOrgChart(PaperHeight#, PaperWidth#, StartTop#, BoxHeight#, BoxWidth#, BoxDistance#)
	DIM iNode as integer

Errcount  = 0
Retry2:

Errcount = ErrCount + 1
On error goto Retry2
if (Errcount = 4) then
	Message("Internal error.  Please Re-run script.")

else
	WITHOBJECT FLOW
 		.SetVisible true
		.FileNew
		.SetUnits 2
		.SetPageLayOut true, PaperWidth, PaperHeight, 0, 0, 0, 0
          .SetRuler 2, 2, 0, 0
		.SetRefresh FALSE

		iNode = 1
		while ST_level(inode) > 0
			dTop = StartTop - ((ST_level(iNode) - 1) * BoxHeight + (ST_level(iNode) - 1) * BoxDistance)
			dBottom = dTop - BoxHeight
			.CreateRectangle left_coordinate(iNode), dTop, (left_coordinate(iNode) + BoxWidth), (dBottom), label(iNode)
			if label(iNode) <> "" then
				.SetObjectText label(iNode)+CHR(13)+ST_Remark(iNode), label(iNode)
			end if
			ParentX(iNode) = left_coordinate(iNode)+BoxWidth/2
			ParentY(iNode) = dBottom
			ChildX(iNode)  = parentX(iNode)
			ChildY(iNode)  = dTop
			tceny# = dBottom + BoxHeight/2
			.ObjectSelectNone
			.CreateConnector ParentX(iNode), tceny	
			.MoveConnector ParentX(iNode), tceny, ParentX(iNode), ParentY(iNode)
			.CreateConnector ParentX(iNode), tceny
			.MoveConnector ParentX(iNode), tceny, ChildX(iNode), ChildY(iNode)
			iNode = iNode + 1
		wend
	END WITHOBJECT
end if
On Error Exit

END SUB 'DrawOrgChart


' Find the first child of "node" who has not been drawn.  This basically goes through
' "ST_level" array and checks for nodes that are a ST_level below "node" and that have not
' been set according to the "isset" array.  Note, the loop should stop when it has
' finished reading all entries (i.e. ST_level[i] <> 0) or when the first child that has
' not been drawn is found or when another node of the same ST_level is encountered.
' Returns -1 if no child was found or the index of the child.

FUNCTION FindFirstUnSetChild(iNode%) as integer
	DIM i as integer
	DIM iNewNode as integer

	i = iNode + 1
	done = false
	iNewNode = -1

	while ST_level(i) <> 0 and done = false
		' i has the same ST_level
		if ST_level(i) = ST_level(iNode) then
			' no unset child
			done = true
		' i is a child and has not been set
		elseif ST_level(i) = ST_level(iNode) + 1 and IsSet(i) = false then
			done = true
			iNewNode = i
		end if
		i = i + 1
	wend
	FindFirstUnSetChild = iNewNode
END FUNCTION 'FindFirstUnSetChild


' Finds the first node of a higher ST_level that has not been set.  It goes backwards through the
' ST_level array until it hits a node that has not bee set.  A -1 is returned if no such node is
' found otherwise the index to the node is returned.   

FUNCTION FindLeftUnSet(iNode%) as integer
	DIM i as integer
	DIM iNewNode as integer

	i = iNode - 1
	done = false
	iNewNode = -1

	while i => 1 and done = false
		if IsSet(i) = false then
			done = true
			iNewNode = i
		end if
		i = i - 1
	wend
	FindLeftUnSet = iNewNode
END FUNCTION 'FindLeftUnSet


' Calculates the left coordinate for a parent from the left and right coordinates from its children.
' If there are an odd number of children, then the left_coordinate of the middle child is used as 
' the left coordinate of the parent otherwise, a formula is used to calculate the parent's left_coordinate.

FUNCTION MidPoint(iNode%, BoxWidth#) as double
	DIM i as integer
	DIM dLeftMost  as double
	DIM dRightMost as double

	i = iNode+1
	first = false

	while ST_level(i) <> 0 and ST_level(i) > ST_level(iNode)
		if ST_level(i) = ST_level(iNode)+1 then
			if (first = false) then
				dLeftMost = left_coordinate(i)
				first = true
			end if
			dRightMost = left_coordinate(i) + BoxWidth
		end if
		i = i+1
	wend
	MidPoint = dLeftMost + ((dRightMost - dLeftMost)/2 - BoxWidth/2)
END FUNCTION 'MidPoint


' Calculates the number of children a particular node has.  It counts the number of ST_level[node]+1 entries
' there are between this node and the end of the array or the next node of the same ST_level, which ever one
' comes first.

FUNCTION NumChildren(iNode%) as integer
	DIM i as integer
	DIM iCount as integer

	i = iNode+1
	iCount = 0

	' loop exists when end of entries
	while (ST_level(i) = ST_level(iNode)+1) or (ST_level(i) > ST_level(iNode))
		if (ST_level(i) = ST_level(iNode)+1) then
			iCount = iCount+1
		end if
		i=i+1
	wend
	NumChildren = iCount
END FUNCTION 'NumChildren


SUB TransformOrgChart(NumRec%)
Dim Num%
CurrentLevel%=0
gMaxVirtualBox = 1

 
	For i=1 To NumRec
		Num = Val(SortedNames(i,3))
 		if Num > 1 then
 			if Num > CurrentLevel then
 				' do nothing
 			else
 				gMaxVirtualBox = gMaxVirtualBox + 1
 			end if
 		end if
 
 	    if Num > gMaxLevel then
     	    gMaxLevel = Num
 	    end if
 
 	    ST_level(i) = Num
 	    NumDeptOnEachLevel(Num) = NumDeptOnEachLevel(Num) + 1

	    'Stores the text	
	    Label(i)     = SortedNames(i,1)
	    ST_Remark(i) = SortedNames(i,4)
	    CurrentLevel = Num		
	Next i

END SUB 'TransformOrgChart


SUB SetColorRGB(choice%)

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
