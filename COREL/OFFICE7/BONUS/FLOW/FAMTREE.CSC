REM Family Tree Wizard
REM Famtree.csc

REM Declares

Declare SUB AddToFamilyCBArr(RepName$)
Declare SUB DelFromFamilyCBArr(RepToName$)
Declare FUNCTION TextToText% (FileName$, Num%)
Declare SUB TransformFamChart(NumRec%)

Declare FUNCTION PageAndFinish% ()
Declare Function CheckDescription% ()

Declare SUB Stevesub ()
Declare SUB SetColorRGB(col%)
Declare Function PrintError%(err%)

DECLARE FUNCTION FindFirstUnSetChild(iNode%) as integer
DECLARE FUNCTION FindTopUnSet(iNode%) as integer
DECLARE FUNCTION GetMaxNum() as integer
DECLARE FUNCTION MidPoint(iNode%, dBoxHeight#) as double
DECLARE FUNCTION NumChildren(iNode%) as integer
DECLARE SUB ConnectObjects(BoxDistance#)
DECLARE SUB CreateFamilyTree(BoxHeight#, BoxDistance#)
DECLARE SUB DrawFamilyTree(PaperHeight#, PaperWidth#, StartLeft#, BoxHeight#, BoxWidth#, BoxDistance#)

REM Globals

Global FamilyComboBoxArr$
Global ListBoxVal%
Global NewPersonName$
Global BornDateText$
Global DeadDateText$
Global RelationVal%
Global ListBoxItemsArr$
Global LBSelectedPerson%
Global NumPersons%
Global FamilyNum%
Global PersonsArr$
Global RelPersonArr$
Global RelationArr$
Global BornDateArr$
Global DeadDateArr$
Global CONST Dimension% = 100
Global Ogroup%
Global Generations%
Global Check%
Global Maxlength% 		'number of different people in FamTree
Global CONST TopMark = "-" 'Constant for who the top person reports to
Global Names$			'var for list of names
Global SortedNames$		'var for sorted list of names (with levels)
Global TopPerson% 
Global TitleText$
Global ErrLine%
Global NoEditKey%
NoEditKey%=0
Global LegendText$
Global LegendChB%

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
LegendChB = 1

Dim FamilyComboBoxArr(2*Dimension) 
Dim RelPersonArr(Dimension)
Dim PersonsArr(Dimension)
Dim RelationArr(Dimension)
Dim BornDateArr(Dimension)
Dim DeadDateArr(Dimension)
Dim ListBoxItemsArr(Dimension)
Dim Names$(Dimension,5)  	  'dimension array to maxlength with name, person to report to, level
Dim SortedNames$(Dimension,6)   'dimension array for sorted names with levels and remarks


GLOBAL CONST MAX_ELT = Dimension

' indicates whether the dept being looked at has all of its children already drawn
GLOBAL IsSet(MAX_ELT) as integer

' stores the left coordinate for each entry from the text file.  
GLOBAL Top_Coordinate(MAX_ELT) as double

' stores each number entry from the FamTree text file
GLOBAL ST_Level(MAX_ELT) as integer
GLOBAL Label(MAX_ELT) as string
GLOBAL ST_Remark(MAX_ELT) as string

GLOBAL NumDeptOnEachLevel(MAX_ELT) as integer

GLOBAL TopConnectorX(MAX_ELT)    as double
GLOBAL TopConnectorY(MAX_ELT)    as double
GLOBAL BottomConnectorX(MAX_ELT) as double
GLOBAL BottomConnectorY(MAX_ELT) as double
GLOBAL LeftConnectorX(MAX_ELT)   as double
GLOBAL LeftConnectorY(MAX_ELT)   as double
GLOBAL RightConnectorX(MAX_ELT)  as double
GLOBAL RightConnectorY(MAX_ELT)  as double

' maximum size of arrays
GLOBAL gMaxVirtualBox as integer
GLOBAL gMaxLevel      as integer
GLOBAL dPaperHeight   as double
GLOBAL dPaperWidth    as double
GLOBAL dStartLeft     as double
GLOBAL dLeft          as double
GLOBAL dBoxHeight     as double
GLOBAL dBoxDistance   as double
GLOBAL dBoxWidth      as double
dBoxHeight   = 1
dBoxDistance = 1
dBoxWidth    = 2
GLOBAL OptionPaperSize%
GLOBAL OptionBoxProperties%
OptionBoxProperties=1

REM ***********************************************************************

CurrentLevel=0
CurrentPerson=""
CurrentRelPerson=""
CurrPos=1
Generations=3

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
Errcount = 0
GLOBAL red%
GLOBAL green%
GLOBAL blue%

TextColor = 2
BoxColor = 1
JustifyArr(1)="Left"
JustifyArr(2)="Center"
JustifyArr(3)="Right"

JustifyType=2
TitleText="Family Chart 1"
LegendText="Name"+CHR(13)+"Birth date"+CHR(13)+"Death date"

Retry:

Errcount = ErrCount + 1
On error goto Retry
if (Errcount = 4) then
	Message("Internal error.  Please Re-run script.")
	PrintError 0
else
	Withobject Flow
		FlowDir$=.GetFlowDir()
	End WithObject
end if
On Error Exit

Global BMPName$
Global BMPExt$
BMPName=FlowDir+ "\FTP"
BMPExt = ".bmp"


REM ************************************Dialog Definitions*****************************************************
BEGIN DIALOG FamTree_Page1 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187,  BMPName + LTRIM$(STR(1)) + BMPExt 
	TEXT  140, 11, 136, 22, "Welcome to CorelFLOW's Family Tree Wizard!"
	TEXT  140, 35, 136, 32, "This Wizard will help you create a Family Tree, with ease.  You can load data from a text file, or work your way through the Wizard."
	TEXT  140, 75, 136, 22, "Please press NEXT to begin."
	PUSHBUTTON  186, 171, 40, 12, "&Next >"    	'Three
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG FamTree_Page2 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187,  BMPName + LTRIM$(STR(2)) + BMPExt 
	TEXT  140, 11, 136, 22, "Select a method to create your diagram."
	OPTIONGROUP ogroup%
		OPTIONBUTTON  143, 39, 126, 12, "&Through the Wizard"
		OPTIONBUTTON  143, 59, 126, 12, "&From an Existing Text File"
	TEXT  144, 111, 125, 35, Description$
	PUSHBUTTON  146, 171, 40, 12, "< &Back"		'three
	PUSHBUTTON  186, 171, 40, 12, "&Next >"		'four (default)
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG FamTree_PageBlank 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187,  BMPName + LTRIM$(STR(3)) + BMPExt 
	TEXT  140, 11, 136, 36, "Indicate the number of generations you wish to chart, up to a maximum of six."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	TEXT  148, 51, 50, 10, "&Generations:"
	SPINCONTROL  198, 51, 30, 12, Generations%
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG FamTree_PageFile 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187,  BMPName + LTRIM$(STR(4)) + BMPExt 
	TEXT  140, 11, 136, 36, "The Wizard will now ask you to select the Location of the Text File."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	TEXT  141, 38, 50, 8, "File Location :"
	TEXTBOX  142, 48, 139, 13, TextBox1Txt$
	PUSHBUTTON  240, 68, 40, 13, "Browse"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG FamTree_PageWizard 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187,  BMPName + LTRIM$(STR(5)) + BMPExt 
	TEXT  140, 12, 136, 51, "The Wizard will now provide you with a list from which your Family Tree will be generated."
	TEXT  140, 42, 136, 51, "On the next page you will have the option to add relatives that will be entered into your Family Tree, edit existing ones and save the information to a file!"
	TEXT  140, 82, 136, 51, "Press NEXT to continue."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG FamTree_PageWizardDisplay 285, 192, "Family Tree Wizard"
	IMAGE  2, 2, 280, 15, BMPName + LTRIM$(STR(6)) + BMPExt 
	LISTBOX  8, 37, 230, 113, ListBoxItemsArr$, LBSelectedPerson%
	PUSHBUTTON  146, 171, 40, 12, "< &Back"	 'Button 3
	PUSHBUTTON  186, 171, 40, 12, "&Next >"	 'button 4
	PUSHBUTTON  242, 37, 40, 12, "N&ew"	 'button 5
	PUSHBUTTON  242, 58, 40, 12, "&Edit"	 'button 6
	PUSHBUTTON  242, 80, 40, 12, "&Delete"	 'button 7
	PUSHBUTTON  242, 102, 40, 12, "&Save"	 'button 8
	TEXT  11, 25, 200, 8, "Name / Relationship  / Related To / Born / Died"
	TEXT 135, 148, 106, 8, "Remember to save your changes."
	CANCELBUTTON  233, 171, 40, 12	 'Cancel button
	GROUPBOX  3, 156, 278, 5, ""
END DIALOG

BEGIN DIALOG FamTree_PageWizardNew 285, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(7)) + BMPExt 
	TEXTBOX  148, 41, 122, 13, NewPersonName$
	TEXT  148, 65, 34, 8, "&Born:"
	TEXTBOX  194, 62, 76, 14, BornDateText$
	TEXT  148, 83, 40, 8, "&Died:"
	TEXTBOX  194, 80, 76, 14, DeadDateText$
	DDCOMBOBOX  148, 122, 122, 51, FamilyComboBoxArr$, RelPersonName$
	TEXT  135, 6, 148, 30, "To define the person at the top of the family tree select the '-' in the relation box."
	GROUPBOX  142, 110, 134, 53, "Relationship"
	OPTIONGROUP RelationVal%
		OPTIONBUTTON  156, 142, 34, 11, "&Mother"
		OPTIONBUTTON  214, 143, 32, 10, "&Father"
	GROUPBOX  142, 28, 134, 78, "Name"
	CANCELBUTTON  233, 171, 40, 12	 'Cancel button
	OKBUTTON  186, 171, 40, 12
END DIALOG

BEGIN DIALOG FamTree_TitleSettings 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187,  BMPName + LTRIM$(STR(8)) + BMPExt 
	TEXT  140, 11, 142, 20, "Here's where you add your title and legend."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Next >"
	GROUPBOX  140, 33, 136, 36, "&Title"
	TEXTBOX  151, 45, 117, 13, TitleText$
	CHECKBOX  151, 84, 60, 11, "&Create Legend", LegendChB%
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5,""
END DIALOG

BEGIN DIALOG FamTree_PageSettings 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(9)) + BMPExt
	TEXT  135, 9, 151, 22, "Select your page settings."
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

BEGIN DIALOG FamTree_BoxSettings 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(10)) + BMPExt
	TEXT  140, 11, 142, 20, "Choose a box size, text and color options."
	GROUPBOX  160, 36, 92, 96, "Box Settings"
	OPTIONGROUP OptionBoxProperties%
		OPTIONBUTTON  172, 49, 45, 10, "&Fit to page"
		OPTIONBUTTON  172, 62, 38, 10, "&Custom"
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

BEGIN DIALOG FamTree_TextSettings 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 186, BMPName + LTRIM$(STR(11)) + BMPExt
	OKBUTTON  186, 171, 40, 12
	GROUPBOX  137, 6, 144, 96, "Text Options"
	GROUPBOX  137, 104, 144, 48, "Colors"
	TEXT  147, 22, 21, 8, "&Font:"
	DDLISTBOX  172, 22, 101, 96, FontsArr$, FontNameVal%
	TEXT  147, 44, 21, 8, "&Justify:"
	DDLISTBOX  172, 42, 101, 61, JustifyArr$, JustifyType%
	CHECKBOX  172, 62, 50, 10, "&Bold", BoldOptn%
	CHECKBOX  172, 75, 50, 9, "&Italic", ItalicOptn%
	CHECKBOX  172, 87, 50, 10, "&Underlined", UnderlinedOptn%
	TEXT  148, 136, 14, 8, "&Box:"
	DDLISTBOX  172, 135, 101, 69, ColorsArr$, BoxColor%
	TEXT  148, 116, 15, 8, "&Text:"
	DDLISTBOX  172, 116, 101, 73, ColorsArr$, TextColor%
	CANCELBUTTON  233, 171, 40, 12
END DIALOG

BEGIN DIALOG FamTree_PageFinish 288, 192, "Family Tree Wizard"
	IMAGE  2, 2, 130, 187, BMPName + LTRIM$(STR(12)) + BMPExt
	TEXT  140, 11, 142, 35, "We are now ready to make your personalized Family Tree.  Press Finish and watch FLOW and SCRIPT do all the work!"
	TEXT  140, 50, 142, 35, "Just imagine all of the things that you can do with the combined power of FLOW and Corel SCRIPT."
	PUSHBUTTON  146, 171, 40, 12, "< &Back"
	PUSHBUTTON  186, 171, 40, 12, "&Finish"
	CANCELBUTTON  233, 171, 40, 12
	GROUPBOX  135, 156, 151, 5, ""
END DIALOG

REM ************************************************************************************************************

REM MAIN PROGRAM

CONST FromFile = 1
CONST FromWizard=2
DIM FromWhere%

Page1:
REM ====================================================================
REM Dialog One - Welcome Dialog for Wizard
ret = DIALOG(FamTree_Page1)

REM If this is a Cancel button just Exit The Wizard
if ret=2 then Stop

Ogroup=0 'Default type - ThroughWizard

Page2:
REM ====================================================================
REM Dialog 2 - Type of the Chart
ret = DIALOG(FamTree_Page2)
if ret=2 then Stop 'If this is a Cancel button just Exit The Wizard
If ret=3 then goto Page1 'Back Button

REM We are now in Next Button
if ogroup=0 then goto ThroughWizard
if ogroup=1 then goto ExistingTextFile
Message "What Are U Doing Here?"
STOP


ExistingTextFile:
REM ====================================================================

ret = dialog(FamTree_PageFile)
If ret= 2 then Stop
IF ret= 3 then Goto Page2
IF ret= 4 then 'Next Button
	If TextBox1Txt = "" then 
		Message "No file has been entered."
		Goto ExistingTextFile	
	Else
 		If TexttoText(TextBox1Txt,0) = 0 Then Goto ExistingTextFile
		FamilyComboBoxArr(1)="-"
		ReportToNum=1
		For i=1 To MaxLength
			PersonsArr(i)  = Names(i,1)
			RelationArr(i) = Names(i,2)
			RelPersonArr(i)= Names(i,3)
			BornDateArr(i) = Names(i,4)
			DeadDateArr(i) = Names(i,5)
			Call AddToFamilyCBArr(PersonsArr(i))
			Call AddToFamilyCBArr(RelPersonArr(i))
			ListBoxItemsArr(i)=PersonsArr(i)+"/"+RelationArr(i)+"/"+RelPersonArr(i)+"/"+BornDateArr(i)+"/"+DeadDateArr(i)
		Next i
		LBSelectedPerson=1
		NumPersons=MaxLength
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
REM ================ Creating Generations =========================
ret = DIALOG(FamTree_PageBlank)
If ret=2 then Stop 'If this is a Cancel button just Exit The Wizard
If ret=3 then goto Page2 'Back Button

if Generations < 0 then 
	Message "The number you have given has a negative value."
	if Generations < 0 then Generations=0
	BEEP
	goto ThroughWizard
EndIf

if Generations > 6 then 
	Message "A maximum number of six generations can be accepted at one time."
	Generations=6
	BEEP
	goto ThroughWizard
EndIf

REM This is The Real Stuff - Writing Directly To Arrays
	FamilyComboBoxArr(1)="-"
	FamilyNum=1
	PersonsArr(1) = "Person"
	RelPersonArr(1)  = "-"
	RelationArr(1) = ""
	BornDateArr(1) = ""
	DeadDateArr(1) = ""
	
	If Generations = 0 then 
		ListBoxItemsArr(1) = ""
		NumPersons=0
		goto ThroughWizard2
	EndIf

	If Generations = 1 then 
		NumPersons=1
		goto Lab10
	EndIf

Dim PNum%
PNum=1
	For i=1 To Generations-1 
		For j=1 to 2^(i-1)
			PNum=PNum+1
			' Mothers
			PersonsArr(PNum) = "Mother"+Str(i)+Str(j)
			If i=1 then 
				RelPersonArr(PNum) = "Person"
			else
				If (j mod 2) = 0 then
					RelPersonArr(PNum) = "Mother"+Str(i-1)+Str((j+1)\2)
				else 
					RelPersonArr(PNum) = "Father"+Str(i-1)+Str((j+1)\2)
				EndIf
			EndIf	

			'Message PersonsArr(PNum)+" "+RelPersonArr(PNum)
			RelationArr(PNum) = "m"
			BornDateArr(PNum) = ""
			DeadDateArr(PNum) = ""
			PNum=PNum+1

			' Fathers
			PersonsArr(PNum) = "Father" + Str(i)+Str(j)
			If i=1 then 
				RelPersonArr(PNum) = "Person"
			else
				If (j mod 2) = 0 then
					RelPersonArr(PNum) = "Mother" + Str(i-1)+Str((j+1)\2)
				else 
					RelPersonArr(PNum) = "Father" + Str(i-1)+Str((j+1)\2)
				EndIf
			EndIf	

			'Message PersonsArr(PNum)+" "+RelPersonArr(PNum)
			RelationArr(PNum) = "f"
			BornDateArr(PNum) = ""
			DeadDateArr(PNum) = ""
		Next j
	Next i

	NumPersons=2^Generations-1

Lab10:
	LBSelectedPerson=1

	For i=1 to NumPersons
		ListBoxItemsArr(i)=PersonsArr(i)+"/"+RelationArr(i)+"/"+RelPersonArr(i)+"/"+BornDateArr(i)+"/"+DeadDateArr(i)
		Call AddToFamilyCBArr(PersonsArr(i))
		Call AddToFamilyCBArr(RelPersonArr(i))
	Next i

	ListBoxItemsArr(NumPersons+1)=""


REM ====================================================================
FromWhere=FromWizard

ThroughWizard2:
ret = DIALOG(FamTree_PageWizard)
If ret=2 then Stop               'If this is a Cancel button just Exit The Wizard
If ret=3 then goto ThroughWizard 'Back Button
LBSelectedPerson=1

Again:
ret = DIALOG(FamTree_PageWizardDisplay)
If ret=2 then Stop               'If this is a Cancel button just Exit The Wizard
If ret=3 then 'Back Button
	If FromWhere = FromWizard then goto ThroughWizard2
	If FromWhere = FromFile   then goto ExistingTextFile
	Message "Please try again."
	STOP
EndIf

If ret=4 then                    'Next Button
	If NumPersons<=0 then 
		Message "The chart is empty."
		goto Again
	EndIf

	If NoEditKey then goto NoCheck
	ret=CheckDescription()
	If PrintError(ret)=1 then goto Again

NoCheck:
	ret = PageAndFinish()
	if ret = 2 then Stop
	if ret = 3 then goto Again

	ret = TexttoText("",NumPersons)
	TransformFamChart(NumPersons)
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
            RelPersonName  = ""
	  else RelPersonName  = "-"
	EndIf

	BornDateText = ""
	DeadDateText = ""

NewAgain:
	ret = DIALOG(FamTree_PageWizardNew)
	If ret=2 then goto Again
     REM New Person is added!!!
	if NewPersonName = RelPersonName then
		Message "A person cannot be related to themselves."
		goto NewAgain
	EndIf
	If NumPersons > 0 then
		For j=1 To NumPersons 
			if NewPersonName=PersonsArr(j) then
				Message "There is already a person with this name!"
				goto NewAgain
			EndIf
		Next j
	EndIf

	NoEditKey=0

	NumPersons=NumPersons+1
	PersonsArr(NumPersons)  = NewPersonName
	RelPersonArr(NumPersons)= RelPersonName
	If RelationVal = 0 then
		RelationArr(NumPersons) = "m"
	else
		RelationArr(NumPersons) = "f"
	EndIf
	BornDateArr(NumPersons) = BornDateText
	DeadDateArr(NumPersons) = DeadDateText
	ListBoxItemsArr(NumPersons)=PersonsArr(NumPersons)+"/"+RelationArr(NumPersons)+"/"+RelPersonArr(NumPersons)+"/"+BornDateArr(NumPersons)+"/"+DeadDateArr(NumPersons)
	IF RelPersonName <> "" then Call AddToFamilyCBArr(RelPersonName)
	IF NewPersonName <> "" then Call AddToFamilyCBArr(NewPersonName)
	Goto Again
EndIf 'End of New Button

If ret=6 then 'Edit Button
	If LBSelectedPerson <= NumPersons then
		NewPersonName = PersonsArr(LBSelectedPerson)
		RelPersonName = RelPersonArr(LBSelectedPerson)
		BornDateText = BornDateArr(LBSelectedPerson)
		DeadDateText = DeadDateArr(LBSelectedPerson)

		If RelationArr(LBSelectedPerson) = "m" then
			RelationVal=0
		Else	
			RelationVal=1
		EndIf
	else goto Again
	EndIf
EditAgain:
	ret = DIALOG(FamTree_PageWizardNew)
	If ret=2 then goto Again

     REM This Person is edited!!!
	if NewPersonName = RelPersonName then
		Message "A person cannot be related to themself."
		goto EditAgain
	EndIf

	If NumPersons > 0 then
		For j=1 To NumPersons 
		   If j <> LBSelectedPerson then
			if NewPersonName=PersonsArr(j) then
				Message "There is already a person with this name!"
				goto NewAgain
			EndIf
             EndIf
		Next j
	EndIf

	NoEditKey=0

	PName  = PersonsArr(LBSelectedPerson)
	RelName= RelPersonArr(LBSelectedPerson)

 	PersonsArr(LBSelectedPerson)  = NewPersonName
	RelPersonArr(LBSelectedPerson)= RelPersonName

	If RelationVal = 0 then
		RelationArr(LBSelectedPerson) = "m"
	else
		RelationArr(LBSelectedPerson) = "f"
	EndIf

	BornDateArr(LBSelectedPerson) = BornDateText
	DeadDateArr(LBSelectedPerson) = DeadDateText
	ListBoxItemsArr(LBSelectedPerson)=PersonsArr(LBSelectedPerson)+"/"+RelationArr(LBSelectedPerson)+"/"+RelPersonArr(LBSelectedPerson)+"/"+BornDateArr(LBSelectedPerson)+"/"+DeadDateArr(LBSelectedPerson)

	'try to delete from ComboBox arr
	Call DelFromFamilyCBArr(PName)
	Call	DelFromFamilyCBArr(RelName)

	Call AddToFamilyCBArr(NewPersonName)
	Call AddToFamilyCBArr(RelPersonName)

	Goto Again
EndIf 'End of Edit Button

If ret=7 then 'Delete Button
	If LBSelectedPerson <= 0 then goto Again 'If there is no selection in the LB
	If LBSelectedPerson > NumPersons then goto Again
	If NumPersons <= 0 then goto Again 'Yes, yes, I know that this is stupid!

	'first remember the names for deleting
	PName = PersonsArr(LBSelectedPerson)
	RelName = RelPersonArr(LBSelectedPerson)

	'the real stuff
	For j=LBSelectedPerson To NumPersons-1
		PersonsArr(j)   = PersonsArr(j+1)
		RelPersonArr(j) = RelPersonArr(j+1)
		BornDateArr(j)  = BornDateArr(j+1)
		DeadDateArr(j)  = DeadDateArr(j+1)
		RelationArr(j)  = RelationArr(j+1)
		ListBoxItemsArr(j)=PersonsArr(j)+"/"+RelationArr(j)+"/"+RelPersonArr(j)+"/"+BornDateArr(j)+"/"+DeadDateArr(j)
	Next j

	ListBoxItemsArr(NumPersons)=""
	NumPersons=NumPersons-1	

	'try to delete from ComboBox arr
	Call DelFromFamilyCBArr(PName)
	Call	DelFromFamilyCBArr(RelName)

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

	If PrintError(ret)=1 then goto Again

	'It looks like everything is OK
	Filename$=GetFileBox ("*.txt","Save",1) 
	REM This is The Real Stuff - Writing to file

	IF Filename="" then goto Again
	Open Filename For Output As 1
	For i=1 To NumPersons
		Write #1, PersonsArr(i),RelationArr(i),RelPersonArr(i),BornDateArr(i),DeadDateArr(i) 
	Next i
	Close #1

	NoEditKey=1

	Goto Again
EndIf

Message "What Are U Doing Here?"
Stop


FUNCTION PageAndFinish 

TitleSettings:
	ret = DIALOG(FamTree_TitleSettings)

	if ret = 2 OR ret = 3 then ' Cancel or Prev Buttons
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf

WizardPageSettings:
	REM =============================================================
	REM The last step before complete final???
	
	ret = DIALOG(FamTree_PageSettings)

	if ret = 2 then ' Cancel Button
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf

invalid:
	IF ret=3 then Goto TitleSettings

	if OptionPaperSize = 4 then
		if dPaperHeight <= 0 or dPaperWidth <= 0 then
			message "Paper Size must be greater than 0"
			GOTO WizardPageSettings
		end if
	end if

WizardBoxSettings:
	ret = DIALOG(FamTree_BoxSettings)

	if ret = 2 then ' Cancel Button
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf

	IF ret=3 then Goto WizardPageSettings

	If ret=5 then 
		ret = DIALOG(FamTree_TextSettings)
		Goto WizardBoxSettings
	Endif

	if OptionBoxProperties = 1 then
		if dBoxHeight = 0 or dBoxWidth = 0 or dBoxDistance = 0 then
			message "Box properties cannot be set to zero"
			GOTO WizardBoxSettings
		end if
	end if

	REM =============================================================
	ret = DIALOG(FamTree_PageFinish)

	if ret = 2 then ' Cancel Button
		PageAndFinish% = ret
		goto EndFunc 	
	EndIf
	
	IF ret=3 then Goto WizardBoxSettings
	

	PageAndFinish% = ret

EndFunc:
END FUNCTION


REM ***********************************************************************
SUB AddToFamilyCBArr (NewName)

	If NewName="" then goto EndOfThisSub
	For j=1 To FamilyNum 
		if NewName = FamilyComboBoxArr(j) then goto EndOfThisSub
	Next j
	FamilyNum = FamilyNum+1
	FamilyComboBoxArr(FamilyNum)=NewName

EndOfThisSub:
End SUB


REM ***********************************************************************
SUB DelFromFamilyCBArr (DelName)


	If DelName="" then goto EndOfThisSub
	If FamilyNum < 2 then goto EndOfThisSub

	'We will delete him/her only if he/she is not into any array
	For i=1 to NumPersons
		If RelPersonArr(i) = DelName then goto EndOfThisSub
		If PersonsArr(i) = DelName then goto EndOfThisSub
	Next i

	For i=2 to FamilyNum
		If FamilyComboBoxArr(i)=DelName then
			j=i
			goto et1
		EndIf
	Next i
	' This string is not in the array
	goto EndOfThisSub	
Et1:
	For i=j To FamilyNum-1		
		FamilyComboBoxArr(i)=FamilyComboBoxArr(i+1)
	Next i
	FamilyComboBoxArr(FamilyNum)=""
	FamilyNum = FamilyNum-1		
EndOfThisSub:
End SUB


REM ***********************************************************************

FUNCTION TexttoText (File_Name$, NumLines%)

	If File_Name <> "" then Goto OpenFile

	REM Tranfer directly the data between arrays

	For i=1 To NumLines
		Names(i,1) = PersonsArr(i)
		Names(i,2) = RelPersonArr(i)
		Names(i,3) = RelationArr(i)
		Names(i,4) = BornDateArr(i)
		Names(i,5) = DeadDateArr(i)
	Next i
	MaxLength=NumLines

DIM Mother%, Father%
DIM Tmp$

	'Reorganize the arrays
	For i = 1 to MaxLength
		Mother=-1
		Father=-1
		For j=1 to MaxLength
			If PersonsArr(i)=RelPersonArr(j) then
				If RelationArr(j)="m" OR RelationArr(j)="M" then Mother = j
				If RelationArr(j)="f" OR RelationArr(j)="F" then Father = j
			EndIf
		Next j

		If Mother > 0 AND Father > 0 AND Father < Mother then
			For j=1 To 5
				Tmp = Names(Mother,j)
				Names(Mother,j)=Names(Father,j)
				Names(Father,j)=Tmp
			Next j

		EndIf
	Next i
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

	If MaxLength MOD 5 > 0 then
		Message "This file is corrupt."+CHR(13)+"Some lines may contain more or less than 5 fields."
		TextToText = 0
		Goto EndTTT				
	EndIf

	MaxLength=MaxLength/5
 
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
		Input #1, Names(Cc,1), Names(Cc,2), Names(Cc,3), Names(Cc,4), Names(Cc,5)
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
SortedNames(1,4)=TopPerson		
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
		SortedNames(Num,4)=i
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
REM               -4    - if somebody has more that one mother
REM               -5    - if somebody has more that one father
REM               -6    - if there is relations <> "f" and "m"

Function CheckDescription%
Dim Marks%(Dimension)
Dim Ver%(Dimension)
NumList=0
Num%=0
Dim Key%
DIM Mother%, Father%

	For i=1 To NumPersons
		If PersonsArr(i)=RelPersonArr(i) then 
			CheckDescription = -2
			ErrLine=i
			Goto EndLab
		EndIf
		If RelPersonArr(i)="-" then 
       		Num=Num+1
			ErrLine=i
		EndIf
		Mother = 0
		Father = 0
		For j=1 To NumPersons
			If PersonsArr(i)=RelPersonArr(j) then
				If RelationArr(j)="m" OR RelationArr(j)="M" then 
					Mother = Mother+1
				ElseIf RelationArr(j)="f" OR RelationArr(j)="F" then 
					Father = Father+1
				Elseif RelPersonArr(i) <> "-" then
					CheckDescription = -6
					ErrLine=j
					Goto EndLab
				EndIf
			EndIf
		Next j
		If Mother > 1 then
			CheckDescription = -4
			ErrLine=i
			Goto EndLab
		EndIf
		If Father > 1 then
			CheckDescription = -5
			ErrLine=i
			Goto EndLab
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
		If RelPersonArr(i)="-" then 
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
  			 If PersonsArr(i)=RelPersonArr(j) then 
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
DIM VerOffset%

VerOffset=0

if OptionPaperSize = 0 and OptionBoxProperties = 0 then
	dBoxHeight   = 1
	dBoxWidth    = 2
	dBoxDistance = 1

	dPaperHeight = (gMaxVirtualBox+2)*dBoxHeight + (gMaxVirtualBox+1)*dBoxDistance + 2
	dPaperWidth  = gMaxLevel*dBoxWidth + (gMaxLevel-1)*dBoxDistance + 2
     VerOffset=1
elseif OptionPaperSize = 0 then
	dPaperHeight = (gMaxVirtualBox+2)*dBoxHeight + (gMaxVirtualBox+1)*dBoxDistance + 2
	dPaperWidth  = gMaxLevel*dBoxWidth + (gMaxLevel-1)*dBoxDistance + 2
     VerOffset=1
elseif OptionPaperSize = 1 then
	dPaperHeight = 11
	dPaperWidth  = 8.5
elseif OptionPaperSize = 2 then
	dPaperHeight = 8.5
	dPaperWidth  = 11
end if

if OptionBoxProperties = 0 and OptionPaperSize <> 0 then
	dBoxHeight  = dPaperHeight/(2*(gMaxVirtualBox+2)-1)
	dBoxWidth = dPaperWidth/(2*gMaxLevel-1)
	If dBoxHeight < dBoxWidth then 
              dBoxDistance = dBoxHeight
   	    else dBoxDistance = dBoxWidth
	EndIf 
end if

dStartLeft = -(dPaperWidth/2 - (dPaperWidth - gMaxLevel * dBoxWidth - (gMaxLevel - 1) * dBoxDistance)/2)

CreateFamilyTree dBoxHeight, dBoxDistance
DrawFamilyTree dPaperHeight, dPaperWidth, dStartLeft, dBoxHeight, dBoxWidth, dBoxDistance
ConnectObjects dBoxDistance


WITHOBJECT FLOW
REM Making Legend
	If LegendChB <> 0 then 
			.CreateTextString LegendText, dPaperWidth/2-dBoxWidth,-dPaperHeight/2+dBoxHeight+VerOffset,dPaperWidth/2,-dPaperHeight/2+VerOffset
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
		.CreateTextString TitleText, -dPaperWidth/4, dPaperHeight/2-VerOffset,dPaperWidth/4,dPaperHeight/2-dBoxHeight-VerOffset
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


SUB ConnectObjects(BoxDistance#)
	DIM iNewNode as integer
	DIM iNode as integer

	iNode = 2

Errcount = 0
Retry2:

Errcount = ErrCount + 1
On error goto Retry2
if (Errcount = 4) then
	Message("Internal error.  Please Re-run script.")
	PrintError 0
else
	WITHOBJECT FLOW
	.SetAutoRouting TRUE, 0.05, 0.05, 50
	while ST_level(iNode) > 1
		if ST_level(iNode) > ST_level(iNode-1) then
			iNewNode = iNode - 1
		else
			iNewNode = iNode
			while ST_level(iNode) <= ST_level(iNewNode-1)
				iNewNode = iNewNode - 1
			wend
			iNewNode = iNewNode - 1
		end if
			if TopConnectorY(iNewNode) < LeftConnectorY(iNode) then
				.CreateRoutedLine TopConnectorX(iNewNode), TopConnectorY(iNewNode), LeftConnectorX(iNode), LeftConnectorY(iNode), -1, 0
				.EndLineCurveAtPoint LeftConnectorX(iNode), LeftConnectorY(iNode)
			elseif BottomConnectorY(iNewNode) > LeftConnectorY(iNode) then
				.CreateRoutedLine BottomConnectorX(iNewNode), BottomConnectorY(iNewNode), LeftConnectorX(iNode), LeftConnectorY(iNode), -1, 0
				.EndLineCurveAtPoint LeftConnectorX(iNode), LeftConnectorY(iNode)
			else 
				.CreateRoutedLine RightConnectorX(iNewNode), RightConnectorY(iNewNode), LeftConnectorX(iNode), LeftConnectorY(iNode), -1, 0
				.EndLineCurveAtPoint LeftConnectorX(iNode), LeftConnectorY(iNode)
			end if
		iNode = iNode + 1
	Wend
	END WITHOBJECT
end if
On Error Exit
END SUB 

'ConnectObjects
' CreateFamilyTree traverses the array starting the children of a node first i.e. the left most node on 
' a tree. It then traverses back to its siblings, and its parents.


SUB CreateFamilyTree(BoxHeight#, BoxDistance#)
	DIM dBottomMostCoordinate as double
	DIM iNode as integer
	DIM iTmpNode as integer

	done = false
	iNode = 1

	' bottom edge of the bottom most box in the family tree
	dBottomMostCoordinate = -(gMaxVirtualBox*BoxHeight + (gMaxVirtualBox-1)*BoxDistance)/2 - BoxDistance
	if NumChildren(iNode) > 0 then
		while done = false
			if ST_Level(iNode+1) > ST_Level(iNode) and IsSet(iNode+1) = false then
				iNode=iNode+1
			' no kids
			elseif ST_Level(iNode+1) <= ST_Level(iNode) and IsSet(iNode) = false then
				IsSet(iNode) = true
				Top_Coordinate(iNode) = dBottomMostCoordinate + BoxDistance
				dBottomMostCoordinate = Top_Coordinate(iNode) + BoxHeight

				if iNode = 1 then
					done=true
				else
					iNode = FindTopUnSet(iNode)
				end if
			elseif (ST_Level(iNode+1) > ST_Level(iNode)) and (IsSet(iNode+1) <> false) then
				iTmpNode=findFirstUnsetChild(iNode)
				' kids are drawn
				if iTmpNode < 0 then
					IsSet(iNode) = true
					' find mid of kids and draw
					Top_Coordinate(iNode) = MidPoint(iNode, dBoxHeight)

					if iNode = 1 then
						done = true
					else
						iNode = FindTopUnSet(iNode)
					end if
				else
					iNode = iTmpNode
				end if
			end if
		wend
	else
		Top_Coordinate(iNode) = -BoxHeight/2
	end if
END SUB 'CreateFamilyTree


'DrawFamilyTree sets up the environment and draws the entire tree based on the ST_Level arrays

SUB DrawFamilyTree(PaperHeight#, PaperWidth#, StartLeft#, BoxHeight#, BoxWidth#, BoxDistance#)
	DIM iNode as integer
	DIM CenterY#	
	DIM CenterX#

	WITHOBJECT FLOW
 		.SetVisible true
		.FileNew
		.SetUnits 2
		.SetPageLayOut true, PaperWidth, PaperHeight, 0, 0, 0, 0
          .SetRuler 2, 2, 0, 0
		.SetRefresh FALSE

		iNode = 1
		while ST_Level(inode) > 0
			dLeft = StartLeft + ((ST_Level(iNode) - 1) * BoxWidth + (ST_Level(iNode) - 1) * BoxDistance)
			dRight = dLeft + BoxWidth

			.CreateRectangle dLeft, Top_Coordinate(iNode)+BoxHeight, dRight, Top_Coordinate(iNode), label(iNode)

			if label(iNode) <> "" then
				.SetObjectText label(iNode)+CHR(13)+ST_Remark(iNode), label(iNode)
			end if
			.ObjectSelectNone

			CenterX = dLeft + BoxWidth/2
			CenterY = Top_Coordinate(iNode) + BoxHeight/2

'Top
			TopConnectorX(iNode) = CenterX
			TopConnectorY(iNode) = CenterY + BoxHeight/2
			.CreateConnector CenterX, CenterY
			.MoveConnector CenterX, CenterY, TopConnectorX(iNode), TopConnectorY(iNode) 

'Bottom
			BottomConnectorX(iNode) = TopConnectorX(iNode)
			BottomConnectorY(iNode) = CenterY - BoxHeight/2
			.CreateConnector CenterX, CenterY
			.MoveConnector CenterX, CenterY, BottomConnectorX(iNode), BottomConnectorY(iNode) 

'Left
			LeftConnectorX(iNode) = CenterX - BoxWidth/2
			LeftConnectorY(iNode) = CenterY
			.CreateConnector CenterX, CenterY
			.MoveConnector CenterX, CenterY, LeftConnectorX(iNode), LeftConnectorY(iNode) 

'Bottom
			RightConnectorX(iNode) = CenterX + BoxWidth/2
			RightConnectorY(iNode) = CenterY
			.CreateConnector CenterX, CenterY
			.MoveConnector CenterX, CenterY, RightConnectorX(iNode), RightConnectorY(iNode) 

			iNode = iNode + 1
		wend
	END WITHOBJECT
END SUB 'DrawFamilyTree


' Find the first child of "node" who has not been drawn.  This basically goes through
' "ST_Level" array and checks for nodes that are a level below "node" and that have not
' been set according to the "isset" array.  Note, the loop should stop when it has
' finished reading all entries (i.e. ST_Level[i] <> 0) or when the first child that has
' not been drawn is found or when another node of the same level is encountered.
' Returns -1 if no child was found or the index of the child.

FUNCTION FindFirstUnSetChild(iNode%) as integer
	DIM i as integer
	DIM iNewNode as integer

	i = iNode + 1
	done = false
	iNewNode = -1

	while ST_Level(i) <> 0 and done = false
		' i has the same level
		if (i) = ST_Level(iNode) then
			' no unset child
			done = true
		' i is a child and has not been set
		elseif ST_Level(i) = ST_Level(iNode) + 1 and IsSet(i) = false then
			done = true
			iNewNode = i
		end if
		i = i + 1
	wend
	FindFirstUnSetChild = iNewNode
END FUNCTION 'FindFirstUnSetChild


' Finds the first node of a higher level that has not been set.  It goes backwards through the
' ST_Level array until it hits a node that has not bee set.  A -1 is returned if no such node is
' found otherwise the index to the node is returned.   

FUNCTION FindTopUnSet(iNode%) as integer
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
	FindTopUnSet = iNewNode
END FUNCTION 'FindTopUnSet


FUNCTION GetMaxNum as integer
	DIM iNum as integer
	iNum = NumDeptOnEachLevel(1)
	for i = 2 to gMaxLevel
		if NumDeptOnEachLevel(i) > NumDeptOnEachLevel(i-1) then
			iNum = NumDeptOnEachLevel(i)
	   end if
	next
	GetMaxNum = iNum
END FUNCTION 'GetMaxNum


' Calculates the top coordinate for a parent from the top and bottom coordinates from its children.
' If there are an odd number of children, then the Top_Coordinate of the middle child is used as 
' the top coordinate of the parent otherwise, a formula is used to calculate the parent's Top_Coordinate.

FUNCTION MidPoint(iNode%, BoxHeight#) as double
	DIM i as integer
	DIM dTopMost  as double
	DIM dBottomMost as double

	i = iNode + 1
	first = false

	while ST_Level(i) <> 0 and ST_Level(i) > ST_Level(iNode)
		if ST_Level(i) = ST_Level(iNode)+1 then
			if (first = false) then
				dTopMost = Top_Coordinate(i)
				first = true
			end if
			dBottomMost = Top_Coordinate(i) + BoxHeight
		end if
		i = i+1
	wend
	MidPoint = dTopMost + ((dBottomMost - dTopMost)/2 - BoxHeight/2)
END FUNCTION 'MidPoint


' Calculates the number of children a particular node has.  It counts the number of level[node]+1 entries
' there are between this node and the end of the array or the next node of the same level, which ever one
' comes first.

FUNCTION NumChildren(iNode%) as integer
	DIM i as integer
	DIM iCount as integer

	i = iNode+1
	iCount = 0

	' loop exists when end of entries
	while (ST_Level(i) = ST_Level(iNode)+1) or (ST_Level(i) > ST_Level(iNode))
		if (ST_Level(i) = ST_Level(iNode)+1) then
			iCount = iCount+1
		end if
		i=i+1
	wend
	NumChildren = iCount
END FUNCTION 'NumChildren


SUB TransformFamChart(NumRec%)
Dim Num%
CurrentLevel%=0
gMaxVirtualBox = 1
DIM j%

	For i=1 To NumRec
		Num = Val(SortedNames(i,3))
 		if Num > 1 then
 			if Num <= CurrentLevel then gMaxVirtualBox = gMaxVirtualBox + 1
 		end if
 
 	    if Num > gMaxLevel then
     	    gMaxLevel = Num
 	    end if
 
 	    ST_level(i) = Num
 	    NumDeptOnEachLevel(Num) = NumDeptOnEachLevel(Num) + 1

	    'Stores the text	
	    Label(i)     = SortedNames(i,1)
	    j = VAL(SortedNames(i,4)) 
	    ST_Remark(i) = "b:" + Names(j,4) + CHR(13) + "d:" + Names(j,5)
	    CurrentLevel = Num		
	Next i

END SUB 'TransformFamTree


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


FUNCTION PrintError%(err%)

	If err=-1 then
		Message "There is no top person!"
		goto ErrEnd
	EndIf

	If err=-2 then
		Message "There is somebody who is related themelf!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto ErrEnd
	EndIf

	If err=-3 then
		Message "There is more than one top person!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto ErrEnd
	EndIf

	If err>0 then
		Message "There is someone related to somebody who is not in the chart!"+CHR(13)+"Take a look at "+PersonsArr(err)+"."
		goto ErrEnd
	EndIf

	If err=-4 then
		Message "There is someone who has more that one mother!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto ErrEnd
	EndIf

	If err=-5 then
		Message "There is someone who has more that one father!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto ErrEnd
	EndIf

	If err=-6 then
		Message "There is relations different than 'm' or 'f'!"+CHR(13)+"Take a look at "+PersonsArr(ErrLine)+"."
		goto ErrEnd
	EndIf

	PrintError=0
	Goto EndFn

ErrEnd:
	PrintError=1
EndFn:

END FUNCTION

