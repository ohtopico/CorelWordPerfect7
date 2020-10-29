REM Uses all UI controls. If you did not install to C:\Corel
REM you must edit this file in Script Editor.
REM UI.csc 30 JUNE, 1995

REM This script uses bitmaps installed with Corel PhotoPaint. 
REM The path c:\corel is hardcoded. If you didn't install to c:\corel, 
REM Then you must change the InstallPath variable to your install directory.

InstallPath = "C:\Corel"
DIM bmparray$(5)
DIM color(8) as string
bmpname = InstallPath + "\photopnt\plgbrush\footprnt.bmp"
bmparray(1) = InstallPath + "\photopnt\plgbrush\fuzzy.bmp"
bmparray(2) = InstallPath + "\photopnt\plgbrush\boxpanel.bmp"
bmparray(3) = InstallPath + "\photopnt\plgbrush\textart.bmp"
bmparray(4) = InstallPath + "\photopnt\plgbrush\treebare.bmp"
bmparray(5) = InstallPath + "\photopnt\plgbrush\saturn.bmp"
color(1) = "Red"
color(2) = "Blue"
color(3) = "Yellow"
color(4) = "Green"
color(5) = "Purple"
color(6) = "Brown"
color(7) = "White"
color(8) = "Black"
'***********************************Dialog declaration
text1 = "This is text"
help = "HELP!"
push1 = "Push button"
BEGIN DIALOG d1 300, 210, "ALL CONTROLS example"
	OKBUTTON 250, 10, 45, 15
	CANCELBUTTON 250, 31, 45, 15
	HELPBUTTON 250, 53, 45, 15, help, HelpID%
	PUSHBUTTON 250, 74, 45, 15, push1
	TEXT 10, 10, 80, 15, text1	
	TEXTBOX 10, 20, 75, 15, textbox1$
	GROUPBOX 10, 40, 78, 49, "Group Box"
		OPTIONGROUP group1%
			OPTIONBUTTON 15, 50, 66, 15, "Option Button #1"
			OPTIONBUTTON 15, 65, 66, 15, "Option Button #2"
	GROUPBOX 95, 40, 75, 49, "Group Box"
		CHECKBOX 100, 50, 65, 15, "Check Button #1", binary1%
		CHECKBOX 100, 65, 65, 15, "Check Button #2", binary2%
	TEXT 10, 92, 70, 15, "Listbox"
	LISTBOX 10, 102, 78, 70, Color, listbox1%
	TEXT 95, 92, 70, 15, "Combobox"
	COMBOBOX 95, 102, 75, 70, Color, combobox1$
	TEXT 10, 175, 70, 15, "Drop Down Listbox"
	DDLISTBOX 10, 185, 70, 70, Color, ddlistbox1%
	TEXT 95, 175, 75, 15, "Drop Down Combobox"
	DDCOMBOBOX 95, 185, 70, 70, Color, ddcombobox1$
	TEXT 95, 10, 80, 15, "Spin Control: "
	SPINCONTROL 95, 20, 40, 15, spin1%
	IMAGE 190, 102, 100, 95, Bmpname
	GROUPBOX 175, 6, 70, 83, "Picture List"
		IMAGELISTBOX 180, 16, 60, 100, bmparray, imglst%
END DIALOG
'***************************Set defaults
textbox1 = "This is a textbox"
group1 = 1
binary1 = -1
binary2 = 1
binary3 = 0
Listbox1 = 2
Combobox1 = color(3)
DDListBox1 = 1
DDCombobox1 = color(5)
spin1 = 9
imglst = 2
TRYAGAIN:
ret = DIALOG(d1)
IF ret = 2 THEN STOP
IF ret = 3 THEN 
	MESSAGE "You pressed a push button"
	GOTO TRYAGAIN
END IF
mess$ = "You entered " + CHR(34) + textbox1 + CHR(34) + " in the textbox"
mess = mess + CHR(13) + "You picked" + STR(spin1) + " in the spin box"
mess$ = mess + CHR(13) + "You selected Option Button #" + CSTR(group1 + 1)
mess = mess + CHR(13) + "You selected:"
IF binary1<>0 THEN mess = mess + " Check Button #1"
IF binary2<>0 THEN mess = mess + " Check Button #2"
mess = mess + CHR(13) + "You picked the color " + color(Listbox1) + " in the ListBox"
mess = mess + CHR(13) + "You entered " + Combobox1 + " in the ComboBox"
mess = mess + CHR(13) + "You entered " + DDCombobox1 + " in the DDComboBox"
mess = mess + CHR(13) + "You picked the color " + color(DDListbox1) + " in the DDListBox"
mess = mess + CHR(13) + "You picked " + bmparray(imglst) + " as your image"
MESSAGE mess
