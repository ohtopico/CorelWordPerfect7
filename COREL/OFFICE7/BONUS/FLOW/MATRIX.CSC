REM Matrix Creation Script
REM Matrix.csc


'A sample script for tiling objects.  Remember that ONLY one object
' must be selected in order for this script to work right



' some global variables we will use

rowval% = 3 'for the row number
colval% = 2 'for the column number
spaceval% = 15 ' for the space interval
go% = -1
groupval% = 1 'whether we want the result grouped

BEGIN DIALOG Dialog1 268, 97, "Matrix Object Script"
	TEXT  37, 4, 194, 13, "One object must be selected for this script to work properly"
	SPINCONTROL  104, 19, 26, 14, rowval%
	SPINCONTROL  104, 41, 26, 14, colval%
	SPINCONTROL  104, 64, 26, 14, spaceval%
	TEXT  16, 20, 71, 13, "Number of rows:"
	TEXT  16, 43, 80, 14, "Number of columns:"
	TEXT  16, 65, 80, 11, "Space between objects"
	TEXT  137, 65, 100, 10, "in tenths of a point"
	CHECKBOX  16, 84, 10, 8, "group", groupval
	TEXT  28, 84, 76, 8, "Group Objects in Matrix"
	PUSHBUTTON  160, 20, 99, 32, "Click here to create the matrix"
END DIALOG

Errcount% = 0


ret = DIALOG(Dialog1)
if ret = 3 then 
	numrow% = (rowval%) 'get the number of rows and columns
	numcol% = (colval%)
	objsp% = (spaceval%)

	'convert from tenths of a point to points
	if (objsp = 0) then
		objspace# = 0
	else
		objspace# = objsp / 10
	endif
Retry:
	Errcount = ErrCount + 1
	On error goto Retry
	if (Errcount = 4) then
		Message("Internal error.  Please Re-run script.")
		goto Last
	else
	WITHOBJECT FLOW
		 go = .GetIsObjectSelected()
	END WITHOBJECT
	end if
	On Error Exit
	
	'if no object is selected, stop
	IF (NOT(go = -1)) then
		Message "You need to select an object"
		GOTO LAST
	end if 

	if ((numcol > 0) AND (numrow > 0) AND (objsp >= 0)) then
		'if all the values are correct, continue
		GOTO APPLYSET
	else
		Message "Incorrect values entered"
		GOTO LAST
	endif
else
'in case the script was accidentally terminated
	goto last
endif


APPLYSET:

'get the size of our original object
	WITHOBJECT FLOW
		oldunits% = .GetUnits()
		.GetObjectRectangle leftset#, topset#, rightset#, bottomset#
		.GetCenter cenx#, ceny#
		.EditCut
	END WITHOBJECT
	
'determine the width and height	
	width# = abs(rightset - leftset)
	height# = abs(topset - bottomset)
	
'store values for the upcoming iterations
	oldleft# = leftset
	oldtop# = topset
	oldright# = rightset
	oldbottom# = bottomset
	origcenx# = cenx
	origceny# = ceny

	FOR i% = 1 TO numrow STEP 1
		FOR j% = 1 TO numcol STEP 1
			WITHOBJECT FLOW
				.ObjectSelectNone
				'paste the objects in the desired tile pattern
				.EditPaste (origcenx + (width) + objspace), origceny
				.GetCenter origcenx, origceny
			END WITHOBJECT
		NEXT j%
		'save our old values 
		oldx# = cenx
		oldy# = ceny
		origceny = (origceny - (height) - objspace)
		origcenx = cenx
	NEXT i%
	
	'select the entire region
	if (NOT(groupval = 0)) then 
		WITHOBJECT FLOW
			.SelectAllObjects
			.ArrangeGroup
		END WITHOBJECT
	end if
	
	WITHOBJECT FLOW
		'set the units back to their original settings
		.SetUnits oldunits
	END WITHOBJECT

LAST:
