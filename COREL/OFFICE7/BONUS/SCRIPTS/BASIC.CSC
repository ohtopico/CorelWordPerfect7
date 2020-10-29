REM This script demonstrates some basic functionality
REM Basic.csc 19 MAY, 1995


DECLARE SUB setvars(strin$)                       'subroutine declaration. Sub routine sets the value of a global variable
DECLARE FUNCTION settovalue(z1 AS INTEGER) AS INTEGER             'FUNCTION declaration the FUNCTION sets the variable


'=====================================================
' Variable declaration using the DIM statement,
' the %, $, & ... suffix or with implicit declarations

	DIM arr(5) AS STRING	'array of STRINGs (5 elements)
	DIM a(10) AS STRING
	DIM b$
	c%=1
	d="Hello"		'STRING set to Hello


' We also have constants

	CONST e=" 'e' is a constant STRING"		'STRING constant




'=====================================================
' Control Statements
'=====================================================


'=====================================================
' Gotos and labels

	GOTO mylabel

elsewhere:			'label FOR goto statement
	GOTO last		'note: Never used

mylabel:

'=====================================================
' While loop example

	WHILE a1%<3		'count up TO 2
		MESSAGE "This is Fun!"
		a1%=a1%+1
	WEND			'end while

	MESSAGE "This is no fun anymore."
		
	
'=====================================================
' Do loop example

	a(1) = "COREL "
	a(2) = "SCRIPT "	
	a(3) = "Macro Language"
	a(4) = "END"

	c=1
	b=""
	DO 			'Assembles a string (b= "COREL SCRIPT Macro Language")
		b = b + a(c)
		c = c + 1
	LOOP UNTIL c=4	  'end do loop

'=====================================================
' if and for examples

	IF c<>1 THEN
		' we had enough !
		MESSAGE	"This is inside an if statement"
	ELSE
		' We need another loop example
		FOR c=1 TO 3
			MESSAGE str(c)
		NEXT c
	END IF


'=====================================================
' Dialog and Select example

	arr$(1) = "black"			'setting each element of the existing array TO a color
	arr$(2) = "red"
	arr$(3) = "white"
	arr$(4) = "blue"
	arr$(5) = "green"


index% = settovalue(1)		'using a FUNCTIONto ASsign a value TO index
setvars "This is inside a subroutine"				'

BEGIN DIALOG combodlg 144, 68, "List Example"		'definition of dialog box
	TEXT           4, 4, 90, 8, "&List:"
	LISTBOX        4, 14, 90, 50, arr$, index%
	OKBUTTON       100, 4, 40, 14
	CANCELBUTTON   100, 20, 40, 14
END DIALOG

dlg:
ret% = DIALOG(combodlg)		'returns the button pressed (1 for OK, 2 for Cancel)

' If Cancel is selected, stop the macro
	SELECT CASE ret%     'selects action based on which button is pressed
		CASE 1
			MESSAGE "You chose " + arr$(index%)    
		CASE 2 
			MESSAGE "You selected cancel!"
			goto last
		CASE ELSE			'impossible
			MESSAGE "Try again"
			GOTO dlg
	END SELECT	

last:
'=====================================================
' subs and functions
SUB setvars (strin$)
	MESSAGE strin
END SUB

FUNCTION settovalue(v%)
	MESSAGE "This is function #" + STR(v)
	settovalue=v%
END FUNCTION 

