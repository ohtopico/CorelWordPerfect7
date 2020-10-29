REM Calculator program written in CorelSCRIPT
REM Calc.csc 24 MAY, 1995
REM 

DIM arr AS STRING
DIM first AS LONG, second AS LONG
arr = " "    	'default value for display
flag = 1		'flag to display calculator
first = 0		'Initialise the main value to 0
second =1		'Initialise the temp value to 1
operation  = 1
DIM clearflag AS INTEGER
clearflag = 1	'Flag to clear display

BEGIN DIALOG Dialog1 122, 145, "Calculator"	'definition of calculator dialog box

	PUSHBUTTON     65, 95, 17, 14, "="			'first so that it is default (ie. Enter key means =)
	PUSHBUTTON     37, 95, 17, 14, "&0"			'Returns 4

	PUSHBUTTON     8, 74, 18, 14, "&1"			'Returns 5

	PUSHBUTTON     37, 74, 17, 14, "&2"			'Returns 6

	PUSHBUTTON     65, 74, 18, 14, "&3"			'Returns 7

	PUSHBUTTON     8, 54, 18, 14, "&4"			'Returns 8

	PUSHBUTTON     37, 54, 17, 14, "&5"			'Returns 9

	PUSHBUTTON     65, 54, 18, 14, "&6"			'Returns 10

	PUSHBUTTON     8, 34, 17, 14, "&7"			'Returns 11

	PUSHBUTTON     37, 34, 17, 14, "&8"			'Returns 12

	PUSHBUTTON     65, 34, 17, 14, "&9"			'Returns 13

	PUSHBUTTON     16, 123, 30, 14, "&C"		'Returns 14

	PUSHBUTTON     94, 34, 17, 14, "&/"			'Returns 15

	PUSHBUTTON     94, 54, 17, 14, "&*"			'Returns 16

	PUSHBUTTON     94, 74, 17, 14, "&--"		'Returns 17

	PUSHBUTTON     94, 95, 17, 14, "&+"			'Returns 18

	PUSHBUTTON	   73, 123, 30, 14,"E&xit"		'Returns 19

	PUSHBUTTON     8, 95, 17, 14, "+/-"			'Returns 20

	GROUPBOX       3, 25, 116, 93, ""

 	TEXTBOX        8, 6, 108, 15, arr

END DIALOG

DO WHILE flag =1 		'display calculator until flag = 0
	
	ret = DIALOG(Dialog1)		'returns the button pressed
	SELECT CASE ret				'selects action based on which button is pressed

		CASE 4 TO 13			'if a number is pressed
			IF clearflag = 1 THEN 	'clears the display if the clearflag = 1
				clearflag = 0
				arr = ""
			END IF
			arr = arr + CSTR(ret - 4)	'adds the number pressed to the end of the display

		CASE 14				'clear button
			arr = ""
			first = 0

		CASE 3, 15 TO 18		'One of the operations

			IF first = 0 THEN		'for the first time after a clear
				first = val(arr)
			ELSE
				second = VAL(arr)	'temp variable = the number on the display
				SELECT CASE operation	'selects action based on which button was pressed
					CASE 15	'division
						first = first/second	'divides the real number by the number on the display
					CASE 16  'multiplication
						first = first * second	'multiplies the real number by the number on the display
					CASE 17  'substraction
						first = first - second	'substracts the number on the display from the real number
					CASE 18  'addition
						first = first + second	'adds the real number to the number on the display
					CASE ELSE
				END SELECT
			END IF
			arr = CSTR(first)		'sets the display TO the answer

			operation = ret   '14 FOR /, 15 FOR *, 16 FOR -, 17 FOR +
			IF operation = 3 THEN first = 0			'equals button
			clearflag = 1

		CASE 2,19	'IF exit OR cancel
			flag = 0

		CASE 20		'plus/minus key
			arr = "-" + arr
		
			IF LEFT(arr$, 2) = "--" THEN 		'double negative
				lengtharr = LEN(arr) - 2		'get length of arr and substract two for the -s
				arr = RIGHT(arr, lengtharr)		'get the rightmost lengtharr characters
			END IF	
		CASE ELSE
	END SELECT
LOOP

