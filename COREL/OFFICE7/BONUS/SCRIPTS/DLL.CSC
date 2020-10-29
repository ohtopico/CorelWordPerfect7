REM Launches the calculator with Windows DLLs
REM Dll.csc July 21, 1995
DECLARE FUNCTION FindWindow LIB "user32" (BYVAL classname AS LONG, BYVAL title AS STRING) AS LONG ALIAS "FindWindowA"
DECLARE FUNCTION WinExec LIB "kernel32" (BYVAL path AS STRING, BYVAL shw AS LONG) AS LONG ALIAS "WinExec"

window$ = "Calculator"
exeName$ = "CALC.EXE"
errmsg$ = "Couldn't run Calculator."

REM Load the application if not loaded already
win = FindWindow(0, window$)
IF win = 0 THEN
	err = WinExec(exeName$, 1)
	IF err < 0 THEN err = err * -1
	IF err < 33 THEN
		MESSAGE(errmsg$)
		STOP
	ENDIF
	win = FindWindow(0, window$)
ENDIF


