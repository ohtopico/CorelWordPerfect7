REM Generates a text file Using file I/O functions
REM Dos.csc MAY 24, 1995
REM The text file is C:\scripts.txt. You can delete it after this runs.

DIM mess$(5)

CURRFOLDER = "C:\"
mess(1) = "As you can see, you can create a text file with almost anything inside of it"
mess(2) = CHR(32) + "Using the CHR function, you can insert special characters"
mess(3) = CHR(32) + "Like tabs,"+CHR(9)+"and carriage returns."
mess(4) = CHR(32) + "As this file illustrates, this ability can be used to generate"
mess(5) = CHR(32) + "Reports for a given macro. User input can be output in such a way."

OPEN "C:\scripts.txt" FOR OUTPUT AS #2

PRINT #2, "This is test#1", CHR(9), "This is still test#1"

PRINT #2, "You entered ", INPUTBOX("Please enter some text")

FOR i% = 1 TO 20
	PRINT #2, "This is test", i%
NEXT i

FOR j% = 1 TO 5
	PRINT #2, mess(j)
NEXT j
CLOSE 2

