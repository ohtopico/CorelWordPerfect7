REM Computer guesses your animals and learns them.
REM Animals.csc July 24, 1995

'#########################################################################

' FUNCTION declarations
DECLARE SUB intro()
DECLARE SUB endmessage()
DECLARE SUB loaddatabase( filename$ )
DECLARE SUB savedatabase( filename$ )
DECLARE FUNCTION quser() AS BOOLEAN
DECLARE SUB psugg()
DECLARE FUNCTION guess( a% ) AS integer
DECLARE SUB Findanimal()

' GLOBAL variables and constants
GLOBAL CONST maxdata=255			' Size of database
GLOBAL CONST YES=6				' Messagebox yes value
GLOBAL CONST YESNOBUTTONS=4		' messagebox YES/NO buttons
GLOBAL CONST Title="Animal Game"	' Game title
GLOBAL anim(maxdata) AS STRING 	' holds animal database

' local main variable
CONST filename = "animals.gme"	' name of the file
DIM again AS BOOLEAN			' program looping condition


'=========================================================================
' Display intro
intro

' Fill up databASe
loaddatabase(filename)

' question loop
again = TRUE
DO
	again = quser()
LOOP UNTIL again = FALSE

IF MESSAGEBOX("Do you want to save this game?",Title,YESNOBUTTONS)=YES THEN
	' Save database
	savedatabase(filename)
end IF

' end message
endmessage


'=========================================================================
' Display introduction message
SUB intro
	DIM temp AS string	' holds temporary value
	temp = "Play GUESS THE ANIMAL with Corel SCRIPT" + CHR(13)
	temp = temp + "Think of an animal and the computer will try to guess it..."
	MESSAGE temp
END SUB

'=========================================================================
' Display end message
SUB endmessage
	DIM temp AS STRING	' holds temporary value
	temp = "Thank you for playing."
	MESSAGE  temp 
END SUB


'=========================================================================
' Try to load database from file.
' if file is currupted or not present, assign default values
SUB loaddatabase( filename )
	ON ERROR GOTO errorhandler
	DIM num AS INTEGER		' number of element in database
	DIM a AS INTEGER		' loop var

	OPEN filename FOR INPUT AS 1
	INPUT #1, anim(1)		' Input number of entries
	num = ASC ( anim(1) )
	FOR a=2 TO num			' Read entire database
		INPUT #1, anim(a)
	NEXT a
	CLOSE #1
	
	subend:
	EXIT SUB
	errorhandler:
		' We could not load database
		' ASsign default values
		CLOSE #1
		anim(1)=CHR(4)
		anim(2)="?"+CHR(3)+CHR(4)+"Does it swim?"
		anim(3)="!gold fish"
		anim(4)="!Hawk"
		resume at subend
END SUB

'=========================================================================
' Try to save database to file.
SUB savedatabase( filename )
	ON ERROR GOTO errorhandler
	DIM num AS INTEGER		' number of element in database
	DIM a AS INTEGER		' loop var

	num = ASC(anim(1))
	OPEN filename FOR OUTPUT AS 1
	FOR a = 1 TO num
		WRITE #1, anim(a)
	NEXT a
	CLOSE #1
	
	subend:
	EXIT SUB
	errorhandler:
		' We could not save database
		CLOSE #1
		MESSAGE "Error saving game!"
		RESUME AT subend
END SUB


'=========================================================================
' Question user on a animal
FUNCTION quser
	DIM ans AS INTEGER			' holds the answer value

	ans = MESSAGEBOX("Are you thinking of an animal?",Title,YESNOBUTTONS)
	IF ans=YES THEN
		' answer is YES
		Findanimal
	ELSE
		' answer is NO
		ans = MESSAGEBOX("Can I give you some suggestions?",Title,YESNOBUTTONS)
		IF ans=YES THEN psugg
	END IF	
	
	' Decide IF we want to continue
	IF ans<>YES THEN 
		quser=FALSE
	ELSE
		quser = TRUE
	END IF
END FUNCTION

'=========================================================================
' Prints suggestion list
SUB psugg
	DIM mess AS STRING	' Message buffer
	DIM a AS INTEGER	' loop counter
	DIM num AS INTEGER  ' number of elements in array
	DIM ani AS INTEGER  ' number of animals found
	
	ani=0
	num = ASC( anim(1) )
	mess = "Here are some suggestions:" + CHR(13) + CHR(13) + CHR(10)
	
	' Prints all animals
	FOR a = 2 TO num
		IF LEFT( anim(a),1)="!" THEN 
			mess = mess + MID( anim(a),2)
			ani=ani+1
	
			' Format string
			IF ani MOD 5 = 0 THEN
				mess = mess + CHR(13)
			ELSE
				mess = mess + CHR(9)
			END IF
		END IF
	NEXT a
	MESSAGE mess
END SUB


'=========================================================================
' Try to guess animal
FUNCTION guess( question% ) 
	IF MESSAGEBOX(MID( anim(question),4),Title,YESNOBUTTONS)=YES THEN
		guess = ASC (MID( anim(question),2,1))
	ELSE
		guess = ASC (MID( anim(question),3,1))
	END IF
END FUNCTION


'=========================================================================
' Find next question and guess
SUB Findanimal
	DIM mess AS STRING		' Message buffer
	DIM g AS INTEGER		' guess
	DIM answer AS INTEGER	' tempo answer
	DIM newval AS INTEGER	' next empty space
	DIM animal AS STRING	' player animal
	DIM question AS STRING	' player question

	' Question :Animal
	BEGIN DIALOG qanimal 144, 70, Title
		TEXT  5, 4, 120, 21, "This animal you were thinking of was a "
		TEXTBOX  5, 15, 125, 14, animal
		OKBUTTON  50, 35, 40, 14
	END DIALOG

	' Start guessing with first question
	g = 2
	
	' find animal
	WHILE LEFT(anim(g),1) = "?"
		g = guess(g)			
	WEND
	
	' verIFy guess
	answer=MESSAGEBOX("The animal you were thinking of was a " + MID(anim(g),2) + "?", Title,YESNOBUTTONS)
	IF answer=YES OR ASC(anim(1))>maxdata-2 THEN
		' We have it
		MESSAGE "Why not think of another animal?"
	ELSE
		' Enter in database
		DIALOG qanimal
		mess = "Please enter a question that would distinguish a " 
		mess = mess + animal + " from a " + MID(anim(g),2)
		BEGIN DIALOG qquestion 144, 82, Title
			TEXT  5, 4, 120, 21, mess
			TEXTBOX  5, 31, 125, 14, question
			OKBUTTON  50, 50, 40, 14
		END DIALOG
		DIALOG qquestion
		newval = ASC(anim(1)) + 1
		answer = MESSAGEBOX("For a " + animal + " the answer would be?",Title,YESNOBUTTONS)
		IF answer=YES THEN
			anim(newval) = "!" + animal
			anim(newval+1) = anim(g)
		ELSE
			anim(newval+1) = "!" + animal
			anim(newval) = anim(g)
		END IF
		anim(g) = "?" + CHR(newval) + CHR(newval+1) + question
		anim(1) = CHR(newval+1)
	END IF
END SUB

