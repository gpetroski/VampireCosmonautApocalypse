; Author: Greg Petroski, Jonathan Swift
; 3/19/09
; Include file for final project

FILE_CREATE_OPEN	= 716Ch
FILE_READ			= 3Fh
FILE_CLOSE		= 3Eh
GRAPHICS_MODE		= 13h	;graphics mode 320x200 256colors
speaker  =  61h	; address of speaker port
timer    =  42h	; address of timer port
delay1   = 25535
delay2   = 0D000h	; delay between notes

OpenFile PROTO, fileName:PTR BYTE;
ReadBMPHeader PROTO, fileHandle:WORD;
LoadBMP PROTO, handle:WORD, BMPHeight:WORD, BMPWidth:WORD, xLoc:WORD, yLoc:WORD
OpenSprite PROTO, fileName:PTR BYTE
ReadIntoShipBuffer PROTO, fileHandle:WORD, pWidth:WORD, pHeight:WORD
DrawShip PROTO, buffer:PTR BYTE, pWidth:WORD, pHeight:WORD, xLoc:WORD, yLOC:WORD
PlaySound PROTO, sound:BYTE
PlayNote PROTO, sound:WORD, noteTime:WORD
CloseFile PROTO, handle:WORD
SetCursorPosition PROTO, xLoc:BYTE, yLoc:BYTE
SetScore PROTO, score:WORD
SetLives PROTO, lives:BYTE


;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////	
;// Description:	STRUCT for in game objects	
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SPRITE STRUCT
	X				WORD ?			;stores x
	Y				WORD ?			;stores y
	Vx				SWORD ?			; Velocity X
	Vy				WORD ?			; Velocity Y
	Visible			BYTE ?			;
	alive			BYTE ?			;yes or no
	nlives			WORD ?			;number of lives	
	points			WORD ?			;points object wort but if you are player stores your points
	BMPHeight       WORD ?  
	BMPWidth        WORD ?
	handle			WORD ? 
SPRITE ENDS