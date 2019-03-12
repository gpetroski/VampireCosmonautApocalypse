TITLE Color String Example              (ColorSt2.asm)

INCLUDE Irvine16.inc
INCLUDE Project16.inc

.data
	tempHold WORD ?			;//this stores the modified X and Y in collision
	eTemp WORD ?			;//stores current ship in Collision
	ALIGN WORD
	player SPRITE <>		;// Space ship sprite
	enemies SPRITE 4 dup(<>)		;// Enemy space ship sprites
	background SPRITE <>	;// Background stars sprite
	bullets SPRITE 8 dup(<>)	;// Bullet sprites
	numBullets BYTE 8
	numEnemies BYTE 1
	QuitVar BYTE 0h		;//check var for quitting
	vmode BYTE ?
	titleScreen BYTE "titleScreen.bmp",0
	mainScreen BYTE "mainScreen.bmp",0
	mainShip   BYTE "ship.bmp",0
	enemyShip BYTE "enemy.bmp",0
	backgroundfile BYTE "stars.bmp",0
	bulletFile BYTE "bullet.bmp",0
	action WORD ?
	currentY WORD 16
	currentX WORD 16
	BMPHeader WORD 54 dup(0)
	HeadBuff        BYTE 54 dup('H')
	palBuff         BYTE 1024 dup('P')
	ScrLine         BYTE 1000 dup(0)
	shipBuffer	 BYTE 1144 dup(0)
	PalSize         WORD ?
	BMPHeight       WORD ?
	BMPWidth        WORD ?
	mainHeight	WORD ?
	mainWidth		WORD ?
	titleHandle WORD ?
	BMPStart BYTE 'BM'
	ASCIICode BYTE ?
	errorMsg1 BYTE "Failed to open file: ",0
	errorMsg2 BYTE "Invalid bmp file: ",0
	mainFileHandle WORD ?
	fileHandle	WORD ?
	count		WORD 0
	songHandle WORD ?
	header BYTE 2 dup(0)
	speakerStatus WORD ?
	songBuffer BYTE 3 dup(0)
	randNum WORD ?
	
.code
main PROC
	mov	ax,@data
	mov	ds,ax

	; Get the current video mode and save it in a variable
	mov  ah,0Fh		
	int  10h
	mov  vmode,al

	;///////////////////////////////////
	;// Get console into graphics mode
	;///////////////////////////////////
	call GraphicsMode
		
	;/////////////////////////
	;// Load Title image
	;/////////////////////////
	invoke OpenSprite, ADDR titleScreen
	jc quit
	mov titleHandle, ax
	invoke LoadBMP, titleHandle, BMPHeight, BMPWidth, 0, 0
	
titleSong:
	;call PlaySong
	;jns titleSong

	;/////////////////////////
	;// Load Score image
	;/////////////////////////
	invoke OpenSprite, ADDR mainScreen
	jc quit
	mov mainFileHandle, ax
	invoke LoadBMP,	mainFileHandle, BMPHeight, BMPWidth, 0, 0

	invoke SetScore, 0
	invoke SetLives, 3

	;///////////////////////////
	;// Load Bullet Image
	;///////////////////////////
	invoke OpenSprite, ADDR bulletFile
	jc quit
	mov bx, 0
	movzx cx, numBullets
LBullets:
	mov bullets[bx].handle, ax
	mov dx, BMPWidth
	mov bullets[bx].BMPWidth, dx
	mov dx, BMPHeight
	mov bullets[bx].BMPHeight, dx
	mov bullets[bx].Visible, 0
	mov bullets[bx].Vx, 3
	add bx, SIZEOF SPRITE
	loop LBullets

	;///////////////////////////
	;// Load Enemy Image
	;///////////////////////////
	invoke OpenSprite, ADDR enemyShip
	jc quit
	mov bx, 0
	movzx cx, numEnemies
LEnemies:
	mov enemies[bx].handle, ax
	mov dx, BMPWidth
	mov enemies[bx].BMPWidth, dx
	mov dx, BMPHeight
	mov enemies[bx].BMPHeight, dx
	mov enemies[bx].Visible, 0
	mov enemies[bx].Vx, -2
	add bx, SIZEOF SPRITE
	loop LEnemies
	
	;///////////////////////////
	;// Load ship image
	;///////////////////////////
	invoke OpenSprite, ADDR mainShip
	jc quit
	;///////////////////////////////////////////////////////////////////////
	;// Save ship related information into player sprite struct
	;///////////////////////////////////////////////////////////////////////
	mov player.handle, ax
	mov ax, BMPWidth
	mov player.BMPWidth, ax
	mov ax, BMPHeight
	mov player.BMPHeight, ax
	mov player.x,20
	mov player.y,100
	
	;///////////////////////////////
	;// Load Stars
	;///////////////////////////////
	invoke OpenSprite, ADDR backgroundfile
	jc quit
	;///////////////////////////////////////////////////////////////////////
	;// Save background related information into background sprite struct
	;///////////////////////////////////////////////////////////////////////
	mov background.handle, ax
	mov ax, BMPWidth
	mov background.BMPWidth, ax
	mov ax, BMPHeight
	mov background.BMPHeight, ax
	mov background.Y, 23
	
	;//////////////////////////////////////////////////
	;// Start of the animation loop
	;//////////////////////////////////////////////////
DrawLoop:
	;//////////////////////////////////////////////////
	;// Redraw stars and ship to screen
	;//////////////////////////////////////////////////
	invoke LoadBMP, background.handle, background.BMPHeight, background.BMPWidth, background.X, background.Y
	invoke LoadBMP, player.handle, player.BMPHeight, player.BMPWidth, player.X, player.Y
	call DrawEnemies
	call DrawBullets

L1:
	;//////////////////////////////////////////////////
	;// Check for keyboard input
	;//////////////////////////////////////////////////
	call INPUT
	jc quit

	;//////////////////////////////////////////////////
	;// set redraw delay 1/12 of a second
	;//////////////////////////////////////////////////
	mov eax, 83
	call Delay
	call SpawnShip
	call collision
	;//////////////////////////////////////////////////
	;// Change the Y of the stars to make it scroll
	;//////////////////////////////////////////////////
	sub background.X, 4
	cmp background.X, 4
	jle resetX
reset:
	jmp DrawLoop
	
resetX:
	;////////////////////////////////////////////////////
	;// Reset the position of the stars to the beginning
	;////////////////////////////////////////////////////
	mov background.X, 320
	jmp reset

quit:
	mov   ah,0			; wait for key
	int   16h

	;invoke CloseFile, background.handle
	;invoke CloseFile, player.handle
	;invoke CloseFile, mainFileHandle
	;invoke CloseFile, titleHandle

	;////////////////////////////////////////////////////
	;// Restore the startup video mode and exit to OS
	;////////////////////////////////////////////////////
	mov   ah,0
	mov   al,vmode
	int   10h

	;////////////////////////////////////////////////////
	;// Exit
	;////////////////////////////////////////////////////
	mov ax,4c00h
	int 21h
main ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		INPUT
;// Description:	
;// Input:		KEYBOARD INPUT pg 495, ASCII  left arrow 4b right arrow 4D space 20h, up arrow 48h, down arrow 50h
;//http://goodfellas.shellcode.com.ar/docz/asm/AoA/Chapter_20/CH20-4.html#HEADING4-1
;//added new ascii from website... my ascii codes were not working...
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
INPUT PROC
	mov ax,0
	mov bx,0
	mov ah, 11h			;//to check keyboard buffer, char waiting
    int 16h				;//Keyboard check proc from Irvine
    jz quit				;//if nothing in buffer goto the end of he function
	;int 21h				;//else... get the value !!!NOT NEEDED!!
    mov ASCIICode,al	;//gets char pressed was al, tried ah
    
	;//ARROW KEYS NOT WORKING can not find correct values
	cmp ASCIICode,77h	;//4800h	;//up arror 48 or 4800
    je up
	
	cmp ASCIICode,0048h	;//this is the real arrow key, up top is W
	je up

	cmp ASCIICode,73h 	;//5000h	;//down arrow 50 or5000
	je  down
	
	cmp ASCIICode,0050h	;//this is the real arrow, up top is S
	je down

	cmp ASCIICode,20h	;//3920h	;//space bar for shooting 20 or 3820
	je bullet
	
	cmp ASCIICode,1Bh	;//1071h	;//Q key 71 or 1071
	je exitp			;//sets quit val

	cmp ASCIICode,0Dh	;//1071h	;//Q key 71 or 1071
	je start			;//sets quit val
	
	jmp clear			;//if all else fails clear and return
up:
	call  MOVEUP
	jmp clear
down:
	call MOVEDOWN
	jmp clear 
bullet: ;//no code yet
	invoke PlaySound, 'S'
	call FireBullet
	jmp clear
start:
	mov ah,10h			;//remove from buffer
	int 16h
	or al, 80h
	jmp quit
exitp:
	mov ah,10h			;//remove from buffer
	int 16h
	stc
	jmp quit
clear:	
	mov ah,10h			;//remove from buffer
    int 16h				;//remove from buffer
    clc
    and al,7Fh
quit:
    ret
    INPUT ENDP
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	collision	
;// Description: checks for collision, called on line 181
;// Input:
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
collision PROC
	mov tempHold,ax
	mov ax,0
	mov si,0
	mov bx,0
	;//mov cx,OFFSET enemies
;//while (esi < 8)

;//ENEMIES WITH BULLETS
enemyloop:
;//if(enemies[esi].visible == 1)
	cmp enemies[bx].Visible,1
	jne ende
	mov si,0
;//while(esi < 8)
bulletloop:
;//if(bullets[esi].visible == 1)
	cmp bullets[si].Visible,1
	jne endb
	mov si,0d
;//compare enemies[etemp].x-10 with bullets[esi].x
	mov ax,enemies[bx].X
	add ax,-20d
	cmp ax,bullets[si].X
	jl endb
;//compare enemies[etemp].x-10 with bullets[esi].x
	mov ax,enemies[bx].X
	add ax,20d
	cmp ax,bullets[si].X
	jg endb
;//compare enemies[etemp].y-10 with bullets[esi].y
	mov ax,enemies[bx].Y
	add ax,-20d
	cmp ax,bullets[si].Y
	jl endb
;//compare enemies[etemp].y+10 with bullets[esi].y
	mov ax,enemies[bx].Y
	add ax,20d
	cmp ax,bullets[si].Y
	jg endb
;//if is it in collision with enemy ship set both invisible
	mov enemies[bx].Visible, 0;
	mov bullets[si].Visible,0;
;//add to score...
endb: add si,SIZEOF SPRITE
	  cmp si,(8 * SIZEOF SPRITE)
	  jl bulletloop
ende: 
	  add bx,SIZEOF SPRITE
	  cmp bx,(8*SIZEOF SPRITE)
	  jl enemyloop

; //player with Enemies ///////////////////////////////////////////////////////
mov bx,0
mov ax,0

PlayerEnemyloop:
;//if enemies[esi].visible == 1)
	cmp enemies[bx].Visible,1
	jne pende
;//compare enemies[etemp].x-10 with player.x
	mov ax,enemies[bx].X
	add ax,-20d
	cmp ax,player.X
	jl pende
;//compare enemies[etemp].x-10 with player.x
	mov ax,enemies[bx].X
	add ax,20d
	cmp ax,player.X
	jg pende
;//compare enemies[etemp].y-10 with player.y
	mov ax,enemies[bx].Y
	add ax,-20d
	cmp ax,player.Y
	jl pende
;//compare enemies[etemp].y+10 with player.y
	mov ax,player.Y
	add ax,20d
	cmp ax,player.Y
	jg pende
;//if is it in collision with enemy ship set both invisible
	mov player.Visible, 0;
pende: 
	  add bx,SIZEOF SPRITE
	  cmp bx,(8 * SIZEOF SPRITE)
	  jl PlayerEnemyloop

;//////////////END//////////////////////////////////////////////
	  mov ax,tempHold
	  ret
collision ENDP
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	FireBullet	
;// Description: Fires a bullet from the ship
;// Input:
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FireBullet PROC
	mov bx, 0
	movzx cx, numBullets
LBullet:
	mov al, bullets[bx].Visible
	cmp al, 0
	je Draw
	add bx, SIZEOF SPRITE
	loop LBullet
	jmp done
Draw:
	mov bullets[bx].Visible, 1
	mov ax, player.X
	add ax, 40
	mov bullets[bx].X, ax
	mov ax, player.Y
	add ax, 10
	mov bullets[bx].Y, ax
done:
	ret
FireBullet ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	SpawnShip	
;// Description: Spawns a ship randomly on the other side of the map
;// Input:
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SpawnShip PROC
	mov bx, 0
	movzx cx, numEnemies
LEnemies:
	mov al, enemies[bx].Visible
	cmp al, 0
	je Draw
	add bx, SIZEOF SPRITE
	loop LEnemies
	jmp done
Draw:
	mov enemies[bx].Visible, 1
	;mov ax, player.X
	;push bx
	;call GetRandomNonColiding
	;pop bx
	mov ax, 149
	call RandomRange
	add ax, 30
	mov enemies[bx].X, 272
	mov enemies[bx].Y, ax
done:
	ret
SpawnShip ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	GetRandomNonColiding	
;// Description: 
;// Input:
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
GetRandomNonColiding PROC
	mov ax, 149
	call RandomRange
	add ax, 30
	mov randNum, ax
	mov bx, 0
	movzx cx, numEnemies
LEnemies:
	mov al, enemies[bx].Visible
	cmp al, 0
	je cont
cont:
	loop LEnemies
	ret
GetRandomNonColiding ENDP


;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	DrawBullets	
;// Description: Draws all visible bullets to the screen
;// Input:
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
DrawEnemies PROC
	mov bx, 0
	movzx cx, numEnemies
LEnemies:
	mov al, enemies[bx].Visible
	cmp al, 1
	jne cont
	mov ax, enemies[bx].X
	add ax, enemies[bx].Vx
	mov enemies[bx].X, ax	
	cmp ax, 0
	jg Draw
	mov enemies[bx].Visible, 0
	jmp cont
Draw:
	push bx
	push cx
	invoke LoadBMP, enemies[bx].handle, enemies[bx].BMPHeight, enemies[bx].BMPWidth, enemies[bx].X, enemies[bx].Y
	pop cx
	pop bx
cont:
	add bx, SIZEOF SPRITE
	loop LEnemies
	ret
DrawEnemies ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	DrawBullets	
;// Description: Draws all visible bullets to the screen
;// Input:
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
DrawBullets PROC
	mov bx, 0
	movzx cx, numBullets
LBullet:
	mov al, bullets[bx].Visible
	cmp al, 1
	jne cont
	mov ax, bullets[bx].X
	add ax, bullets[bx].Vx
	mov bullets[bx].X, ax
	cmp ax, 308
	jl Draw
	mov bullets[bx].Visible, 0
	jmp cont
Draw:
	push bx
	push cx
	invoke LoadBMP, bullets[bx].handle, bullets[bx].BMPHeight, bullets[bx].BMPWidth, bullets[bx].X, bullets[bx].Y
	pop cx
	pop bx
cont:
	add bx, SIZEOF SPRITE
	loop LBullet
	ret
DrawBullets ENDP


;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	SetCursorPosition	
;// Description: sets the position of the cursor
;// Input:	xLoc - x position
;//			yLox - y position
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SetCursorPosition PROC, xLoc:BYTE, yLoc:BYTE
    ;//mov al,ASCIICode     ;Reads from al the CHARACTER
    mov ah,02h
    mov bh,0			 ;video page 0
    mov dh, yLoc
    mov dl, xLoc
    int 10h			    ; draw program
    ret
SetCursorPosition ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	SetScore	
;// Description: Sets the score
;// Input:	score - points to write
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SetScore PROC, score:WORD
	mov ax, score
	mov cx, 38	; Starting position of cursor
L1:
	cwd
	mov bx, 10
	div bx
	push ax
	push dx
	push cx
	invoke SetCursorPosition, cl, 1
	pop cx
	pop ax
	add ax, 48
	push cx
	call DisplayC
	pop cx
	dec cx
	pop ax
	cmp ax, 0
	jnz L1
	ret
SetScore ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	SetLives	
;// Description: Sets the score
;// Input:	lives - number of lives to write
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SetLives PROC, lives:BYTE
	invoke SetCursorPosition, 26, 1
	mov al, lives
	add al, 48
	call DisplayC
	ret
SetLives ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	DisplayC	
;// Description:	Display Character pg 507
;// Input:	
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
DisplayC PROC
    ;//mov al,ASCIICode     ;Reads from al the CHARACTER
    mov ah,0Ah
    mov bh,0			 ;video page 0
    mov bl,yellow
    mov cx,1			 ;repetition count
    int 10h			    ; draw program
    ret
DisplayC ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	MOVEUP	
;// Description: moves the position of the Y from the space up 
;// Input: from  INPUT proc		
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
MOVEUP PROC

	cmp player.Y,30	;//can not go into score board
	jng fin
	sub player.Y,5
fin:	
	ret
MOVEUP ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		MOVEDOWN
;// Description:	moves position of the Y 1 space down
;// Input:		
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
MOVEDOWN PROC
	cmp player.Y,179;//can not move below screen
	jg fin
	add player.Y,5
fin:
	ret
MOVEDOWN ENDP


;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		GraphicsMode
;// Description:	This puts the console into graphics mode of 320x200 with 256 colors
;// Input: none
;// Ouptut: none
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
GraphicsMode PROC
    ; Go into graphics mode 320x200 256colors
    mov     ax, GRAPHICS_MODE
    int     10h
    push    0A000h
    pop     es                      ; ES = A000h (video segment).
    ret
GraphicsMode ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		OpenSprite
;// Description:	Opens a bmp image, determines if it is valid, and puts the file handle for that into ax
;// Input:		fileName - pointer to the file name string
;// Output:		fileHandle - handle of the file opened
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
OpenSprite PROC, fileName:PTR BYTE
	invoke OpenFile, fileName
	jc	OpenFail
	mov fileHandle, ax
	invoke ReadBMPHeader, fileHandle;
	jc	InvalidBMP
	call    ReadPal                 ; Read the BMP's palette and put it in a buffer
	push    es
	call    SendPal                 ; Send the palette to the video registers
	pop es
	mov ax, fileHandle
	jmp quit
OpenFail:
	;////////////////////////////////////////////////////
	;// Display error message for open file failure
	;////////////////////////////////////////////////////
	mov edx, OFFSET errorMsg1
	call WriteString
	mov dx, fileName
	call WriteString
	stc
	jmp quit
InvalidBMP:
	;////////////////////////////////////////////////////
	;// Display error message for invalid BMP
	;////////////////////////////////////////////////////
	mov edx, OFFSET errorMsg2
	call WriteString
	mov dx, fileName
	call WriteString
	stc
quit:
	ret
OpenSprite ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		OpenFile
;// Description:	Opens a file for reading, carry bit is set if the open fails
;// Input:		fileName		name of file on the disk
;// Output:		fileHandle	used to access file to read
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
OpenFile PROC, fileName:PTR BYTE
	; Open the input file
	mov	ax, FILE_CREATE_OPEN	; extended create or open
	mov	bx, OPEN_ACCESS_READONLY	; mode = read-only
	mov	cx, 0				; normal attribute
	mov	dx, 1				; action: open
	mov	si, fileName
	int	21h       		; call MS-DOS
	jc	Failed			; quit if error
	Failed:
	ret
OpenFile ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		CloseFile
;// Description:	Closes a file based on the file handle
;// Input:		handle - file handle
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CloseFile PROC, handle:WORD
	; Open the input file
	mov	ax, FILE_CLOSE
	mov	bx, handle
	int	21h       		; call MS-DOS
	ret
CloseFile ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		ReadBMPHeader
;// Description:	Reads the top 54 bytes of a BMP file to see if it is a valid
;// Input:		fileHandle	used to access file to read
;// Code taken from Kip Irvines site examples on opening a BMP http://kipirvine.com/asm/files/index.html
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ReadBMPHeader PROC, handle:WORD;
	mov ah, FILE_READ
	mov al, 0
	mov bx, handle
	mov cx, 54
	mov dx, offset BMPHeader
	int 21h                     ; Read file header into buffer.
	call CheckValid              ; Is it a valid BMP file?
	jc Error                  ; No? Quit.
	call GetBMPInfo;
	jmp quit
Error:	
	mov al, 'Q'
	call DisplayC
quit:
	ret
ReadBMPHeader ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:		GetBMPInfo
;// Description:	Gets the length and width of BMP file
;// Input:		fileHandle	used to access file to read
;// Code taken from Kip Irvines site examples on opening a BMP http://kipirvine.com/asm/files/index.html
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
GetBMPInfo proc
; This procedure pulls some important BMP info from the header
; and puts it in the appropriate variables.
	mov     ax,BMPHeader[0Ah]          ; AX = Offset of the beginning of the graphic.
	sub     ax,54                   ; Subtract the length of the header
	shr     ax,2                    ; and divide by 4
	mov     PalSize,ax              ; to get the number of colors in the BMP
	; (Each palette entry is 4 bytes long).
	mov     ax,BMPHeader[12h]          ; AX = Horizontal resolution (width) of BMP.
	mov     BMPWidth,ax             ; Store it.
	mov     ax,BMPHeader[16h]          ; AX = Vertical resolution (height) of BMP.
	mov     BMPHeight,ax            ; Store it.
	ret
GetBMPInfo      endp

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	
;// Description: Draws a BMP to the screen with an offset of the given x and y coordinates
;// Input:	handle - file handle of file to be drawn
;//			pHeight - the height of the picture
;//			pWidth - the width of the picture
;//			xLoc - x coordinate to start drawing from
;//			yLoc - y coordinate to start drawing from
;// Code taken from Kip Irvines site examples on opening a BMP http://kipirvine.com/asm/files/index.html
;// Modified to take x,y offsets and to read from a position in a file
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
LoadBMP proc, handle:WORD, pHeight:WORD, pWidth:WORD, xLoc:WORD, yLoc:WORD
; BMP graphics are saved upside-down.  This procedure reads the graphic
; line by line, displaying the lines from bottom to top.  The line at
; which it starts depends on the vertical resolution, so the top-left
; corner of the graphic will always be at the top-left corner of the screen.

; The video memory is a two-dimensional array of memory bytes which
; can be addressed and modified individually.  Each byte represents
; a pixel on the screen, and each byte contains the color of the
; pixel at that location.

	mov     ah,42h      
	mov     bx,handle
	mov cx, 0
	mov dx, 1078				; Size of header in front of file
	mov     al,0
	int     21h


	mov     cx,pHeight            ; We're going to display that many lines
	add		cx, yLoc
	ShowLoop:
	push    cx
	mov     di,cx                   ; Make a copy of CX
	shl     cx,6                    ; Multiply CX by 64
	shl     di,8                    ; Multiply DI by 256
	add     di,cx                   ; DI = CX * 320, and points to the first
	; pixel on the desired screen line.
	add	   di, xLoc

	mov     ah, FILE_READ
	mov	   bx, handle
	mov     cx, pWidth
	mov     dx,offset ScrLine
	int     21h                     ; Read one line into the buffer.

	cld                             ; Clear direction flag, for movsb.
	mov     cx,pWidth
	mov     si,offset ScrLine
	rep     movsb                   ; Copy line in buffer to screen.

	pop     cx
	dec cx
	cmp cx, yLoc
	jle EndLoad
	inc cx
	loop    ShowLoop
EndLoad:	ret
LoadBMP endp

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	ReadPal
;// Description: Reads the color palette of the BMP
;// Input: none
;// Code taken from Kip Irvines site examples on opening a BMP http://kipirvine.com/asm/files/index.html
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ReadPal proc
	mov     ah,3fh
	mov     cx,PalSize              ; CX = Number of colors in palette.
	shl     cx,2                    ; CX = Multiply by 4 to get size (in bytes)
	; of palette.
	mov     dx,offset palBuff
	int     21h                     ; Put the palette into the buffer.
	ret
ReadPal endp

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	SendPal	
;// Description: Sends palette information to the video register
;// Input:	palBuff - buffer containing palette
;//			palSize - size of palette
;// Code taken from Kip Irvines site examples on opening a BMP http://kipirvine.com/asm/files/index.html
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SendPal proc
; This procedure goes through the palette buffer, sending information about
; the palette to the video registers.  One byte is sent out
; port 3C8h, containing the number of the first color in the palette that
; will be sent (0=the first color).  Then, RGB information about the colors
; (any number of colors) is sent out port 3C9h.
	mov     si,offset palBuff       ; Point to buffer containing palette.
	mov     cx,PalSize              ; CX = Number of colors to send.
	mov     dx,3c8h
	mov     al,0                    ; We will start at 0.
	out     dx,al
	inc     dx                      ; DX = 3C9h.
	sndLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB.

	mov     al,[si+2]               ; Get red value.
	shr     al,2                    ; Max. is 255, but video only allows
	; values of up to 63.  Dividing by 4
	; gives a good value.
	out     dx,al                   ; Send it.
	mov     al,[si+1]               ; Get green value.
	shr     al,2
	out     dx,al                   ; Send it.
	mov     al,[si]                 ; Get blue value.
	shr     al,2
	out     dx,al                   ; Send it.

	add     si,4                    ; Point to next color.
	; (There is a null chr. after every color.)
	loop    sndLoop
	ret
SendPal endp



;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	CheckValid
;// Description: Reads the header of a BMP and determines if it is valid
;// Input: BMPHeader - header of the BMP
;// Code taken from Kip Irvines site examples on opening a BMP http://kipirvine.com/asm/files/index.html
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CheckValid proc
	clc
	mov     si,offset BMPHeader
	mov     di,offset BMPStart
	mov     cx,2                    ; BMP ID is 2 bytes long.
	CVloop:
	mov     al,[si]                 ; Get a byte from the header.
	mov     dl,[di]
	cmp     al,dl                   ; Is it what it should be?
	jne     NotValid                ; If not, set the carry flag.
	inc     si
	inc     di
	loop    CVloop

	jmp     CVdone

	NotValid:
	stc

	CVdone:
	ret
CheckValid      endp

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	StartSpeaker
;// Description: Turns on the speaker to be ready for output
;// Input: none
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
StartSpeaker PROC
	in   al,speaker		; get speaker status
	mov speakerStatus, ax	; Save speaker status
	or   al,00000011b   	; set lowest 2 bits
	out  speaker,al     	; turn speaker on
	ret
StartSpeaker ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	PlaySound
;// Description: Plays a shooting sound or explosion sound depending on what character is passed in
;// Input: sound - character representing what sound to play
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
PlaySound PROC, sound:BYTE
	call StartSpeaker
	cmp sound, 'S'
	je shoot
	;cmp sound, 'E'
	;je explosion

	;///////////////////////////////////////////////////////////
	;// Lots of convoluted logic to make an exploding sound
	;///////////////////////////////////////////////////////////
	mov dx, 5
explosion:
	mov al, 70
	mov cx, 2
	mov bx, 0
expLoop:
	out timer,al       	; timer port: pulses speaker
	out timer, al
	push cx

   ; Create a delay loop between pitches:
	mov  cx, 150
L1a:	push cx	; outer loop
	mov  cx, 0A000h
L1b:	; inner loop
	loop L1b
	pop  cx
	loop L1a

	pop cx
	cmp bx, 1
	je L1c
	add al, 3
	loop expLoop
	mov bx, 1
	mov cx, 4
L1c:
	sub al, 7
	loop expLoop
	dec dx
	jnz explosion
	jmp done

	;///////////////////////////////////////////////////////////
	;// Lots of convoluted logic to make an shooting sound
	;///////////////////////////////////////////////////////////
shoot:
	mov al, 9
L2:
	out  timer,al       	; timer port: pulses speaker
	out timer, al

   ; Create a delay loop between pitches:
	mov  cx, 300
L3:	push cx	; outer loop
	mov  cx, 0A000h
L3a:	; inner loop
	loop L3a
	pop  cx
	loop L3

	add al, 1
	cmp al, 12
	jl L2
	jmp done

done:
	call StopSpeaker
	ret
PlaySound ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	StopSpeaker
;// Description: Turns off the speaker
;// Input: none
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
StopSpeaker PROC
	mov ax, speakerStatus
	and  al,11111100b    	; clear lowest 2 bits
	out  speaker,al	; turn speaker off
	ret
StopSpeaker ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	 PlaySong
;// Description: Reads in notes and time from a .gsn (Greg song) file
;// Input: fileHandle - handle of song file
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
PlaySong PROC
	invoke PlayNote, 4063, 4	; D
	invoke PlayNote, 2711, 8	; A
	invoke PlayNote, 2559, 8	; Bb
	invoke PlayNote, 3043, 8	; G
	call INPUT
	js done
	invoke PlayNote, 2711, 8	; A
	invoke PlayNote, 3416, 8	; F
	invoke PlayNote, 3043, 8	; G
	call INPUT
	js done
	invoke PlayNote, 4063, 4	; D
	invoke PlayNote, 2711, 8	; A
	invoke PlayNote, 2559, 8	; Bb
	invoke PlayNote, 3043, 8	; G
	call INPUT
	js done
	invoke PlayNote, 2711, 8	; A
	invoke PlayNote, 3416, 8	; F
	invoke PlayNote, 3043, 8	; G
	call INPUT
	js done
	invoke PlayNote, 4063, 4	; D
	invoke PlayNote, 2031, 8	; D1
	invoke PlayNote, 2280, 8	; C1
	invoke PlayNote, 2559, 8	; Bb
	call INPUT
	js done
	invoke PlayNote, 2280, 8	; C1
	invoke PlayNote, 2559, 8	; Bb
	invoke PlayNote, 2711, 8	; A
	call INPUT
	js done
	invoke PlayNote, 4063, 8	; D
	invoke PlayNote, 3416, 8	; F
	invoke PlayNote, 2559, 8	; Bb
	invoke PlayNote, 2711, 8	; A
	invoke PlayNote, 2559, 8	; Bb
	call INPUT
	js done
	invoke PlayNote, 2711, 8	; A
	invoke PlayNote, 3043, 8	; G
	invoke PlayNote, 3416, 8	; F
done:
	ret
PlaySong ENDP

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;// Procedure:	PlayNote
;// Description: Plays a note based on the code passed in and the time
;// Input:	sound - decimal code representing a note that the speaker can play
;//			noteTime - how long the note plays for
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
PlayNote PROC, sound:WORD, noteTime:WORD
	call StartSpeaker
	mov ax, sound
	jz rest
	out timer, al
	mov al, ah
	out timer, al
rest:
	mov eax, delay1
	cdq
	div noteTime
	mov ebx, eax
L2:
	mov ecx, 65535
L1:
	dec ecx
	jne L1
	dec ebx
	jne L2
	call StopSpeaker
	mov eax, 250
	call Delay
	ret
PlayNote ENDP

END main