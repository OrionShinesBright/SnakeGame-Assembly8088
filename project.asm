[org 0x100]
jmp start


;-----------------------------------------------------------------------------;
;***						;**********************;                       ***;
;***						;*** DATA VARIABLES ***;                       ***;
;***						;**********************;                       ***;
;-----------------------------------------------------------------------------;

currRow: 	dw 24
currCol: 	dw 78

currDir: 	dw 1				; u=0, r=1, d=2, l=3

oldkbisr: 	dd 0
oldtimer: 	dd 0

secondTick: db 0

winMsg:	 	db "You Won!"  		; len = 8
loseMsg: 	db "You Lost!" 		; len = 9
forfeitMsg: db "Forfeiting..." 	; len = 13

startMsg1: 	db "COAL PROJECT" 										; len = 12
startMsg2: 	db "Muhammad Rafay [24L-0649] (BCS-3D)" 				; len = 34
startMsg3: 	db "Press any key to continue" 							; len = 25
startMsg4: 	db "CONTROLS: Arrow Keys to move, Escape to forfeit" 	; len = 47

oldMainT: dd 0

melodyArr: 	
	; phrase 1
	dw 	659, 659, 698, 784,
	dw	784, 698, 659, 587,
	dw	523, 523, 587, 659,
	dw 	659, 659, 587, 587
	; phrase 2
	dw 	659, 659, 698, 784,
	dw	784, 698, 659, 587,
	dw	523, 523, 587, 659,
	dw 	587, 587, 523, 523
	; phrase 3
	dw 	587, 587, 659, 523,
	dw 	587, 698, 659, 523,
	dw 	587, 698, 659, 587,
	dw 	523, 587, 523, 523
	; phrase 4
	dw 	659, 659, 698, 784,
	dw	784, 698, 659, 587,
	dw	523, 523, 587, 659,
	dw 	587, 587, 523, 523

melodyArrSize: db 64
melodyArrCurr: db 0


;-----------------------------------------------------------------------------;
;***						;**********************;                       ***;
;***						;*** HELPER SUBRTNS ***;                       ***;
;***						;**********************;                       ***;
;-----------------------------------------------------------------------------;

;;;;;;;;;;;;;;;;
; CLEAR SCREEN ;
;;;;;;;;;;;;;;;;

; Parameters:
; 1. color 	(word) [bp+4] -> ax
clrscr:
	push bp
	mov bp, sp
	pusha
	
	mov ax, 0xb800
	mov es, ax

	mov di, 0
	mov ax, [bp+4]
	mov cx, 2000

	cld
	rep stosw

	popa
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;
; DRAW RECTANGLE ;
;;;;;;;;;;;;;;;;;;

; Parameters:
; 1. topLeft 	(word) [bp+10]	-> di
; 2. height		(word) [bp+8]	-> dx
; 3. width		(word) [bp+6]	-> cx
; 4. shader		(word) [bp+4]	-> ax
drawRectangle:
	push bp
	mov bp, sp
	pusha
	
	mov ax, 0xb800
	mov es, ax

	mov di, [bp+10]
	mov dx, [bp+8]
	mov cx, [bp+6]
	mov ax, [bp+4]

	drawRectangleLoop:
		push di
		push cx

		cld
		rep stosw
		
		pop cx
		pop di
		add di, 160
		
		dec dx
		cmp dx, 0
		jnz drawRectangleLoop

	popa
	pop bp
	ret 8

;;;;;;;;;;;;;;;;;;;;;;;;;;
; BORDERS AROUND SCREENS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

makeBorders:
	push bp
	pusha

	videoMemInit:
		mov ax, 0xb800
		mov es, ax
	
	mov di, 160
	mov cx, 23
	drawLeftCols:
		mov word [es:di], 0x08b3
		mov word [es:di + 2], 0x0cba
		add di, 160
		loop drawLeftCols

	mov di, 318
	mov cx, 23
	drawRightCols:
		mov word [es:di], 0x08b3
		mov word [es:di - 2], 0x0cba
		add di, 160
		loop drawRightCols

	mov di, 2
	mov cx, 78
	drawTopRows:
		mov word [es:di], 0x08c4
		mov word [es:di + 160], 0x0ccd
		add di, 2
		loop drawTopRows

	mov di, 3842
	mov cx, 78
	drawBotRows:
		mov word [es:di], 0x08c4
		mov word [es:di - 160], 0x0ccd
		add di, 2
		loop drawBotRows
	
	drawCorners:
		; outer set
		mov word [es:0], 0x08da
		mov word [es:158], 0x08bf
		mov word [es:3840], 0x08c0
		mov word [es:3998], 0x08d9
		; inner set
		mov word [es:162], 0x0cc9
		mov word [es:316], 0x0cbb
		mov word [es:3682], 0x0cc8
		mov word [es:3836], 0x0cbc

	popa
	pop bp
	ret


;----------------------------------------------------------------------------------;
	  ;****************************************************************;
	  ;***					;*** SOUND SYSTEMS ***;					***;
	  ;****************************************************************;
;----------------------------------------------------------------------------------;

;;;;;;;;;;;;;;;;;;;;;;;
; START PLAYING SOUND ;
;;;;;;;;;;;;;;;;;;;;;;;

playSound:
    cmp ax, 0
    je noSound

    pusha

    mov cx, ax        ; CX = frequency

    ; PIT control:
    	; channel 2
    	; lobyte/hibyte
    	; mode 3 (square wave)
    mov al, 10110110b
    out 0x43, al

    mov dx, 0x0012
    mov ax, 0x34DC
    div cx

	; send out mode
    out 0x42, al
    mov al, ah
    out 0x42, al

    ; enable speaker
    in  al, 0x61
    or  al, 00000011b
    out 0x61, al

	call delayLoop

    popa
    ret

;;;;;;;;;;;;;;;;;;;;;
; IF NO SOUND PLAYS ;
;;;;;;;;;;;;;;;;;;;;;

noSound:
    ret

;;;;;;;;;;;;;;;;;;;;;;
; STOP PLAYING SOUND ;
;;;;;;;;;;;;;;;;;;;;;;

stopSound:
    pusha
    in  al, 0x61
    and al, 11111100b
    out 0x61, al
    popa
    ret

;;;;;;;;;;;;;;;;;;;;
; RHYTHM FOR SOUND ;
;;;;;;;;;;;;;;;;;;;;

delayLoop:
    pusha

	mov cx, 0xffff
	mov bx, cx
	dloop1:
		dec bx
    	loop dloop1

    popa
    ret

;;;;;;;;;;;;;;;;;
; PLAY THE NOTE ;
;;;;;;;;;;;;;;;;;

notePlay:
    call playSound
    call delayLoop
    call stopSound
    call delayLoop
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In a tree by the brook, there's a songbird who sings ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

melody:
	pusha

	mov al, [cs:melodyArrSize]
	cmp [cs:melodyArrCurr], al
	jne beginMelody

	wrapMelodyToStart:
		mov [cs:melodyArrCurr], 0

	beginMelody:
		mov si, melodyArr
		mov bl, [cs:melodyArrCurr]
		xor bh, bh
		shl bx, 1
		add si, bx
	
		mov ax, [si]
    	call notePlay

		inc byte [cs:melodyArrCurr]

	popa
    ret

;;;;;;;;;;;;;;;;;;;;;
; MELODY ON SUCCESS ;
;;;;;;;;;;;;;;;;;;;;;

winMelody:
	push ax

	mov ax, 523
	call notePlay
	mov ax, 659
	call notePlay
	mov ax, 784
	call notePlay

	pop ax
	ret

;;;;;;;;;;;;;;;;;;;;
; MELODY ON DEFEAT ;
;;;;;;;;;;;;;;;;;;;;

loseMelody:
	push ax

	mov ax, 784
	call notePlay
	mov ax, 659
	call notePlay
	mov ax, 523
	call notePlay

	pop ax
	ret

;-----------------------------------------------------------------------------;
;***						;**********************;                       ***;
;***						;*** ENTITY PLACING ***;                       ***;
;***						;**********************;                       ***;
;-----------------------------------------------------------------------------;

;;;;;;;;;;;;;;;;;;;
; PLACE OBSTACLES ;
;;;;;;;;;;;;;;;;;;;

; The obstacles are static, so no parameters
placeObstacles:
	pusha

	mov ax, 0x2220 	; green color
	mov cx, 7		; num of obstacles

	placeObstacle_farRight:
		push word 156
		push word 25
		push word 2
		push ax
	placeObstacle_rightHorizontal:
		push word 1238
		push word 1
		push word 12
		push ax
	placeObstacle_rightVertical:
		push word 900
		push word 7
		push word 2
		push ax
	placeObstacle_midVertical:
		push word 1682
		push word 7
		push word 2
		push ax
	placeObstacle_leftHorizontalTop:
		push word 1478
		push word 1
		push word 12
		push ax
	placeObstacle_leftHorizontalBot:
		push word 2604
		push word 1
		push word 12
		push ax
	placeObstacle_leftVertical:
		push word 1138
		push word 7
		push word 2
		push ax

	placeObstacles_loop:
		call drawRectangle
		loop placeObstacles_loop

	popa
	ret

;;;;;;;;;;;;;;
; PLACE GOAL ;
;;;;;;;;;;;;;;

; The goal is static, so no parameters
placeGoal:
	pusha

	mov ax, 0xb800
	mov es, ax

	mov ax, 0x4420
	; Doing two cells for goal
	mov [es:0], ax
	mov [es:2], ax
	
	popa
	ret

;;;;;;;;;;;;;;;;
; PLACE PLAYER ;
;;;;;;;;;;;;;;;;

; Parameters:
; 1. row	(word) [bp+8] -> si
; 2. col	(word) [bp+6] -> bx
; 3. color	(word) [bp+4]
placePlayer:
    push bp
    mov bp, sp
    push es
    push ax

    mov ax, 0xb800
    mov es, ax

    mov bx, [bp+6]
    mov si, [bp+8]

    ; byte offset
    mov ax, 160
    mul si
    mov bx, ax
    add bx, [bp+6]
    
    mov ax, [bp+4]
    mov word [es:bx], ax

    pop ax
    pop es
    pop bp
    ret 6


;-----------------------------------------------------------------------------;
;***					;**************************;                       ***;
;***					;*** MECHANICS HANDLERS ***;                       ***;
;***					;**************************;                       ***;
;-----------------------------------------------------------------------------;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DIRECTION CHANGE HANDLERS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

moveUp:
	mov word [cs:currDir], 0
	jmp endKbISR

moveRight:
	mov word [cs:currDir], 1
	jmp endKbISR

moveDown:
	mov word [cs:currDir], 2
	jmp endKbISR

moveLeft:
	mov word [cs:currDir], 3
	jmp endKbISR

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MOVEMENT IMPLEMENTATION ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

updatePosition:
	; Move up
	cmp1:
		cmp word [cs:currDir], 0
		jne cmp2
		sub word [cs:currRow], 1
		jmp doneUpdatePos
	; Move right
	cmp2:
		cmp word [cs:currDir], 1
		jne cmp3
		add word [cs:currCol], 4
		jmp doneUpdatePos
	; Move down
	cmp3:
		cmp word [cs:currDir], 2
		jne cmp4
		add word [cs:currRow], 1
		jmp doneUpdatePos
	; Move left
	cmp4:
		sub word [cs:currCol], 4
	
	doneUpdatePos:
		call checkConditions
		ret

;;;;;;;;;;;;;;;;;;;;;
; STATUS CALCULATOR ;
;;;;;;;;;;;;;;;;;;;;;

checkConditions:
	push ax
	push es
	push si
	push bx

	; for on-screen locations
	checkPhysicalPosition:
		; rows
		cmp word [cs:currRow], 24
		ja isDead
		cmp word [cs:currRow], 0
		jb isDead
		; cols
		cmp word [cs:currCol], 158
		ja isDead
		cmp word [cs:currCol], 0
		jb isDead

	; for in-game status
	checkGamePosition:
    	; calculating current position
    	mov ax, 0xb800
    	mov es, ax

    	mov bx, [cs:currCol]
    	mov si, [cs:currRow]

    	mov ax, 160
    	mul si
    	mov bx, ax
    	add bx, [cs:currCol]
		
		; greens (obstacles) = 0x2220
		cmp word [es:bx], 0x2220
		je isDead
		; reds (goals) = 0x4420
		cmp word [es:bx], 0x4420
		je isWinner
	
	pop bx
	pop si
	pop es
	pop ax
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;
; STATUS CHANGE HANDLERS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

isDead:
	; death screen
	mov bl, 0x0c
	mov cx, 9
	mov bp, loseMsg
	call loseMelody
	jmp exitOut

isWinner:
	; winning screen
	mov bl, 0x0a
	mov cx, 8
	mov bp, winMsg
	call winMelody
	jmp exitOut	

isForfeited:
	; forfeiting screen
	mov bl, 0x0b
	mov cx, 13
	mov bp, forfeitMsg
	call loseMelody
	jmp exitOut	


;----------------------------------------------------------------------------;
;***	                        ;************;                            ***;
;***	                        ;*** ISRs ***;                            ***;
;***	                        ;************;                            ***;
;----------------------------------------------------------------------------;

;;;;;;;;;;;;;;
; SETTING UP ;
;;;;;;;;;;;;;;

setupISRs:
	pusha
	push es

	xor ax, ax
	mov es, ax
	
	saveOldISRs:	
			; 1. kb
		mov ax, [es:9*4]
		mov [oldkbisr], ax
		mov ax, [es:9*4+2]
		mov [oldkbisr+2], ax
			; 2. timer
		mov ax, [es:8*4]
		mov [oldtimer], ax
		mov ax, [es:8*4+2]
		mov [oldtimer+2], ax

	hookingISRs:
		cli
			; 1. kb
			mov word [es:9*4], kbisr
			mov [es:9*4+2], cs
			; 2. timer
			mov word [es:8*4], timerisr
			mov [es:8*4+2], cs
		sti

	pop es
	popa

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UNHOOKING AND RESTORING ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

restoreISRs:
	pusha
	push es

	xor ax, ax
	mov es, ax

		; 1. kb
	mov ax, [cs:oldkbisr]
	mov [es:9*4], ax
	mov ax, [cs:oldkbisr+2]
	mov [es:9*4+2], ax
		; 2. timer
	mov ax, [cs:oldtimer]
	mov [es:8*4], ax
	mov ax, [cs:oldtimer+2]
	mov [es:8*4+2], ax

	pop es
	popa
	ret

;;;;;;;;;;;;
; KEYBOARD ;
;;;;;;;;;;;;

; hooks into IRQ 1 through INT 9
; does not attempt to chain for other keys
kbisr:
	push ax

	readKey:
		in al, 0x60
	
	makeComparisons:
		cmp al, 0x48	; up = 0
		je moveUp
		cmp al, 0x4d	; right = 1
		je moveRight
		cmp al, 0x50	; down = 2
		je moveDown
		cmp al, 0x4b	; left = 3
		je moveLeft
		
		cmp al, 0x01	; escape = forfeit
		je isForfeited

	endKbISR:
		mov al, 0x20
		out 0x20, al

	pop ax
	iret

;;;;;;;;;
; TIMER ;
;;;;;;;;;

; Works for every second tick, through a flag
; Makes all game mehanics work
; Hooks to IRQ 0 through INT 8
timerisr:
	push ax

	cmp byte [cs:secondTick], 0
	jne incrementTick

	push word [cs:currRow]
	push word [cs:currCol]
	push word 0x7720
	call placePlayer

	call updatePosition

	push word [cs:currRow]
	push word [cs:currCol]
	push word 0x792a
	call placePlayer
	
	call melody

	dec byte [cs:secondTick]
	jmp eoi_timerisr

	incrementTick:
		inc byte [cs:secondTick]

	eoi_timerisr:
		mov al, 0x20
		out 0x20, al

	pop ax
	iret


;-------------------------------------------------------------------------;
;***                    ;***********************;                      ***;
;***                    ;*** SCREEN DRAWINGS ***;                      ***;
;***                    ;***********************;                      ***;
;-------------------------------------------------------------------------;

;;;;;;;;;;;;;;;;;;;;;;;;;
; SCREEN INITIALIZATION ;
;;;;;;;;;;;;;;;;;;;;;;;;;

initializeScreen:
	pusha
	
	push word 0x7720
	call clrscr
	call placeObstacles
	call placeGoal

	push word [currRow]
	push word [currCol]
	push word 0x792a
	call placePlayer
	
	popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STARTING SCREEN HANDLER ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

mainScreen:
	pusha
	push es

	push word 0x0020
	call clrscr

	call makeBorders

	print_startMsg1:
		mov bl, 0x0b
		mov cx, 12
		mov bp, startMsg1
		mov ah, 0x13
		mov al, 1
		mov bh, 0
		mov dx, 0x0a22
		push cs
		pop es
		int 0x10

	print_startMsg2:
		mov bl, 0x03
		mov cx, 34
		mov bp, startMsg2
		mov ah, 0x13
		mov al, 1
		mov bh, 0
		mov dx, 0x0b17
		push cs
		pop es
		int 0x10
	
	print_startMsg3:
		mov bl, 0x0a
		mov cx, 25
		mov bp, startMsg3
		mov ah, 0x13
		mov al, 1
		mov bh, 0
		mov dx, 0x0d1b
		push cs
		pop es
		int 0x10
	
	print_startMsg4:
		mov bl, 0x02
		mov cx, 47
		mov bp, startMsg4
		mov ah, 0x13
		mov al, 1
		mov bh, 0
		mov dx, 0x0e11
		push cs
		pop es
		int 0x10
	
	hookTimerForLoadingAnimation:
		xor ax, ax
		mov es, ax
			mov ax, [es:8*4]
			mov [oldMainT], ax
			mov ax, [es:8*4+2]
			mov [oldMainT+2], ax
		cli
			mov word [es:8*4], timerHookForMain
			mov [es:8*4+2], cs
		sti

	jmp main_randomDecor

	timerHookForMain:
		pusha
		push es

		mov ax, 0xb800
		mov es, ax

		mov [es:892], 0x77db
		check1:
			cmp byte [es:2478+160], '|'
			jne check2
			mov byte [es:2478+160], '/'
			jmp checkEnd
		check2:
			cmp byte [es:2478+160], '/'
			jne check3
			mov byte [es:2478+160], '-'
			jmp checkEnd
		check3:
			cmp byte [es:2478+160], '-'
			jne check4
			mov byte [es:2478+160], '\'
			jmp checkEnd
		check4:
			mov byte [es:2478+160], '|'
		
		checkEnd:
			mov al, 0x20
			out 0x20, al

			pop es
			popa
			iret


	main_randomDecor:
		mov ax, 0xb800
		mov es, ax

		mov byte [es:2474+160], '-'
		mov byte [es:2475+160], 0x0b
		mov byte [es:2476+160], '['
		mov byte [es:2477+160], 0x0b
		mov byte [es:2478+160], '|'
		mov byte [es:2479+160], 0x0e
		mov byte [es:2480+160], ']'
		mov byte [es:2481+160], 0x0b
		mov byte [es:2482+160], '-'
		mov byte [es:2483+160], 0x0b

		mov word [es:558], 0x0bc1

		mov word [es:712], 0x0bda
		mov word [es:714], 0x0bb4
		mov word [es:716], 0x0bb2
		mov word [es:718], 0x0bb0
		mov word [es:720], 0x0bb2
		mov word [es:722], 0x0bc3
		mov word [es:724], 0x0bbf

		mov word [es:3272], 0x0bc0
		mov word [es:3274], 0x0bb4
		mov word [es:3276], 0x0bb2
		mov word [es:3278], 0x0bb0
		mov word [es:3280], 0x0bb2
		mov word [es:3282], 0x0bc3
		mov word [es:3284], 0x0bd9
	
		mov word [es:3438], 0x0bc2

	getKey_mainScreen:
		mov ah, 0
		int 0x16
	
	unhookTimerForMain:
		xor ax, ax
		mov es, ax
		cli
			mov ax, [oldMainT]
			mov [es:8*4], ax
			mov ax, [oldMainT+2]
			mov [es:8*4+2], ax
		sti

	pop es
	popa
	ret

;;;;;;;;;;;;;;;;;;
; EXITING SCREEN ;
;;;;;;;;;;;;;;;;;;

drawFinalScreen:
	pusha
	push es

	push word 0x0720
	call clrscr
	call makeBorders

	printExitMsg:
		mov ah, 0x13
		mov al, 1
		mov bh, 0
		mov dx, 0x0203
		push cs
		pop es
		int 0x10
	
	printHoldingMsg:
		mov bl, 0x07
		mov cx, 25
		mov bp, startMsg3
		mov ah, 0x13
		mov al, 1
		mov bh, 0
		mov dx, 0x0303
		push cs
		pop es
		int 0x10

	pop es
	popa
	ret

;---------------------------------------------------------------------;
;***                    ;*******************;                      ***;
;***                    ;*** DRIVER CODE ***;                      ***;
;***                    ;*******************;                      ***;
;---------------------------------------------------------------------;

start:
	call mainScreen
	call initializeScreen
	call setupISRs

gameLoop:
	jmp gameLoop


;--------------------------------------------------------------------;
;***                    ;******************;                      ***;
;***                    ;*** CLEAN EXIT ***;                      ***;
;***                    ;******************;                      ***;
;--------------------------------------------------------------------;

exitOut:
	call restoreISRs
	call drawFinalScreen

	mov al, 0x20
	out 0x20, al

	waitForFinalKey:
		mov ax, 0
		int 0x16
	
	push word 0x0720
	call clrscr

	mov ax, 0x4c00
	int 0x21
