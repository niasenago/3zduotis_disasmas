.model small
.stack 100h
.data
	filehandle  dw                  0
	filename  	db 					255 dup (0)
	outputFilename db 				255 dup (0)
	outFilehandle  dw       	    0

	errMsg 	    db "Nepavyko atidaryti failo. $"
 	msg         db "35 all good!$"
	smth		db "something $"

 	buff        db 255, ?, 			255 dup (?) 
	outBuff     db 255, ?, 			255 dup (?) 
   	hex 		db					3	dup (0)

	end_line	db 13, 10, 24h 

	regSI		db "[SI] $"
	regDI		db "[DI] $"

	regAX		db "AX $"
	regBX		db "BX $"
	regCX		db "CX $"
	regDX		db "DX $"

	regAH		db "AH $"
	regBH		db "BH $"
	regCH		db "CH $"
	regDH		db "DH $"	

	regAL		db "AL $"
	regBL		db "BL $"
	regCL		db "CL $"
	regDL		db "DL $"	

	comIdiv		db "idiv $"
	comDiv		db "div $"
	comTest		db "test $"
	comIn		db "in $"
	comIret		db "iret $"
	comInt		db "int $"
	comLes		db "les $"
	comXchg		db "xchg $"

	w 			db 0
	mode 		db 0
	reg 		db 0
	rm          db 0

	index 		dw 0

.code


proc strcpy		; bx offset to string
	push di

	push si
	xor di, di
	xor si,si
	mov di, index
	mov al, '$'
	ciklas:
		add bx, si
		cmp byte ptr [bx], '$'
		je baigiam
		mov di, [bx + si]
		inc di
		inc si
	jmp ciklas
baigiam:
	mov index, di
	pop di
	pop si
ret
endp


proc pAx
 	mov dx, offset regAX 
	mov ah, 09h					 
	int 21h 
ret
endp
proc pBx
	mov dx, offset regBX 
	mov ah, 09h					
	int 21h 
ret
endp
proc pCx
	mov dx, offset regCX 
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
ret
endp
proc pDx
 	mov dx, offset regDX 
	mov ah, 09h		
	int 21h	
ret
endp
;-----------------
proc pAh
 	mov dx, offset regAh 
	mov ah, 09h					 
	int 21h 
ret
endp
proc pBh
	mov dx, offset regBh 
	mov ah, 09h					
	int 21h 
ret
endp
proc pCh
	mov dx, offset regCh 
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
ret
endp
proc pDh
 	mov dx, offset regDh 
	mov ah, 09h		
	int 21h	
ret
endp
;-----------------
proc pAl
 	mov dx, offset regAl 
	mov ah, 09h					 
	int 21h 
ret
endp
proc pBl
	mov dx, offset regBl 
	mov ah, 09h					
	int 21h 
ret
endp
proc pCl
	mov dx, offset regCl
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
ret
endp
proc pDl
 	mov dx, offset regDl 
	mov ah, 09h		
	int 21h	
ret
endp
;-------------------------------

proc pEndl
	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h
ret
endp

;----------------------------------------------------------
proc printIdiv
 	mov dx, offset comIdiv 
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 

	cmp byte ptr [si + 1], 11111001b	;cmp with f9
		je printCx
		jl printAx
	cmp byte ptr [si + 1], 11111010b
		je printDx
		jmp printBx

printCx:
	call pCX
	jmp endl
printAx:
	call pAx
	jmp endl
printBx:
 	call pBX
	jmp endl
printDx:
	call pDX
	
endl:
	call pEndl
	ret
endp
proc printIn
 	mov dx, offset comIn 		;In
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 	

	cmp byte ptr [si], 11101100b
		je prAl

	mov dx, offset regAX 		;AX
	mov ah, 09h					 
	int 21h 
	jmp vaziuojam
prAl:
	mov dx, offset regAL		;Al
	mov ah, 09h					 
	int 21h 
vaziuojam:
	mov dx, offset regDX 		;DX
	mov ah, 09h		
	int 21h		

	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h

	add di, 1
	add si, 1

ret
endp
;------------------------------------------------------------
proc printDiv
 	mov dx, offset comDiv 
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
		
	cmp byte ptr [si + 1], 11110001b	;cmp with f1
		je printCx
		jl printAx
	cmp byte ptr [si + 1], 11110010b	;cmp with f2
		je printDx
		jmp printBx
ret
endp
;-----------------------------------------------------------------
proc printInt
 	mov dx, offset comInt 		;In
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
	push cx
	mov ds:[hex + 2], 'h'
	xor ax,ax
	mov bl, 10h
	mov al, [si + 1]
	cmp al, 9
	jle lessThan10

	div bl
	add al, 30h

	mov ds:[hex], Al
lessThan10:
	mov al, [si + 1]
	div bl
	add ah, 30h
	mov ds:[hex + 1], ah

	;print number of interrupt
	mov ah, 40h         ; DOS 2+ - WRITE - WRITE TO FILE OR DEVICE
    mov bx, 1           ; File handle = STDOUT
	mov cx, 3
	mov dx, offset hex
	int 21h

	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h

	mov ds:[hex], 0
	mov ds:[hex + 1], 0
	

	pop cx	
	add di, 2		
	add si, 2
	jmp tesiam

ret
endp
;--------------------------------------------
proc printLes
 	mov dx, offset comLes 		;In
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 

	cmp	byte ptr [si+1], 00000100b
	je prAxSi
	cmp	byte ptr [si+1], 00000101b
	je prAxDi
	cmp	byte ptr [si+1], 00011100b
	je prBxSi
	cmp	byte ptr [si+1], 00011101b
	je prBxDi

	jmp exit
prAxSi:
	call pAx
	mov ah, 09h                
 	mov dx, offset regSI
	int 21h
	jmp exit
prAxDi:
	call pAx
	mov ah, 09h                
 	mov dx, offset regDI
	int 21h
	jmp exit
prBxSi:
	call pBx
	mov ah, 09h                
 	mov dx, offset regSI
	int 21h
	jmp exit
prBxDi:
	call pBx
	mov ah, 09h                
 	mov dx, offset regDI
	int 21h	
	jmp exit
exit:

	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h
	add di, 2		
	add si, 2
ret
endp

;---------------------------------------------
proc pExchgAx
	mov dx, offset comXchg
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
	call pAx
	call pAx
ret
endp
proc pExchgBx
	mov dx, offset comXchg
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
	call pBX
	call pAx
ret
endp
proc pExchgCx
	mov dx, offset comXchg
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
	call pCX
	call pAx
ret
endp
proc pExchgDx
	mov dx, offset comXchg
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
	call pDX
	call pAx

ret
endp

proc openInFile				;open input file
    mov dx, offset filename	;DS:DX -> ASCIZ filename
	mov ah, 3dh				;open file
	mov al, 0				;AL = mode
	int 21h	
	mov filehandle, ax	
	jc openError
	jmp continue
openError:
	mov ah, 9
	mov dx, offset errMsg
	int 21h
	mov ax, 4c00h
	int 21h	
continue:
ret
endp
;-------------------------------------------------
proc pXchg
	mov dx, offset comXchg
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h

	call modRegRmX
	call printRegRm
ret
endp
;-------------------------------------------
proc printRegRm	;reg rm should be set
cmp rm, 00000011b
	je printBl
	jl printACDl
	jg printACDBh
printACDl:
	cmp rm, 00000001b
	je printCL
	jl printAl
	jg printDl

printACDBh:
	cmp rm, 00000110b
	je printDh
	jl printACh
	jg printBh 
printACh:
	cmp rm, 00000100b
	je printAh
	jg printCh

printAl:
	call pAl
	jmp cont
printCl:
	call pCl
	jmp cont
printDl:
	call pDl
	jmp cont
printBl:
	call pBl
	jmp cont
;-----------
printAh:
	call pAh
	jmp cont
printCh:
	call pCh
	jmp cont
printDh:
	call pDh
	jmp cont
printBh:
	call pBh
	jmp cont

cont:

	cmp reg, 00000011b
	je printBl2
	jl printACDl2
	jg printACDBh2
printACDl2:
	cmp reg, 00000001b
	je printCL2
	je printAl2
	jg printDl2

printACDBh2:
	cmp reg, 00000110b
	je printDh2
	jl printACh2
	jg printBh2 
printACh2:
	cmp reg, 00000100b
	je printAh2
	jg printCh2

printAl2:;
	call pAl
	jmp cont2
printCl2:
	call pCl
	jmp cont2
printDl2:
	call pDl
	jmp cont2
printBl2:
	call pBl
	jmp cont2
;-----------
printAh2:
	call pAh
	jmp cont2
printCh2:
	call pCh
	jmp cont2
printDh2:
	call pDh
	jmp cont2
printBh2:
	call pBh
	jmp cont2
cont2:

ret
endp
proc modRegRmX
	push ax
	mov al, byte ptr [si]
	and al, 00000001b
	mov ds:[w], al		

	mov al, byte ptr [si+1]
	and al, 11000000b
	shr al, 6
	mov ds:[mode], al	

	mov al, byte ptr [si+1]
	and al, 00111000b
	shr al, 3
	mov ds:[reg] , al		

	mov al, byte ptr [si+1]
	and al, 00000111b
	mov ds:[rm], al		
	pop ax
ret
endp

proc printTestReg
	mov ah, 09h                
 	mov dx, offset comTest
	int 21h	
	call modRegRmX
	call printRegRm
	
ret
endp

;--------------------------------------------------
proc inlineArg
    xor cx,cx
    mov cl,es:[80h] ;amount of inline inputed chars

    mov si,0082h ;0082h beginning of argv
    xor bx,bx
    xor di,di
copy1:

    mov al,es:[si + bx]             ;bx is counter for the buffer and input name
    cmp al, 32

    je secondName

    mov ds:[filename + bx], al ;copy filename from es to variable
    inc bx
    loop copy1
    jmp continue

secondName:
    cmp al, 32
    je skip
    mov ds:[outputFilename + di], al ;copy outputFilname

    inc di                          ;di counter for output name
skip:
    inc bx                          ;bx counter for buffer
    mov al,es:[si + bx]
    loop secondName 

ret
endp
proc createOutFile
	mov ax, 3c00h
	xor cx,cx
	lea dx, outputFilename
	int 21h
	mov outFilehandle, ax
ret
endp


;-------------------------------------------------------------------------------------------------------------------------------------------
start:
    mov ax, @DATA
	mov ds, ax

	call inlineArg
	call openInFile
	call createOutFile
	mov index, offset outBuff
;read data from file  
readFromFile:
    mov ah, 3fh		        	;read from file
	mov bx, filehandle		     ;BX = file handle
	mov cx, 255			         ;CX = number of bytes to read
	mov dx, offset buff	
	int 21h

    push ax						;push number of succesifully entered values
    ;jc openError

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;close file
	mov ah, 3eh 			    ;close file
	mov bx, offset filehandle	;BX = file handle
	int 21h




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	mov si, offset buff
	pop ax
	mov cx, ax
	xor di,di
	jmp l1


l1:
										;idiv div and test instructions start from 1111 
	cmp byte ptr [si], 11110111b		;f7 (comand with one word)
	je idivDivTest
	;cmp byte ptr [si], 11110110b		;f6 (comand with one byte)
	;je idivDivTest

	cmp byte ptr [si], 11101101b
	je pInAx
	cmp byte ptr [si], 11101100b
	je pInAx

	cmp byte ptr [si], 11001111b		;cf is iret instruction
	je pIret
	cmp byte ptr [si], 11001101b 		;cd int
	je pInt
	cmp byte ptr [si], 11000100b		;c4 beginning of les instruction
	je pLes 
	jmp check
JUMP3readFromFile:
	jmp readFromFile	
check:
	cmp byte ptr [si], 10010000b
	je xchgAx
	cmp byte ptr [si], 10010001b
	je xchgCx
	cmp byte ptr [si], 10010010b
	je xchgDx
	cmp	 byte ptr [si], 10010011b
	je xchgBx
	;cmp	byte ptr [si], 10000111b
	;je xchgReg
	cmp	byte ptr [si], 10000110b
	je xchgReg1
	cmp byte ptr [si], 10000100b
	je JUMPERtestReg

	jmp rand
JUMP2readFromFile:
	jmp JUMP3readFromFile
;-------------------------------------------------
	jmp tesiam
idivDivTest:
	cmp byte ptr [si + 1], 11111000b	;f8
	jge pIdiv
	cmp  byte ptr [si + 1], 11110000b
	jge pDiv							;in other case jump to test command!!!
		pIdiv:
			call printIdiv
			add di, 2
			add si, 2
			jmp tesiam
		pDiv:
			call printDiv
			add di, 2
			add si, 2
	jmp tesiam
pInAx:
	call printIn
	jmp tesiam
xchgReg1:
	jmp xchgReg
pIret:
	mov ah, 09h                
 	mov dx, offset comIret
	int 21h	
	add di, 1		
	add si, 1

	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h

	jmp tesiam


pInt:
	call printInt
	jmp tesiam
JUMP1readFromFile:
	jmp JUMP2readFromFile
pLes:
	call printLes
	jmp tesiam

xchgAx:
	call pExchgAx
	jmp xchgTesiam
xchgBx:
	call pExchgBx
	jmp xchgTesiam
xchgCx:
	call pExchgCx
	jmp xchgTesiam
xchgDx:
	call pExchgDx
	jmp xchgTesiam
xchgTesiam:
	call pEndl

	add di, 1		
	add si, 1

	jmp tesiam
JUMPERtestReg:
	jmp testReg
xchgReg:

	 
	call pXchg

	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h
	add di, 2
	add si, 2
	jmp tesiam

testReg:
	call printTestReg
	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h
	add di, 2
	add si, 2
	jmp tesiam

rand:				;if occures unknow instruction 
	inc di
	inc si

	mov ah, 09h                
 	mov dx, offset smth
	int 21h 	
	mov ah, 09h                
 	mov dx, offset end_line 
	int 21h

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tesiam:
 
	cmp di, cx		;kai visaks apdorota iseiti is ciklo
	je closeProgram
	jmp l1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; print out buff to file
	mov ax, 4000h
	mov bx, offset outFilehandle
	mov dx, offset outBuff
	;cx jau ir taip yra baitu kiekis
	int 21h

	cmp cx, 0		;if buffer overflow
	je JUMP1readFromFile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
closeProgram:

	mov ah, 09h                
 	mov dx, offset msg 
	int 21h 
;;;;;;;;;;;;;;;;;;;close output file
	mov ah, 3eh 			    	;close file
	mov bx, offset outFilehandle;BX = file handle
	int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	mov ah, 4ch               
 	int 21h  
end start