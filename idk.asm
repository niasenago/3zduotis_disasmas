; dalinis disasembleris, 4 variantas.
;atpazysta komandas: idiv, div, in, iret, test, int, les, xchg 
;Artiom Hovhannisyan


.model small
.stack 100h
.data
	filehandle  dw                  0
	filename  	db 					255 dup (0)
	outputFilename db 				255 dup (0)
	outFilehandle  dw       	    0

	errMsg 	    db "Nepavyko atidaryti failo. $"
 	msg         db "42 all good!$"
	smth		db "something $"

 	buff        db 255, ?, 			255 dup (?) 
	outBuff     db 255, ?, 			255 dup (?) 
   	hex 		db					3	dup (0)
    code        db                  4   dup (0)
	end_line	db 13, 10, 24h 

	regSP		db "SP $"
	regBP		db "BP $"
	regSI2		db "SI $"
	regDI2		db "DI $"


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
	check		db 0

	index 		dw 0
	instrPtr 	db 					6 dup(0)


.code
proc printInstructionPointer
	push di
	push ax
	push bx

	xor ax,ax
	mov ax, di
	mov bl, 10h
	shr ax,8
	div bl

	 cmp al, 09h
    jle maziau10_4

    add al, 37h
    jmp toliau_4
maziau10_4:
    add al, 30h
	
toliau_4: 
    mov ds:[instrPtr], al 

    cmp ah, 09h
    jle maziauA_4

    add ah, 37h
    jmp toliau2_4
maziauA_4:
   add ah, 30h 
toliau2_4:
	mov ds:[instrPtr + 1], ah 
;----------------------------------
	mov ax, di
	shl ax, 8
	shr ax, 8

	div bl

	cmp al, 09h
    jle maziau10_4_2

    add al, 37h
    jmp toliau_4_2
maziau10_4_2:
    add al, 30h
	
toliau_4_2: 
    mov ds:[instrPtr+2], al 

    cmp ah, 09h
    jle maziauA_4_2

    add ah, 37h
    jmp toliau2_4_2
maziauA_4_2:
   add ah, 30h 
toliau2_4_2:
	mov ds:[instrPtr + 3], ah 

	mov ds:[instrPtr + 4], ' '
	mov ds:[instrPtr + 5], '$'

	           
 	mov dx, offset instrPtr
	mov ah, 09h     
	int 21h	

	pop bx
	pop ax
	pop di
ret
endp

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
proc hexToAsciiSpecial
    push ax
    push bx

	xor ax,ax
	mov bl, 10h
	mov al, [si]

	div bl

    cmp al, 09h
    jle maziau10_3

    add al, 37h
    jmp toliau_3
maziau10_3:
    add al, 30h
	
toliau_3: 
    mov ds:[code], al 

    cmp ah, 09h
    jle maziauA

    add ah, 37h
    jmp toliau2_3
maziauA_3:
   add ah, 30h 
toliau2_3:
	
	mov ds:[code + 1], ah    

    mov ds:[code + 2], ' '
    mov ds:[code + 3], '$'

    mov ah, 09h                
 	mov dx, offset code
	int 21h	

    pop ax
    pop bx

ret
endp

proc hexToAscii
    push ax
    push bx

	xor ax,ax
	mov bl, 10h
	mov al, [si]

	div bl

    cmp al, 09h
    jle maziau10

    add al, 37h
    jmp toliau
maziau10:
    add al, 30h
	
toliau: 
    mov ds:[code], al 

    cmp ah, 09h
    jle maziauA

    add ah, 37h
    jmp toliau2
maziauA:
   add ah, 30h 
toliau2:
	
	mov ds:[code + 1], ah    

    mov ds:[code + 2], ' '
    mov ds:[code + 3], '$'

    mov ah, 09h                
 	mov dx, offset code
	int 21h	
;--------------------------------
	xor ax,ax
	mov al, [si + 1]

	div bl
    
    cmp al, 09h
    jle maziau10_2

    add al, 37h
    jmp toliau_2
maziau10_2:
    add al, 30h
	
toliau_2: 
    mov ds:[code], al 

    cmp ah, 09h
    jle maziauA_2

    add ah, 37h
    jmp toliau2_2
maziauA_2:
   add ah, 30h 
toliau2_2:
	
	mov ds:[code + 1], ah    

    mov ds:[code + 2], ' '
    mov ds:[code + 3], '$'

    mov ah, 09h                
 	mov dx, offset code
	int 21h	

    pop ax
    pop bx
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

proc pSP
 	mov dx, offset regSP 
	mov ah, 09h					 
	int 21h 
ret
endp
proc pBP
	mov dx, offset regBP 
	mov ah, 09h					
	int 21h 
ret
endp
proc pSI
	mov dx, offset regSI2 
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 
ret
endp
proc pDI
 	mov dx, offset regDI2 
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
proc pTest
	mov dx, offset comTest 
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 	


ret
endp


proc printIdiv
 	mov dx, offset comIdiv 
	mov ah, 09h					 ;; print interrupt; end of line is $ chars 
	int 21h 

	call modRegRmX
	call printRm 
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
		
	call modRegRmX
	call printRm 

ret
endp


proc printRm
	cmp rm, 00000011b
	je printBX
	jl printACDX
	jg printSBSD
printACDX:
	cmp rm, 00000001b
	je printCX
	jl printAX
	jg printDX

printSBSD:
	cmp rm, 00000110b
	je printSI
	jl printSB
	jg printDI 
printSB:
	cmp rm, 00000100b
	je printSP
	jg printBP

printAX:
	call pAX
	jmp cont3
printCX:
	call pCX
	jmp cont3
printDX:
	call pDX
	jmp cont3
printBX:
	call pBX
	jmp cont3
;-----------
printSP:
	call pSP
	jmp cont3
printBP:
	call pBP
	jmp cont3
printSI:
	call pSI
	jmp cont3
printDI:
	call pDI
	jmp cont3

cont3:

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
	call printInstructionPointer
										;idiv div and test instructions start from 1111 
	cmp byte ptr [si], 11110111b		;f7 (comand with one word)
	je idivDivTest
	;cmp byte ptr [si], 11110110b		;f6 (comand with one byte)
	;je idivDivTest

	cmp byte ptr [si], 11101101b
	je JUMPERpInAx
	cmp byte ptr [si], 11101100b
	je JUMPERpInAx

	cmp byte ptr [si], 11001111b		;cf is iret instruction
	je JUMPERpIret
	cmp byte ptr [si], 11001101b 		;cd int
	je JUMPERpInt
	cmp byte ptr [si], 11000100b		;c4 beginning of les instruction
	je JUMPERpLes 
	jmp check1
JUMPERpInAx:
	jmp pInAx
JUMPERpIret:
	jmp pIret
JUMP3readFromFile:
	jmp readFromFile	
check1:
	cmp byte ptr [si], 10010000b
	je JUMPERxchgAx
	cmp byte ptr [si], 10010001b
	je JUMPERxchgCx
	cmp byte ptr [si], 10010010b
	je JUMPERxchgDx
	cmp	 byte ptr [si], 10010011b
	je JUMPERxchgBx
	;cmp	byte ptr [si], 10000111b
	;je xchgReg
	cmp	byte ptr [si], 10000110b
	je xchgReg1
	cmp byte ptr [si], 10000100b
	je JUMPER1testReg

	jmp rand

JUMPERpInt:
	jmp pInt

JUMP2readFromFile:
	jmp JUMP3readFromFile


JUMPERpLes:
	jmp pLes
;-------------------------------------------------
	jmp tesiam
idivDivTest:
    call hexToAscii


	push ax
	mov al, byte ptr [si+1]
	and al, 00111000b
	shr al, 3
	
	cmp al, 00000110b
	je printDivInstruction
	cmp al, 00000000b
	je printTestInstruction
	cmp al, 00000111b
	je printIdivInstruction
	jmp rand


JUMPERxchgAx:
	jmp xchgAx
JUMPERxchgCx:
	jmp xchgCx
printDivInstruction:
	call printDiv
	jmp dirbam
printTestInstruction:
	call pTest 
	jmp dirbam
printIdivInstruction:
	call printIdiv
	jmp dirbam

dirbam:
	call pEndl

	add di, 2
	add si, 2
	pop ax
	jmp tesiam

JUMPERxchgDx:
	jmp xchgDx
pInAx:
    call hexToAsciiSpecial
	call printIn
	jmp tesiam
xchgReg1:
	jmp xchgReg

JUMPERxchgBx:
	jmp xchgBx
pIret:
    call hexToAsciiSpecial
	mov ah, 09h                
 	mov dx, offset comIret
	int 21h	
	add di, 1		
	add si, 1

	call pEndl

	jmp tesiam

JUMPER1testReg:
	jmp JUMPERtestReg


pInt:
    call hexToAscii
	call printInt
	jmp tesiam
JUMP1readFromFile:
	jmp JUMP2readFromFile
pLes:
    call hexToAscii
	call printLes
	jmp tesiam

xchgAx:
    call hexToAscii
	call pExchgAx
	jmp xchgTesiam
xchgBx:
    call hexToAscii
	call pExchgBx
	jmp xchgTesiam
xchgCx:
    call hexToAscii
	call pExchgCx
	jmp xchgTesiam
xchgDx:
    call hexToAscii
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
    call hexToAscii

	 
	call pXchg

	call pEndl
	add di, 2
	add si, 2
	jmp tesiam

testReg:
    call hexToAscii
	call printTestReg
	call pEndl
	add di, 2
	add si, 2
	jmp tesiam
JUMPER1readFromFile:
    jmp JUMP1readFromFile
rand:				;if occures unknow instruction 
	inc di
	inc si
    call hexToAscii
	mov ah, 09h                
 	mov dx, offset smth
	int 21h 	
	call pEndl
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
	je JUMPER1readFromFile
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