.model small
.stack 100h
.data
	filehandle      dw              0
	filename  	    db 	        	255 dup (0)
	outputFilename  db 				255 dup (0)
	outFilehandle   dw       	    0

    errMsg          db "Nepavyko atidaryti failo $"
 	msg             db "3 all good!$"
    create_of_error_msg db "Nepavyko sukurti output filo. $"

    hello           db "Labas, $"
    world           db "pasauli! $"
    outBuff         db              255 dup(0)
	index 		    dw              0
.CODE

proc strcpy		; bx offset to string
	push di
	push si

	xor di, di
	xor si,si

	mov di, index
	ciklas:
		cmp byte ptr [bx], 24h
		je baigiam
		mov di, [bx]
		inc di
		inc bx

	jmp ciklas
baigiam:
	mov index, di
	
	pop di
	pop si
ret
endp

proc inlineArg    
    xor ch, ch
    mov cl, es:[80h]

    dec cl
    mov si, 82h

    ; ieskomas input failo pavadinimas
read_ifn:
    mov di, offset filename

    read_ifn_loop:
        mov dl, es:[si]

        cmp dl, " "
        je end_read_ifn

        mov ds:[di], dl

        inc si
        inc di
    loop read_ifn_loop

end_read_ifn:
    cmp cl, 0
    je skip_read_ofn
    inc si

    ; ieskomas output failo pavadinimas
read_ofn:
    mov di, offset outputFilename
    read_ofn_loop:
        mov dl, es:[si]

        cmp dl, 0
        je skip_read_ofn

        cmp dl, 0Dh
        je skip_read_ofn

        mov [di], dl
        inc si
        inc di
    loop read_ofn_loop

skip_read_ofn:

ret
endp
proc createOutFile
	mov ax, 3c00h
	xor cx,cx
	mov dx, offset outputFilename
	int 21h
    jc create_of_error
	mov outFilehandle, ax
    ret

create_of_error:
    mov dx, offset create_of_error_msg
    mov ah, 09h                
	int 21h 
    mov ax, 4C00h
    int 21h
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

proc closeFiles
	mov ah, 3eh 			    ;close input file
	mov bx, offset filehandle	;BX = file handle
	int 21h

    mov ah, 3eh 			    	;close outputFile
	mov bx, offset outFilehandle    ;BX = file handle
	int 21h
ret
endp

start:
    mov ax, @DATA
	mov ds, ax

    call inlineArg
	call openInFile
	call createOutFile

    mov index, offset outBuff
    mov bx, offset hello
    call strcpy
    mov bx, offset world
    call strcpy
    
	mov ah, 40h         ; DOS 2+ - WRITE - WRITE TO FILE OR DEVICE
    mov bx, 1           ; File handle = STDOUT
	mov cx, 225
	mov dx, offset outBuff
	int 21h

    mov ah, 09h                
 	mov dx, offset msg 
	int 21h 
    call closeFiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	mov ah, 4ch               
 	int 21h  




end start