.model tiny
.code
org 100h
start:
	div dx

	mul dx
	xchg ah, bl
	
	idiv cx
	idiv bx

	int 1h
	int 10h
	int 21h
	test ah,al

	xchg ax, ax
	xchg ch, al

	in ax, dx
	in al, dx

	iret

	test ah,BL
	test ch, dl

	xchg ah, dh

	les ax,[di]
	les bx, [si]


	idiv bx
	div dx

	mov ax, 4c00h
	int 21h

end start