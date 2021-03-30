;constants for cleaner code
STDIN 	equ 0
STDOUT 	equ 1
STDERR 	equ 2

SYS_READ 	equ 0
SYS_WRITE 	equ 1
SYS_EXIT 	equ 60

;utf-8 constants
TWOBYTEMIN		equ	0x80
THREEBYTEMIN 	equ 0x880
FOURBYTEMIN		equ 0x10880
UTF8MAX			equ 0x10FFFF
;other constants for clarity and easier refactoring
inputChunk 	equ 1
moduloVal 	equ 0x10FF80

section .data

section .bss
	output 		resb 4
	inputBuff 	resb inputChunk

section .text
	global _start

;------------------------------------------------------------------
;exit macro
%macro exit 1
    mov 	rax, SYS_EXIT
    mov 	rdi, %1
    syscall
%endmacro

;get into input buffer macro
%macro gInBuff 1
	mov		rax, SYS_READ
	mov		rdi, STDIN
	mov		rsi, inputBuff
	mov		rdx, inputChunk
	syscall
	cmp 	rax, 0
	je		%1
%endmacro

;shifts %1 left and right for %2
;to set first %2 bits to 0
%macro shCln 2
	shl		%1, %2
	shr		%1, %2
%endmacro

;modulo rax % %1 and store in rax
%macro modulo 1
	xor 	rdx, rdx
	div		%1
	mov 	rax, rdx
%endmacro
;-----------------------------------------------------------------

;//////////////////////MAIN CODE BLOCK///////////////////////////////
_start:
	mov 	rbp, rsp 	;rsp is dynamic in its nature so using rbp is more consistent and easier
						;from now on rbp will be constant and pointing to argc on stack
	call 	_convertArgs
	call	_diakAlg

;exit codes section
_exit:
	exit 	0
_exit1:
	exit 	1
_exit2:
	exit 	2

;/////////////////////////DIAK ALG BLOCK//////////////////////////////
_diakAlg:
	call 	_toUnicode	;based on first byte it converts multiple utf-8 bytes to single unicode value stored in rax, and length in rdx
	cmp 	rdx, 1		;if length is 1 then char is utf-8 valid and we wont diakrynize it 
	je		_printer

	mov 	rdi, rdx	;good practice tells to make rdi and rsi function args
	mov		rsi, rax

	call	_shortestPossible	;check if given char is utf-8 valid - its written in the shortest way possible

	call 	_diakrytynization	;diakrytynization function, calculation is stored in rax

	mov 	rsi, rax	;again good practice to store unicode in rsi
	call	_toUtf
	
_printer:
	mov 	rdi, rdx	;good practice tells to make rdi and rsi function args
	mov		rsi, rax
	call	_writeUtf
	jmp 	_diakAlg
;//////////////////////////////////////////////////////////////////////

;////////////////////////DIAKRYTYNIZATION BLOCK//////////////////////////////////////
;calculate diakrynized unicode value and store in rax
;TAKES rsi 
;CHANGES rsi, r11, rcx, r9, r10, r8, rsi, rax, rdx 
_diakrytynization:
	mov		r8, moduloVal
	mov		r9, rbp		;we need to leave rbp constant
	mov		rcx, [r9]	;get argc into rcx
	dec 	rcx			;ignore path

	sub		rsi, 0x80	;w(x - 0x80)
	mov		r10, 1		;will store x^some power
	xor 	r11, r11	;init register for storing sum

	add		r9, 8		;set r9 ptr to path
_dLoop:
	add		r9, 8		;set r9 ptr to next arg

	mov 	rax, r10	;set rax to x^some power
	mul		QWORD [r9]	;multiply with arg
	modulo	r8 			;modulo the result to avoid overflow
	add		r11, rax	;add loop result to overall result
	;here we could also modulo r11 but maximum we add is 0x10FF80
	;which means we could technically put around 8279627000000 loops in r11
	;which is possible but so unlikely that modulo here would be perfomance issue

	mov 	rax, r10	;get x^some power into rax
	mul 	rsi			;calc x^some power + 1
	mov 	r10, rax	;get it back into r10

	loop	_dLoop		;LOOP END

	mov 	rax, r11	;get result into rax
	add 	rax, 0x80 	;w() + 0x80
	modulo	r8			;we modulo the very last result
	ret
;////////////////////////////////////////////////////////////////////////////////////

;/////////////////////////TO UTF BLOCK///////////////////////////////////////////////
;convert unicode to utf8 for output purposes, calculates utf-8 char in rax, and its byte length in rdx
;TAKES	rsi
;CHANGES r9, r10, rax, rcx, rdx
_toUtf:
	mov		r9b, 11000000b 		;will be used to set first bits of the first byte
	mov 	r10, 1				;indicates number of bytes after the first one
	cmp		rsi, THREEBYTEMIN 	; simple comparisons of unicode value to determine byte length
	jb		_toUtfConv

	add		r9b, 00100000b
	inc		r10
	cmp		rsi, FOURBYTEMIN
	jb		_toUtfConv

	add		r9b, 00010000b
	inc		r10
_toUtfConv:
	mov		r8, rsi		;copying unicode value to manipulate it and store overall result
	
	mov 	rax, 6		;get 6*r10 into rcx 
	mov		rcx, r10
	mul		rcx
	mov 	rcx, rax

	shr		r8d, cl		;6*bytes shift to right will leave only significant bits of the first byte
	add		r8b, r9b	;finishing to construct first byte

_toUtfLoop:
	sub		rcx, 6		;rcx is loop counter as well as shitf left number so that we end up with next 6bits at the end of al 
	shl		r8d, 8		;make room for next byte

	mov		rax, rsi		;copying for bit manipulation
	shr		eax, cl			;shifting so that we end up with needed 6 bits at the end of al
	shCln	al, 2			;cleaning first slots of al
	add		al, 10000000b	;setting byte to 10xxxxxx

	add		r8b, al			;adding byte

	cmp		rcx, 0			;Looping
	jne		_toUtfLoop

	mov		rax, r8			;returning calculation in rax
	mov		rdx, r10		;returning calculation in rdx
	inc		rdx				;returning byte length in rdx (it will be usefull in the output)
	ret
;////////////////////////////////////////////////////////////////////////////////////

;/////////////////////////WRITE UTF BLOCK////////////////////////////////////////////
;writes given utf output
;TAKES rsi, rdi
;CHANGES	rsi, r8, r9, r10
_writeUtf:
	bswap	esi				;swapping bytes as they are inserted inverted
	mov		[output], esi	;inserting to mem
	mov 	r8, rdi			;store byte length

	mov 	r9, 4			;r9 will store byte offset
	sub 	r9, r8
	mov 	r10, output		;r10 will store output+offset
	add		r10, r9

	mov 	rax, SYS_WRITE	;syscall config
	mov 	rdi, STDOUT
	mov		rsi, r10
	mov 	rdx, r8
	syscall

	ret
;////////////////////////////////////////////////////////////////////////////////////

;/////////////////////////TO UNICODE BLOCK///////////////////////////////////////////
;calculates utf-8 to unicode, storing unicode in rax, and char byte length in rdx
;TAKES	nothing
;CHANGES	r8, rax, rdx, r9, rcx, rsi, rdi
_toUnicode:
	gInBuff	_exit		;inputChunk gets into the buffer (currently 1 byte)
	mov 	r8, [inputBuff] ;store input in some register
	mov		rax, r8		;if 1 byte then rax needs to contain answer
	mov 	rdx, 1		;we know that length is al least 1 (or error)

	bt		r8w, 7		;if first bit is 0 then we have ascii character (bt must have at least 16bit register)
	jnc		_ret		;print ascii right away
	bt		r8w, 6		;next bit must be 1 for it to be valid utf8
	jnc		_exit1		;exit error

	;trying to make a loop out of it was troublesome as bt instruction doesnt take register, register pair
	inc		rdx			;inc byte length
	bt 		r8w, 5		;check next byte
	jnc		_convToUni	;if 0 then length definition has ended we can convert

	;repeat
	inc		rdx
	bt 		r8w, 4
	jnc		_convToUni

	inc		rdx
	bt 		r8w, 3
	jnc		_convToUni

	jmp		_exit1		;length 5+ indicates an error

_convToUni:
	push	rdx 		;syscalls will change rdx and we need it to store byte length
	mov 	rcx, rdx	;for shCln
	mov 	r9, rcx		;loop counter
	shCln	r8b, cl		;cleaning first byte of length bits
	dec		r9			;if length is 2 then we need 1 loop pass etc

_uniLoop:
	gInBuff	_exit1		;get next byte

	mov 	rax, [inputBuff];store it in rax
	shCln 	al, 2		;clean definition bytes

	shl		r8, 6		;glueing operation specified on wikipedia
	add 	r8, rax

	dec		r9			;loop counter operations
	cmp		r9, 0
	jne		_uniLoop

	pop 	rdx			;restore length info
	mov		rax, r8		;make rax an output
	ret
;///////////////////////////////////////////////////////////////////////////////////

;/////////////////////////SHORTEST POSSIBLE BLOCK///////////////////////////////////
;checks if utf-8 char is written in shortest way possible 
;TAKES rsi, rdi
;CHANGES flags
_shortestPossible:
	cmp 	rsi, UTF8MAX
	ja		_exit1

	cmp 	rdi, 2		;choose branch
	je		_twoB
	cmp 	rdi, 3
	je		_threeB

	cmp		rsi, FOURBYTEMIN	;comparing with constants to check validity
	jb		_exit1
	ret 
_twoB:
	cmp		rsi, TWOBYTEMIN
	jb		_exit1
	ret
_threeB:
	cmp		rsi, THREEBYTEMIN
	jb		_exit1
	ret
;///////////////////////////////////////////////////////////////////////////////////

;////////////////////////////////////////////////////////////////////////////////////
;/////////////////////CONVERT COMMAND LINE ARGS BLOCK////////////////////////////////
;convert command line args to ints
;TAKES rbp (can be considered pseudo-arg as rbp is constant in code)
;CHANGES rax, r8, r9, stack
_convertArgs:
	mov 	r9,	rbp		;we need to leave rbp constant
	mov 	rax, [r9]	;get top of the stack to rax
	cmp 	rax, 1		;if rax (argc) is 1 then we have 0 args
	je 		_exit1		;if rax = 1 then invalid

	mov 	r8, [r9]	;r8 will be loop counter
	dec		r8			;we in reality have argc - 1 args

	add		r9, 8		;set r9 ptr to path
_convertLoop:
	add 	r9, 8		;set rbp ptr to next arg
	mov 	rdi, [r9]	;_stringToInt gets rdi as an argument so we mov line arguments there
						;as we are in 64bit system first command line arg will be at 16 bytes offset
	call	_stringToInt
	mov		[r9], rax	;put converted value back

	dec 	r8			;loop logic section
	cmp 	r8, 0
	jne		_convertLoop

	ret
;///////////////////////////////////////////////////////////////////

;///////////////////////////////////////////////////////////////////
;string to int subroutine
;TAKES rdi 
;CHANGES rax, rsi, rdi
_stringToInt:

    xor 	rax, rax	;set rax to 0
_stoiLoop:
    movzx 	rsi, byte [rdi]	;get character stored at rdi
    cmp 	rsi, 0    	;check \0
    je 		_ret

    cmp 	rsi, 48     ;checking if char is between 48 and 57
    jb 		_exit2
    cmp 	rsi, 57
    ja 		_exit2

    sub 	rsi, 48     ;conversion to int value

    imul 	rax, 10		;*10 + newInt
    add 	rax, rsi

    inc 	rdi         ;get next char
    jmp 	_stoiLoop	;loop back
;///////////////////////////////////////////////////////////////////
_ret:
	ret
