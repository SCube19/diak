;constants for cleaner code
STDIN 				equ 0
STDOUT 				equ 1

SYS_READ 			equ 0
SYS_WRITE 			equ 1
SYS_EXIT 			equ 60

;utf-8 constants
TWOBYTEMIN			equ	0x80
THREEBYTEMIN 		equ 0x800
FOURBYTEMIN			equ 0x10000
UTF8MAX				equ 0x10FFFF

;other constants for clarity and easier refactoring
dataChunk 			equ 8192
moduloVal 			equ 0x10FF80

section .data

section .bss
	inputBuff 		resb dataChunk	;actual input buffer	
	outputBuff 		resb dataChunk	;output buffer
	inputOneBuff	resb 1			;helper buffer for easier code
	
section .text
	global	 _start

;------------------------------------------------------------------
;exit macro
%macro exit 1
    mov 	rax, SYS_EXIT
    mov 	rdi, %1
    syscall
%endmacro

;get into some buffer macro
%macro gInBuff 3
	mov		rax, SYS_READ
	mov		rdi, STDIN
	mov		rsi, %1
	mov		rdx, %2
	syscall

	cmp 	rax, 0		;rax = 0 means eof
	je		%3
%endmacro

;write given amount of bytes onto stdout macro
%macro wOutBuff 1
	mov		rax, SYS_WRITE
	mov		rdi, STDOUT
	mov		rsi, outputBuff
	mov		rdx, %1
	syscall	
%endmacro

;shifts %1 left and right by %2 bits
;to set first %2 bits to 0
%macro shCln 2
	shl		%1, %2
	shr		%1, %2
%endmacro

;modulo rax % %1 and store in rax
%macro modulo 1
	xor 	rdx, rdx	;rdx must be 0 to not mess up a division
	div		%1
	mov 	rax, rdx
%endmacro


;-----------------------------------------------------------------

;//////////////////////MAIN CODE BLOCK///////////////////////////////
_start:
	mov 	rbp, rsp 	;rsp is dynamic in its nature so using rbp is more consistent and easier
						;from now on rbp will be constant and pointing to argc on stack
	call 	_convertArgs

	push 	r12			;we will use them in _diakAlg and they should remain unchanged 
	push	r13			;(although it doesnt really matter in the context of the program)
	push	r15
	xor		r13, r13	;it will hold outputBuffer byte offset
	call	_diakAlg	;actual diakrynizator

;exit codes section
_exit:
	call 	_endSequence
	exit 	0
_exit1:
	call	_endSequence
	exit 	1

_endSequence:
	pop 		r9		;saving function call
	wOutBuff 	r13		;writing what's left
	pop 		r15		;restoring rX registers
	pop			r13
	pop			r12
	push		r9
	ret
;/////////////////////////////////////////////////////////////////////

;/////////////////////////DIAK ALG BLOCK//////////////////////////////
_diakAlg:
	gInBuff	inputBuff, dataChunk, _exit	;dataChunk sized block gets into the buffer
	xor		r12, r12			;we will store number of alredy diakrynized bytes in r12
	mov 	r15, rax			;r15 will hold number of read bytes
_diakAlgLoop:	
	call 	_toUnicode			;based on first byte it converts multiple utf-8 bytes to single unicode value stored in rax, and length in rdx
	cmp 	rdx, 1				;if length is 1 then char is utf-8 valid and we wont diakrynize it as it's below threshold 
	je		_store

	mov 	rdi, rdx			;good practice tells to make rdi and rsi function args
	mov		rsi, rax

	call	_shortestPossible	;check if given char is utf-8 valid - its written in the shortest way possible

	call 	_diakrytynization	;diakrytynization function, calculation is stored in rax

	mov 	rsi, rax			;again good practice to store unicode in rsi
	call	_toUtf				;converts unicode into utf8(rax) and gives it's byte length(rdx)

_store:
	push	rax					;store rax on the stack
	push 	rdx					;store rdx on the stack
	mov 	rax, r13			;if character is too much for a buffer just print it and reset r13
	add 	rax, rdx			;r13 would be r13 + rdx after storing
	cmp		rax, dataChunk
	je		_doesntFit
	jmp		_doesFit
_doesntFit:
	call 	_printer
_doesFit:
	pop		rdi					;restore rdx into rdi
	pop 	rsi					;restore rax into rsi
	
	call	_storeInOutBuff

	cmp		r13, dataChunk		;if buffer is full write it 
	jae		_dumpOut
	jmp		_notDump
_dumpOut:
	call 	_printer
_notDump:
	cmp		r12, dataChunk		;check if our buffer has been computed
	jae		_diakAlg			;if so then jump back to get another buffer filled
	jmp 	_diakAlgLoop		;loop back with the same input buffer
;////////////////////////////////////////////////////////////////////////////

;//////////////////////PRINTER BLOCK/////////////////////////////////////////
;prints output buffer
;TAKES r13
;CHANGES r13, rax, rdx, rsi, rdi
_printer:
	wOutBuff	r13				;write output buffer
	xor 		r13, r13		;reset r13 offset
	ret
;//////////////////////////////////////////////////////////////////////

;//////////////////////////CHECK BUFF FOR EOF BLOCK//////////////////////////////////
;checks for eof
;TAKES r8, r12, r15
;CHANGES nothing
_checkBuffForEOF:	
	cmp		r8, 0		;0 means that we probably are at eof 
	je		_andCheck
	ret
_andCheck:
	cmp		r12, r15	;it ensures EOF
	jae		_exit
	ret
;////////////////////////////////////////////////////////////////////////////////////

;////////////////////////DIAKRYTYNIZATION BLOCK//////////////////////////////////////
;calculate diakrynized unicode value and store in rax
;TAKES rsi, (rbp)
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
	mul 	rsi			;calc x^(some power + 1)
	modulo 	r8			;modulo x^some power
	mov 	r10, rax	;get it back into r10

	loop	_dLoop		;LOOP END

_diaEnd:
	mov 	rax, r11	;get result into rax
	modulo	r8			;we modulo the result
	add 	rax, 0x80 	;w() + 0x80
	ret
;////////////////////////////////////////////////////////////////////////////////////

;/////////////////////////TO UTF BLOCK///////////////////////////////////////////////
;convert unicode to utf8 for output purposes, calculates utf-8 char in rax, and its byte length in rdx
;TAKES	rsi
;CHANGES r9, r10, rax, rcx, rdx
_toUtf:
	xor 	r9, r9
	mov		r9b, 11000000b 		;will be used to set first bits of the first byte
	mov 	r10, 1				;indicates number of bytes after the first one
	cmp		rsi, THREEBYTEMIN 	;simple comparisons of unicode value to determine byte length
	jb		_toUtfConv

	add		r9b, 00100000b		;add next 1
	inc		r10
	cmp		rsi, FOURBYTEMIN
	jb		_toUtfConv

	add		r9b, 00010000b		;character is 4 byte so 11110xxx
	inc		r10
_toUtfConv:
	mov		r8, rsi			;copying unicode value to manipulate it and store overall result
	
	mov 	rax, 6			;get 6*r10 into rcx 
	mov		rcx, r10
	mul		rcx
	mov 	rcx, rax

	shr		r8d, cl			;6*bytes shift to right will leave only significant bits of the first byte
	add		r8b, r9b		;finishing to construct first byte

_toUtfLoop:
	sub		rcx, 6			;rcx is loop counter as well as shitf left number so that we end up with next 6bits at the end of al 
	shl		r8d, 8			;make room for next byte

	mov		rax, rsi		;copying for bit manipulation
	shr		eax, cl			;shifting so that we end up with needed 6 bits at the end of al
	shCln	al, 2			;cleaning first slots of al
	add		al, 10000000b	;setting byte to 10xxxxxx

	add		r8b, al			;adding byte

	cmp		rcx, 0			;looping
	jne		_toUtfLoop

	mov		rax, r8			;returning calculation in rax
	mov		rdx, r10		;returning calculation in rdx
	inc		rdx				;returning byte length in rdx (it will be usefull in the output)
	ret
;////////////////////////////////////////////////////////////////////////////////////

;/////////////////////////STORE IN OUT BUFF BLOCK////////////////////////////////////////////
;stores utf-8 bytes inside an output buffer
;TAKES rsi, rdi, r13
;CHANGES rsi, r8, rcx, r13, rax
_storeInOutBuff:
	cmp		rdi, 1							;choose branch
	je		_oneByteInsert
	bswap	esi								;swapping bytes as it makes inserting easier/mov is done in reverse
	cmp		rdi, 2
	je		_twoByteInsert
	cmp		rdi, 4
	je		_fourByteInsert

	
	shr		esi, 8							;now bytes are reversed as if bswap ignored 0 bytes
	mov		rcx, 3
_storingLoop:
	mov		BYTE [outputBuff + r13], sil	;inserting byte to mem
	inc		r13								;increasing offset
	shr		esi, 8							;getting next byte 

	loop	_storingLoop					;looping back
	ret

_oneByteInsert:
	mov		BYTE [outputBuff + r13], sil	;moving 1 byte (it didnt require bswap)
	inc		r13								;inc offset
	ret
_twoByteInsert:
	shr		esi, 16							;now bytes are reversed as if bswap ignored 0 bytes(it would be more efficient if bswap could take 2bytes)
	mov		WORD [outputBuff + r13], si
	add		r13, 2
	ret
_fourByteInsert:
	mov		DWORD [outputBuff + r13], esi	;only earlier bswap required
	add		r13, 4
	ret
;////////////////////////////////////////////////////////////////////////////////////

;/////////////////////////TO UNICODE BLOCK///////////////////////////////////////////
;calculates utf-8 to unicode, storing unicode in rax, and char byte length in rdx
;TAKES	r12 
;CHANGES	r12, r8, rax, rdx, r9, rcx, rsi, rdi
_toUnicode:
	
	xor 	r8, r8						;just to be sure :D its not worth 
	mov 	r8b, BYTE [inputBuff + r12] ;store definition byte in some register
	mov		BYTE [inputBuff + r12], 0	;make read byte 0 to determine eof later with ease 
	call	_checkBuffForEOF			;as name suggests

	inc		r12			;increase byte address offset
	mov		rax, r8		;if 1 byte then rax needs to contain answer
	mov 	rdx, 1		;we know that length is al least 1 (or error)

	bt		r8w, 7		;if first bit is 0 then we have ascii character (bt must have at least 16bit register so r8w instead of r8b)
	jnc		_ret		;print ascii right away
	bt		r8w, 6		;next bit must be 1 for it to be valid utf8
	jnc		_exit1		;exit error

	;trying to make a loop out of it was troublesome as bt instruction doesnt take <register, register> pair
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
	mov		r9, rcx		;loop counter as syscalls kept changing rcx 
	shCln	r8b, cl		;cleaning first byte off of length bits
	dec		r9			;if length is 2 then we need 1 loop pass etc.

_uniLoop:
	cmp		r12, dataChunk				;if r12 is >= dataChunk then we are overflowing the buffer
	jb		_uniLoopInsert				;and we need to get some more bytes via syscalls (byte by byte for simplicity)

	gInBuff	inputOneBuff, 1, _exit1		;get 1 byte into memory and exit error if eof now
	
	jmp		_uniLoopInsert2				;jump to mov al, [inputOneBuff]

_uniLoopInsert:
	mov 	al, BYTE [inputBuff + r12] 	;store next byte in al
	mov		BYTE [inputBuff + r12], 0	;again make read byte 0 for eof check
	inc		r12							;increase byte address
	jmp 	_restOfUniLoop

_uniLoopInsert2:
	mov		al, [inputOneBuff]			;store 1 byte input 

_restOfUniLoop:
	shCln 	al, 2						;clean the definition bytes

	shl		r8d, 6						;glueing operation specified on wikipedia
	add 	r8d, eax

	dec 	r9
	cmp		r9, 0
	jne		_uniLoop					;looping

	pop 	rdx			;restore length info
	mov		rax, r8		;make rax an output
	ret

	
;///////////////////////////////////////////////////////////////////////////////////

;/////////////////////////SHORTEST POSSIBLE BLOCK///////////////////////////////////
;checks if utf-8 char is written in shortest way possible 
;TAKES rsi, rdi
;CHANGES flags
_shortestPossible:
	cmp 	rsi, UTF8MAX		;4 byte max check
	ja		_exit1

	cmp 	rdi, 2				;choose branch
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
	mov 	r10, moduloVal	;for moduling converted value
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

	modulo	r10			;modulo converted value
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
    xor 	rax, rax		;set rax to 0
_stoiLoop:
    movzx 	rsi, byte [rdi]	;get character stored at rdi
    cmp 	rsi, 0    		;check \0
    je 		_ret

    cmp 	rsi, 48     	;checking if char is between 48 and 57
    jb 		_exit1
    cmp 	rsi, 57
    ja 		_exit1

    sub 	rsi, 48     	;conversion to int value

    imul 	rax, 10			;*10 + newInt
    add 	rax, rsi

    inc 	rdi         	;get next char
    jmp 	_stoiLoop		;loop back
;///////////////////////////////////////////////////////////////////
;label for convience as you can simply do jmp 	_ret instead of making different ret label for every subroutine
_ret:			
	ret
