;constants for clearer code
STDIN equ 0
STDOUT equ 1
STDERR equ 2

SYS_READ equ 0
SYS_WRITE equ 1
SYS_EXIT equ 60

section .data
	inputChunk db 1

section .bss
	inputText resb 20
	output resb 1

section .text
	global _start


;exit macro
%macro exit 1
    mov 	rax, SYS_EXIT
    mov 	rdi, %1
    syscall
%endmacro

;//////////////////////////////////////////////////////////////////
_start:
;get command line args section
	mov 	rbp, rsp 	;rsp is dynamic in its nature so using rbp is more consistent and easier
						;from now on rbp will be constant and pointing to argc on stack
	call 	_checkArgsValid

;get section
;change section
;print section
;exit codes section
_exit:
	exit 0
_exit1:
	exit 1
_exit2:
	exit 2

;///////////////////////////////////////////////////////////////////
;check line arguments validity
_checkArgsValid:
	mov 	r9,	rbp		;we need to leave rbp constant
	mov 	rax, [r9]	;get top of the stack to rax
	cmp 	rax, 1		;if rax (argc) is 1 then we have 0 args
	je 		_exit1		;if rax = 1 then invalid

	mov 	r8, [r9]	;r8 will be loop counter
	dec		r8			;we in reality have argc - 1 args

	add		r9, 8		;set r9 ptr to path
_checkerLoop:
	add 	r9, 8		;set rbp ptr to next arg
	mov 	rdi, [r9]	;_stringToInt gets rdi as an argument so we mov line arguments there
						;as we are in 64bit system first command line arg will be at 16 bytes offset
	call	_stringToInt
	mov		[r9], rax	;put converted value back

	dec 	r8			;loop logic section
	cmp 	r8, 0
	jne		_checkerLoop

	call 	_writeArgs
	ret


;///////////////////////////////////////////////////////////////////
;string to int subroutine
;takes rdi as argument, returns rax
_stringToInt:
    xor 	rax, rax	;set rax to 0
_stoiLoop:
    movzx 	rsi, byte [rdi]	;get character stored at rdi
    cmp 	rsi, 0    	;check \0
    je 		_stoiEnd

    cmp 	rsi, 48     ;checking if char is between 48 and 57
    jb 		_exit2
    cmp 	rsi, 57
    ja 		_exit2

    sub 	rsi, 48     ;conversion to int value

    imul 	rax, 10		;*10 + newInt
    add 	rax, rsi

    inc 	rdi         ;get next char
    jmp 	_stoiLoop	;loop back
_stoiEnd:
	ret


;loop write argument line args
_writeArgs:
	mov 	r9, rbp	
	mov 	r8, [rbp]
	dec		r8

	add		r9, 8
_writeLoop:
	add		r9, 8
	mov 	rax, [r9]
	mov 	[output], rax
	mov		rax, SYS_WRITE
	mov		rdi, STDOUT
	mov		rsi, output
	mov		rdx, 1
	syscall

	dec 	r8
	cmp		r8, 0
	jne		_writeLoop

	ret