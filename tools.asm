; =================== UTILITIES ===================== ;
; this file provides utility macros                   ;
; and procedures for assembly 8086 programs.          ;
; # Usage:                                            ;
; - put this line under CODESEG in your program:      ;
; include "tools.asm"                                 ;
; made by Nir Yona                                    ; 
; =================================================== ;

DATASEG
  TOOLS_newline db 10, 13, '$'
  TOOLS_buffer db ?, ?, 100 dup(?), '$'
  
  ; (search: colors list color list clrs list)
  ; from https://yassinebridi.github.io/asm-docs/8086_bios_and_dos_interrupts.html#attrib
  BLACK          equ 0000b
  BLUE           equ 0001b
  GREEN          equ 0010b
  CYAN           equ 0011b
  RED            equ 0100b
  MAGENTA        equ 0101b
  BROWN          equ 0110b
  LIGHT_GRAY     equ 0111b
  DARK_GRAY      equ 1000b
  LIGHT_BLUE     equ 1001b
  LIGHT_GREEN    equ 1010b
  LIGHT_CYAN     equ 1011b
  LIGHT_RED      equ 1100b
  LIGHT_MAGENTA  equ 1101b
  YELLOW         equ 1110b
  WHITE          equ 1111b
  ; more colors at https://imgur.com/a/Bwg8pVC

  SCREEN_HEIGHT equ 200
  SCREEN_WIDTH  equ 320
CODESEG

; same as pusha
macro pushh
  push ax bx cx dx di si es 
endm

; same as popa
macro popp
  pop es si di dx cx bx ax
endm


macro pstart
  push bp
  mov bp, sp
  pushh
endm

macro pend retCount
  popp
  pop bp

  ifnb <retCount>
    ret retCount
  endif

  ret
endm

; ========================================
; Description: Print a number.
; Input  : (By order) [1] num.
; Output : Number on screen.
; Example:
;		push num
;		call ShowDecimal
; ========================================
proc ShowDecimal
  pstart

	; ------------
	; Stack State:
	; | [bp + 4] |
	; |   num    |
	; ------------
  mov ax, [bp + 4]

	; check if negative
	test ax, 08000h
	jz PositiveAx
	
  ; negative!
	; put '-' (minus) on the screen
	push ax
	mov dl,'-'
	mov ah,2
	int 21h
	pop ax

	neg ax ; make it positive
  
  PositiveAx:
    mov cx,0   ; will count how many time we did push 
    mov bx,10  ; the divider

  put_mode_to_stack:
    xor dx,dx
    div bx
    add dl,30h
	  ; dl is the current LSB digit 
	  ; we cant push only dl so we push all dx
    push dx    
    inc cx
    cmp ax,9   ; check if it is the last time to div
    jg put_mode_to_stack

	  cmp ax,0
	  jz pop_next  ; jump if ax was totally 0
    add al,30h  
	  mov dl, al    
  	mov ah, 2h
	  int 21h        ; show first digit MSB
	       
  pop_next: 
    pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	  mov dl, al
    mov ah, 2h
	  int 21h        ; show all rest digits
    loop pop_next
		   
  pend 2
endp ShowDecimal

; ===============================================
; [MACRO] PRINTV
; Desc:  Print a string and replace $ with the
;				 given variable
; Usage: PRINTV <msg>, <v1>
; Exmpl: PRINTV "num is $", ax
; ===============================================
macro printv msg, v1
  LOCAL skip_dcl, msg_dcl, msg_len, iter_msg, print_var, next, done
  pushh

  JMP skip_dcl

    msg_dcl db msg
    msg_len = $-msg_dcl

  skip_dcl:
  
  mov di, v1

  xor si, si
  iter_msg:
    cmp si, msg_len
    je done

    mov al, [cs:msg_dcl+si]
 
    cmp al, '$'
    je print_var
    
    ;print_char
    mov ah, 2
    mov dl, al
    int 21h
    jmp next

    print_var:
    push di
    call ShowDecimal
  
    next:
    inc si
    jmp iter_msg

  done:
  popp
endm


; ===============================
; Description: Print a newline.
; Output : Newline on the screen.
; Example:
;		call Newline
; ===============================
proc Newline
  push ax dx

  mov dx, offset TOOLS_newline
  mov ah, 09h
  int 21h

  pop dx ax
  ret
endp Newline

; =========================================
; Description: Set to video mode (320x200).
; Output : Video mode.
; Example:
;		call videomode
; =========================================
proc videoMode
  push ax
  
  mov al, 13h
  mov ah, 0
  int 10h
  
  mov ax, 1  ; show cursor
  int 33h


  pop ax
  ret
endp videoMode

; macro: set to video mode (320x200)
macro video
  call videomode
endm

; macro: set to text mode (80x25)
macro text
  push ax
  mov ax, 0003h
  int 10h
  pop ax
endm

proc textMode
  push ax

  mov al, 03h
  mov ah, 0
  int 10h

  pop ax
  ret
endp textMode


; this macro prints a string that is given as a parameter, example:
; PRINT 'hello world!'
; NOTE: adds newline
macro println msg
  LOCAL skip_dcl, msg_dcl
  push ax dx ds

  JMP skip_dcl
      msg_dcl db msg, 10, 13, '$'

  skip_dcl:
    ; make codeseg the default segment so
    ; we can access it via offset <varname>
    mov ax, cs
    mov ds, ax

    mov dx, offset msg_dcl
    mov ah, 09h
    int 21h

  ; rollback
  pop ds dx ax
endm

; this macro prints a string that is given as a parameter, example:
; PRINT 'hello world!'
macro print msg
  LOCAL skip_dcl, msg_dcl
  push ax dx ds

  JMP skip_dcl
      msg_dcl db msg, '$'

  skip_dcl:
    ; make codeseg the default segment so
    ; we can access it via offset <varname>
    mov ax, cs
    mov ds, ax

    mov dx, offset msg_dcl
    mov ah, 09h
    int 21h

  ; rollback
  pop ds dx ax
endm

macro WaitKey
  mov ah, 0
  int 16h
endm


; this macro prints the variable whose offset is in si with given length (byte length)
; msg db "hello" 
; push si
; mov si, offset msg 
; PRINT 5 
; pop si
; OUTPUT: "hello" (adds a newline)
macro printvl len
  local iter_ofst
  
  push ax bx dx si

  xor bx, bx
  mov bl, len

  xor bx, bx
  iter_ofst:
    mov dl, [si + bx]  
    mov ah, 2
    int 21h

    inc bx
    cmp bl, len
    jl iter_ofst

  mov dx, offset TOOLS_newline
  mov ah, 09h
  int 21h

  pop si dx bx ax

endm


; prints a string with given color
; ARGUMENTS: color , message 
; list of color attributes / colors attributes (clrs) are available on DSEG
macro printc color, msg
  LOCAL skip_dcl, msg_dcl, msg_len, iter_msg
  push ax bx cx dx

  JMP skip_dcl
      msg_dcl db msg
      msg_len dw $-msg_dcl

  skip_dcl:
  ; colors
  ; use this code for compatibility with dos/cmd prompt full screen mode: 
  mov     ax, 1003h
  mov     bx, 0   ; disable blinking. 
  int     10h

  mov al, ' '   ; placeholder
  mov bl, color
  mov cx, [cs:msg_len]
  mov ah, 09h
  int 10h

  xor bx, bx
  iter_msg:
    mov ah, 02h
    mov dl, [cs:msg_dcl + bx]
    int 21h

    inc bx
    cmp bx, [cs:msg_len]
    jl iter_msg


  ; rollback
  pop dx cx bx ax
endm

; same as `printc` except it also puts a newline at the end
; ARGUMENTS: color (search "clrs" for list), message 
macro printcl color, msg
  printc color, msg
  call newline
endm


; prints a character with given color number of times
; ARGUMENTS: color (search "clrs" for list), times to print, char 
macro putcch color, times, char
  push ax bx cx

  ; colors
  ; use this code for compatibility with dos/cmd prompt full screen mode: 
  mov     ax, 1003h
  mov     bx, 0   ; disable blinking. 
  int     10h
  mov al, char
  mov bl, color
  mov cx, times
  mov ah, 09h
  int 10h

  ; rollback
  pop cx bx ax
endm

; moves the cursor back number of colors
; ARGUMENTS: columns to go back.
; macro curback columns
;   push ax cx dx
;   ; get cursor position
;   mov ah, 03h
;   int 10h ; column in DL
;   print "before:"
;   xor cx, cx
;   mov cl, dl
;   push cx
;   call ShowDecimal
;   sub dl, columns
;   print "after:"
;   xor cx, cx
;   mov cl, dl
;   push cx
;   call ShowDecimal

;   ; set cursor positon (DL, DH)
;   mov ah, 02h
;   int 10h
;   pop dx cx ax
; endm


; =================================================================
; Descrpt: Gets input from keyboard.
; Input  : (By order) [1] inputMaxLen, [2] dataVar, [3] dataLenVar.
; Output : Output on screen.
; =================================================================
proc KeyboardInput
  push bp
  mov bp, sp

  ; --------------------------------------
  ; Stack State:
  ; |   bp + 4   |  bp + 6 |    bp + 8   |
  ; | dataLenVar | dataVar | inputMaxLen |
  ; --------------------------------------

  mov bx, [bp + 8]
  inc bl
  mov [offset TOOLS_buffer], bl
  mov dx, offset TOOLS_buffer
  mov ah, 0ah
  int 21h

  mov bx, [bp + 4]
  xor ax, ax
  mov al, [TOOLS_buffer + 1]
  mov [bx], ax

  xor bx, bx
  @@save_buffer:
    mov al, [TOOLS_buffer + bx + 2]
    mov si, [bp + 6]
    mov [si + bx], al

    inc bx
    cmp bl, [TOOLS_buffer + 1]
    jl @@save_buffer
  
  call newline

  pop bp
  ret 6
endp KeyboardInput


; draws black in coord
macro draw x,y
  push ax bx cx dx si
  mov ah, 0Ch
  mov al, 0CH
  mov cx, x
  mov dx, y
  int 10h
  pop si dx cx bx ax
endm

; draws black in coord
macro drawc x,y, c
  push ax bx cx dx si
  mov ah, 0Ch
  mov al, c
  mov cx, x
  mov dx, y
  int 10h
  pop si dx cx bx ax
endm



; =============================================================================
; Description: Open a file in read mode, if it doesnt exist filehandle=-1.
; Input  : (By order) [1] offset filename, [2] offset filehandle.
; Output : Updated filehandle, if error/file does not exist then filehandle=-1.
; Example:
;		push offset filename
;		push offset filehandle
;		call OpenRead
; =============================================================================
proc OpenRead
  push bp
  mov bp, sp
  pushh

  ; ---------------------------------------
	; Stack State:
	; |      [bp + 4]     |     [bp + 6]    |
	; | offset filehandle | offset filename |
  ; ---------------------------------------
  mov si, [bp+4]
  mov di, [bp+6]

  ; try to open
  mov ah, 03Dh  ; open file
  mov al, 0     ; read mode
  mov dx, [bp+6]
  int 21h  ; filehandle in AX
  jc @@open_error

  ; no error
  mov bx, [bp + 4]
  mov [bx], ax  ; update filehandle
  jmp @@end

  @@open_error:
  printv "cant open file in read mode (code: $)",ax
  println ''
  mov bx, [bp + 4]
  mov [word bx], -1  ; update filehandle

 
  @@end:
  popp
  pop bp
  ret 4
endp OpenRead

; -----------------------
; opens a file for write, and if doesnt exist creates
; USAGE:
; push offset filename
; push offset filehandle
; call OpenWrite
; ------------------------  
proc OpenWrite
  push bp
  mov bp, sp
  pushh

  ; SS
  ; [bp+4] offset handle
  ; [bp+6] offset fname

  ; try to open
  mov ah, 3Dh
  mov al, 1
  mov dx, [bp + 6]
  int 21h
  jc @@may_not_exist

  ; exists
  mov bx, [bp + 4]
  mov [bx], ax
  jmp @@end

  @@may_not_exist:
  cmp ax, 2
  jne @@open_error

  ; create file
  xor cx, cx
  mov dx, [bp + 6]
  mov ah, 3Ch
  int 21h
  jc @@open_error
  
  mov bx, [bp + 4]
  mov [bx], ax
  jmp @@end

  @@open_error:
  println "cant open file for write"
  mov bx, [bp + 4]
  mov [word bx], -1

  @@end:
  popp
  pop bp
  ret 4
endp OpenWrite

; -----------------------
; closes a file 
; USAGE:
; push [filehandle]
; call CloseFile
; ------------------------
proc CloseFile
  push bp
  mov bp, sp
  push ax bx cx dx si

  ; SS:
  ; [bp+4]: file handle

  mov bx, [bp+4]

  cmp bx, -1  ; invalid handle
  je @@end

  mov ah, 3Eh
  int 21h

  @@end:
  pop si dx cx bx ax
  pop bp
  ret 2
endp CloseFile

macro pixel color, x, y
  push ax bx cx dx si

  mov cx, x
  mov dx, y
  mov ah, 0Ch
  mov al, color
  int 10h

  pop si dx cx bx ax
endm



; checks for a keypress and if clicked CF=1 otherwise CF=0
macro zkey key
  local done, no
  
  pushh
  
  ; check keys in buffer
  mov ah, 1
  int 16h
  jz no
  
  ; read keys in buffer
  xor ah, ah
  int 16h
  
  ; is requested key
  cmp al, key
  jne no

  ; yes, CF=1
  stc
  jmp done

  ; no, CF=0
  no:
  clc

  done:
  popp
endm


; this macro accepts a timer var
; and sets its value to current time
macro resetTimer timer
    pushh
    mov ah, 0
    int 1Ah
    mov [offset timer], dx
    popp
endm

; this macro accepts a timer var, ticks cooldown and callback proc
; and calls the callback if ticks cooldown time has passed :-))))
macro checkTimer timer, ticks, callback
  local endmac, set_carry
  pushh
  mov ah, 0
  int 1Ah

  sub dx, [offset timer]
  cmp dx, ticks
  popp

  jge set_carry
  jmp endmac

  set_carry:
  resetTimer timer
  call callback

  endmac:
endm

; macro: sets video mode background color to input
macro bgset color
  pushh

  ; apply stosb instr to the video mode area
  mov ax, 0A000h
  mov es, ax

  xor di, di  ; start from 0
  cld ; and go forward

  mov al, color

  mov cx, 320*200
  rep stosb
  
  popp
endm

; macro: sets text mode background color to input
macro bgtextset color
  pushh

  ; apply stosb instr to the video mode area
  mov ax, 0B800h
  mov es, ax

  xor di, di  ; start from 0
  cld ; and go forward

  mov ah, color
  mov al, '?'

  mov cx, 80*25
  rep stosw
  
  popp
endm



; MACRO (TEXT MODE)
; put a character with color at x,y coords
; ARGUMENTS: char, color, x, y
macro char chr, color, x, y
  pushh

  ; store cursor position
  mov ah, 03h
  int 10h
  push dx

  xor bh, bh  ; always page 0

  ; set cursor position
  mov ah, 02h
  mov dl, x
  mov dh, y
  int 10h

  ; write char with attr
  mov ah, 09h
  mov al, chr
  mov bl, color
  mov cx, 1
  int 10h

  ; recall cursor position
  mov ah, 02h
  pop dx
  int 10h

  popp
endm


; calls given procedure if je
macro calleq proc
  local endmac

  jne endmac
  call proc

  endmac:
endm

macro calleq_then proc, thenJmp
  local endmac

  jnc endmac

  call proc
  jmp thenJmp

  endmac:
endm

; calls first param proc if je else second param
macro calleqelse procif, procelse
  local endmac, noteq

  jne noteq
  call procif
  jmp endmac

  noteq:
  call procelse

  endmac:
endm

; retrives video mode fixed cursor position in CX=x DX=y, BX=click type
macro cursor
  mov ax, 3
  int 33h

  shr cx, 1
endm

; LOGIC: alsoif
; used after a cmp for je
; if je in cmp AND if je in alsoif macro then
macro alsoif a, b
  local endmac

  jne endmac

  cmp a, b
  jne endmac

  endmac:
endm

; checks if number is in range of two numbers
; if it is then JE else JNE
macro inrange min, cur, max
  local bad, endmm

  cmp cur, min
  jl bad
  cmp cur, max
  jg bad

  cmp ax, ax
  jmp endmm

  bad:
  push ax bx
  mov ax, 4
  mov bx, 2
  cmp ax, bx
  pop bx ax

  endmm:
endm


; gets a pressed key (if pressed CF=1 and key in AL else CF=0)
macro getkey
  local nokey, e

  mov ah, 01h
  int 16h
  jz nokey
  
  mov ah, 0
  int 16h
  stc

  jmp e
  
  nokey:
  clc

  e:
endm

; calls a given procedure if "jc"
macro callc proc
  local no_carry

  jnc no_carry

  call proc

  no_carry:
endm


; clear video mode screen
proc clearvideo
  pushh

  ; hide cursor
  mov ax, 2
  int 33h

  ; apply stosb instr to the video mode area
  mov ax, 0A000h
  mov es, ax

  xor di, di  ; start from 0
  cld ; and go forward

  mov al, BLACK

  mov cx, 320*200
  rep stosb
 

  ; put cursor to 0,0
  xor dx, dx
  xor bx, bx
  mov ah, 02h
  int 10h

  ; show cursor
  mov ax, 1
  int 33h

  popp
  ret
endp clearvideo


; prints a byte value as decimal
macro printb n
  push ax
  xor ah, ah
  mov al, n
  push ax
  call showdecimal
  pop ax
endm

macro btnrange X, Y, minX, addX, minY, addY
  local endi

  inrange minX, X, minX+addX
  jne endi

  inrange minY, Y, minY+addY

  endi:
endm





; print a string and replace $ with two variables by order
; EXAMPLE: set ax, 10; set bx, 20; printvv "($,$)", ax, bx
; will do print "(" + call showdecimal(ax) + print "," + call showdecimal(bx) + print ")"
macro printvv msg, v1, v2
  LOCAL skip_dcl, msg_dcl, msg_len, iter_msg, print_var, next, done, pv
  pushh

  JMP skip_dcl
      msg_dcl db msg
      msg_len = $-msg_dcl
  skip_dcl:
  
  push v2
  push v1

  xor di, di  ; var counter
  xor si, si
  iter_msg:
    cmp si, msg_len
    je done

    mov al, [cs:msg_dcl+si]
 
    cmp al, '$'
    je print_var
    
    ;print_char
    mov ah, 2
    mov dl, al
    int 21h
    jmp next

    print_var:
    inrange 0, di, 1
    je pv
    jmp next  ; if there are more than two $s then ignore

    pv:
    call ShowDecimal  ; uses item in stack
    inc di
    jmp next

    next:
    inc si
    jmp iter_msg

  done:
  popp
endm


; print a string and replace $ with three variables by order
; EXAMPLE: set ax, 10; set bx, 20; set cx, 30; printvvv "($,$,$)", ax, bx, cx
; will do print "(" + call showdecimal(ax) + print "," + call showdecimal(bx) + print "," + call showdecimal(cx) + print ")"
macro printvvv msg, v1, v2, v3
  LOCAL skip_dcl, msg_dcl, msg_len, iter_msg, print_var, next, done, pv
  pushh

  JMP skip_dcl
      msg_dcl db msg
      msg_len = $-msg_dcl
  skip_dcl:
  
  push v3
  push v2
  push v1

  xor di, di  ; var counter
  xor si, si
  iter_msg:
    cmp si, msg_len
    je done

    mov al, [cs:msg_dcl+si]
 
    cmp al, '$'
    je print_var
    
    ;print_char
    mov ah, 2
    mov dl, al
    int 21h
    jmp next

    print_var:
    inrange 0, di, 2
    je pv
    jmp next  ; if there are more than two $s then ignore

    pv:
    call ShowDecimal  ; uses item in stack
    inc di
    jmp next

    next:
    inc si
    jmp iter_msg

  done:
  popp
endm


; just equal extended (long range)
macro jxe whereto
  local nono

  jne nono

  jmp whereto

  nono:
endm

; jump carry extended (long range)
macro jxc whereto
  local nono

  jnc nono

  jmp whereto

  nono:
endm


proc SeekFile
  push bp
  mov bp, sp
  pushh

	; --------------------------------------
	; Stack State:
	; |   [bp + 4]   | [bp + 6] | [bp + 8] |
	; | originOffset |  origin  |  handle  |
	; --------------------------------------

  mov bx, [bp+8]
  mov al, [bp+6]
  mov dx, [bp+4]
  mov ah, 42h
  xor cx, cx
  int 21h 



  popp
  pop bp
  ret 6
endp SeekFile

; seek file short macro
macro seekf handle, origin, originOffset
  push handle
  push origin
  push originOffset
  call SeekFile
endm

; open file for read short macro
macro openr filename, handleoffset
  push offset filename
  push offset handleoffset
  call OpenRead
endm

; open filef for write short macro
macro openw filename, handleoffset
  push offset filename
  push offset handleoffset
  call OpenWrite
endm

; close file short macro
macro closef handle
  push handle
  call CloseFile
endm

; read from file short macro
macro readf handle, howMany, saveTo
  pushh
  mov ah, 03Fh
  mov bx, handle
  mov cx, howMany
  mov dx, offset saveTo
  int 21h
  popp
endm

; write to file short macro
macro writef handle, howMany, what
  pushh
  mov ah, 40h
  mov bx, handle
  mov cx, howMany
  mov dx, offset what
  int 21h
  popp
endm



