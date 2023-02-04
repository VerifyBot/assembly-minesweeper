DATASEG
  ; assets
  ; {VARNAME} db NAME, 0, WIDTH, HEIGHT
  CELL db "cell.bmp", 0, 8, 8
  AIR  db "air.bmp",  0, 8, 8
  BOMB db "bomb.bmp", 0, 8, 8
  FLAG db "flag.bmp", 0, 8, 8
  ;LOGO db "logog.bmp", 0, 197, 29
  
  ; minesweeper numbers
  NUMBERS_SIGN db 0
  ms_1 db "1.bmp", 0, 8, 8  ; jump 8 bytes
  ms_2 db "2.bmp", 0, 8, 8
  ms_3 db "3.bmp", 0, 8, 8
  ms_4 db "4.bmp", 0, 8, 8
  ms_5 db "5.bmp", 0, 8, 8
  ms_6 db "6.bmp", 0, 8, 8
  ms_7 db "7.bmp", 0, 8, 8
  ms_8 db "8.bmp", 0, 8, 8

  ; font numbers
  NS_SIGN db 0
  n0 db "n0.bmp",0,7,10  ; jump 9 bytes
  n1 db "n1.bmp",0,7,10
  n2 db "n2.bmp",0,7,10
  n3 db "n3.bmp",0,7,10
  n4 db "n4.bmp",0,7,10
  n5 db "n5.bmp",0,7,10
  n6 db "n6.bmp",0,7,10
  n7 db "n7.bmp",0,7,10
  n8 db "n8.bmp",0,7,10
  n9 db "n9.bmp",0,7,10

  ; == custom fonts ==
  ; {FONT_NAME},0,CHARACTER_WIDTH,CHARACTER_HEIGHT
  ; NOTE: 2 pixels space between characters is assumed
  ;FONT1 db "font1.bmp",0,6,9


  ; big assets
  ; {VARNAME}      db NAME, 0
  ; {VARNAME}Size dw WIDTH, HEIGHT
  ; ex: 
  ;  MAINPAGE  db "mainp.bmp", 0
  ;  mainpsize dw 320, 200
  
  ; then to draw:
  ; asset NAME, startX, startY
  ; assetBig NAME, startX, startY


   
  BMP_MAX_WIDTH = 320
  BMP_MAX_HEIGHT = 200
  OneBmpLine 	db BMP_MAX_WIDTH dup (0)  ; One Color line read buffer 
  ScrLine    	db BMP_MAX_WIDTH dup (0)  ; One Color line read buffer

	; BMP File data
	FileName 	      db ?, 0
	FileHandle	    dw ?
	Header 	        db 54 dup(0)
	Palette   	    db 400h dup (0)
	BmpFileErrorMsg db 'Error At Opening Bmp File', 0dh, 0ah,'$'
	ErrorFile       db 0
	
  ; array for mouse int 33 ax=09 (not a must) 64 bytes 
	;Color db ?
	Xclick dw ?
	Yclick dw ?
	Xp dw ?
	Yp dw ?
	SquareSize dw ?
	BmpLeft dw ?
	BmpTop dw ?
	BmpColSize dw ?
	BmpRowSize dw ?

  useTrans db 0

  noPad db 0 ; for PrintNumber
CODESEG

; macro for AssetEasy (easy params)
macro asset var, startX, startY
  mov [byte useTrans], 0
  push offset var
  push startX
  push startY
  call AssetEasy
endm

macro assetTrans var, startX, startY
  mov [byte useTrans], -10
  push offset var
  push startX
  push startY
  call AssetEasy
endm

; macro for BigAssetEasy (easy params)
macro assetBig var, startX, startY
  push offset var
  push startX startY
  call BigAssetEasy
endm

; this procedure draws a small (1byte width, 1byte height)
; asset on the screen
; PARAMS BY ORDER: offset assetVar, startX, startY
proc AssetEasy
  push bp
  mov bp, sp
  pushh

  mov cx, 255
    
  ; SS:
  ; [bp+4]: startY
  ; [bp+6]: startX
  ; [bp+8]: var

  ; hide cur
  mov ax, 2
  int 33h


  mov ax, [bp+6]
  mov [BmpLeft], ax
  mov ax, [bp+4]
  mov [BmpTop],  ax

  cld
  mov ax, ds
  mov es, ax
  mov di, [bp+8]
  mov al, 0
  repne scasb

  ; width
  xor ax, ax
  mov al, [di] 
  mov [BmpColSize], ax
  ; height
  xor ax, ax
  mov al, [di + 1]
  mov [BmpRowSize], ax

  mov dx, [bp+8]
  call OpenShowBmp

  ; show cur
  mov ax, 1
  int 33h

  mov [byte useTrans], 0

  popp
  pop bp
  ret 6
endp AssetEasy


; this procedure draws a big (word width, word height)
; asset on the screen
; PARAMS BY ORDER: offset assetVar, startX, startY
proc BigAssetEasy
  push bp
  mov bp, sp
  pushh

  ; SS:
  ; [bp+4]: startY
  ; [bp+6]: startX
  ; [bp+8]: var

  ; hide cur
  mov ax, 2
  int 33h

  mov ax, [bp+6]
  mov [BmpLeft], ax
  mov ax, [bp+4]
  mov [BmpTop],  ax

  cld
  mov ax, ds
  mov es, ax
  mov di, [bp+8]
  mov al, 0
  repne scasb
  
  ; width
  mov ax, [di] 
  mov [BmpColSize], ax
  ; height
  mov ax, [di + 2]
  mov [BmpRowSize], ax

  mov dx, [bp+8]
  call OpenShowBmp

  ; show cur
  mov ax, 1
  int 33h


  popp
  pop bp
  ret 6
endp BigAssetEasy


	
;==========================
;==========================
;===== Procedures  Area ===
;==========================
;==========================

proc OpenShowBmp near	 
	call OpenBmpFile

	cmp [ErrorFile], 1
	je @@end
	
	call ReadBmpHeader
	
	call ReadBmpPalette
	
	call CopyBmpPalette
	
	call ShowBMP
	
	call CloseBmpFile

  @@end:
	ret
endp OpenShowBmp
	
; [Register DX]: filename to open
proc OpenBmpFile	near						 
	mov ah, 3Dh
	xor al, al
	int 21h

	jc @@ErrorAtOpen

	mov [FileHandle], ax

	jmp @@end
	
  @@ErrorAtOpen:
	mov [ErrorFile],1

  @@end:	
	ret
endp OpenBmpFile
 

proc CloseBmpFile near
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h

	ret
endp CloseBmpFile


; Read 54 bytes the Header
proc ReadBmpHeader	near					
	push cx
	push dx
	
	mov ah, 3fh
	mov bx, [FileHandle]
	mov cx, 54
	mov dx, offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader


proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)			
	push cx
	push dx
	
	mov ah, 3fh
	mov cx, 400h
	mov dx, offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette


; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest
proc CopyBmpPalette   near										
	push cx
	push dx
	
	mov si, offset Palette
	mov cx, 256
	mov dx, 3C8h
	mov al, 0  ; black first							
	out dx, al ;3C8h
	inc dx 	   ;3C9h

CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (losing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)		
								
	loop CopyNextColor
	
	pop dx
	pop cx
	
	ret
endp CopyBmpPalette

proc ShowBMP 
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpRowSize lines in VGA format),
; displaying the lines from bottom to top.
	push cx
	
	mov ax, 0A000h
	mov es, ax
	
	mov cx, [BmpRowSize]  ; <IMAGE HEIGHT>
	mov ax, [BmpColSize]  ; <IMAGE WIDTH>

  ; row size must divided by 4 (rowSize%4==0) so if it is less we must calculate the extra padding bytes
  ; check if (rowSize%4 == 0) and if not set remainder to bp
	xor dx,dx
	mov si, 4
	div si
	cmp dx,0
	mov bp,0
	jz @@row_ok

  ; if not, save needed padding to bp
	mov bp, 4
	sub bp, dx

  @@row_ok:	
	
  mov dx, [BmpLeft]
	
  @@NextLine:
    push cx dx
    
    mov di, cx  ; Current Row at the small bmp (each time -1)
    add di, [BmpTop] ; add the Y on entire screen
    
   
    ; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
    dec di
    mov cx, di
    shl cx, 6
    shl di, 8
    add di, cx
    add di, dx
     
    ; small Read one line
    mov ah, 3fh
    mov cx, [BmpColSize]  
    add cx,bp  ; extra  bytes to each row must be divided by 4
    mov dx, offset ScrLine
    int 21h
    
    call TransCheck
    
    pop dx cx     
    loop @@NextLine
	
	pop cx
	ret
endp ShowBMP 


proc TransCheck
  cmp [useTrans], -10
  je @@trans
  jmp @@no_trans
  
  @@trans:
  ; transparent
  push ax bx cx
  mov si,offset ScrLine
  mov cx, [BmpColSize]
  dec cx
  @@cell_draw:
    mov bx, cx
    mov al, [si + bx]
    
    cmp al, 255
    je @@next_color

    mov [es:di+bx], al

    @@next_color:
    sub cx, 1
    cmp cx, 0
    jge @@cell_draw
  pop cx bx ax

  jmp @@after_copy

  @@no_trans:
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[BmpColSize]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	@@after_copy:




  ret
endp TransCheck



proc PrintNumber
  push bp
  mov bp, sp
  pushh

  ; SS:
  ; [bp+4]: starty
  ; [bp+6]: startx
  ; [bp+8]: number to print
  mov di, [bp+4]
  mov si, [bp+6]

  ; =======================
  mov ax, [bp+8]
  mov cx,0   ; will count how many time we did push 


  mov bx,10  ; the divider
  @@_put_mode_to_stack:
    xor dx,dx
    div bx
	  ; dl is the current LSB digit 
	  ; we cant push only dl so we push all dx
    push dx    
    inc cx
    cmp ax,9   ; check if it is the last time to div
    jg @@_put_mode_to_stack
  
  cmp [byte NOPAD], 0
  jne @@no_pad
  call @@pad_zeros

  @@no_pad:
  cmp ax,0
  jz @@_pop_next  ; jump if ax was totally 0
  
  ; == AL is now the needed number
  call @@print_num

  @@_pop_next:

    pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
    call @@print_num
    loop @@_pop_next
  ; ======================
  
  jmp @@end

  @@pad_zeros:
    push ax bx cx dx
    mov bx, 4
    sub bx, cx

    cmp ax, 0
    je @@pad

    dec bx

    @@pad:
      cmp bx, 0
      je @@end_pad

      mov al, 0
      call @@print_num
      sub bx, 1
      jmp @@pad
    @@end_pad:
    pop dx cx bx ax
    ret


  @@print_num:  ; num to print in al
    push ax bx cx dx
    mov bx, offset ns_sign
    inc bx

    xor ah, ah
    mov cl, 9
    mul cl
    add bx, ax

    mov cx, si
    mov dx, di

    add si, 7
    
    push bx cx dx
    call asseteasy
    pop dx cx bx ax

    ret
  
  @@end:
  mov [byte NOPAD], 0
  popp
  pop bp
  ret 6
endp PrintNumber


