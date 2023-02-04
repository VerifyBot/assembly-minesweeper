DATASEG
  HELPPAGE  db "helpf.bmp", 0
  helppsize dw 320, 200

CODESEG

; ==============================
; Description: Help page first-run drawer.
; Example:
;		call DrawHelpPage
; ==============================
proc DrawHelpPage
  call clearvideo

  assetBig HELPPAGE, 0, 0

  ret
endp DrawHelpPage


; ======================================================
; Description: Help page keypress handler.
; Input  : (By order) [1] KeyPressedInLSB.
; Example:
;		push KeyPressedInLSB
;		call HelpKeyPress
; ======================================================
proc HelpKeyPress
  push bp
  mov bp, sp
  pushh

 	; -------------------
	; Stack State:
	; |     [bp + 4]    |
	; | KeyPressedInLSB |
	; -------------------
  mov ax, [bp+4]

  cmp al, 'q'
  je @@exit_sign

  jmp @@end

  @@exit_sign:
  mov [current_page], 0
  jmp @@end

  
  @@end:
  popp
  pop bp
  ret 2
endp HelpKeyPress

; =============================================================
; Description: Help page left click handler.
; Input  : (By order) [1] xCoord, [2] yCoord.
; Example:
;		push xCoord
;		push yCoord
;		call HelpLeftClick
; =============================================================
proc HelpLeftClick
  push bp
  mov bp, sp
  pushh

	; -----------------------
	; Stack State:
	; | [bp + 4] | [bp + 6] |
	; |  yCoord  |  xCoord  |
	; -----------------------
  mov cx, [bp+6]
  mov dx, [bp+4]



  popp
  pop bp
  ret 4
endp HelpLeftClick


; =============================================================
; Description: Help page right click handler.
; Input  : (By order) [1] xCoord, [2] yCoord.
; Example:
;		push xCoord
;		push yCoord
;		call HelpRightClick
; =============================================================
proc HelpRightClick
  push bp
  mov bp, sp
  pushh

	; -----------------------
	; Stack State:
	; | [bp + 4] | [bp + 6] |
	; |  yCoord  |  xCoord  |
	; -----------------------
  mov cx, [bp+6]
  mov dx, [bp+4]



  popp
  pop bp
  ret 4
endp HelpRightClick

