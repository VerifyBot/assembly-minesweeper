DATASEG
  ; [BUTTONS]
  BTNS_MAIN_PAGE_WIDTH  equ 94
  BTNS_MAIN_PAGE_HEIGHT equ 28

  BTNS_START_X equ ((320 - BTNS_MAIN_PAGE_WIDTH) /  2)
  BTNS_START_Y equ ((200 - BTNS_MAIN_PAGE_HEIGHT) / 2)-10

  ; <start main page assets>  prefix: mpa_
  mpa_MAINPAGE  db "mainf.bmp", 0
  mpa_mainpsize dw 320, 200

  mpa_btn_width  equ 101
  mpa_btn_height equ 29
  
  ; start x, start y

  mpa_start_x equ 109
  mpa_start_y equ 71

  mpa_help_x equ 109
  mpa_help_y equ 106

  mpa_stats_x equ 109
  mpa_stats_y equ 141

  
  helps  db "help.bmp", 0,94, 28
  exits  db "exit.bmp", 0,94, 28
  starts db "start.bmp",0,94, 28

CODESEG
; === MAIN PAGE PROCEDURES === ;
proc DrawMainPage
  call clearvideo  ; clear screen

  assetBig mpa_MAINPAGE, 0, 0
  ;asset STARTS BTNS_START_X, BTNS_START_Y
  ;asset HELPS BTNS_START_X, BTNS_START_Y+40
  ;asset EXITS BTNS_START_X, BTNS_START_Y+80

  ret
endp DrawMainPage

proc MainRightClick
  push bp
  mov bp, sp
  pushh
 
  ; ================ ;
  ; SS:              ;
  ; [bp+4]: y coord  ;
  ; [bp+6]: x coord  ;
  ; ================ ;
  mov cx, [bp+6]
  mov dx, [bp+4]

  ; nothing yet.
  
  @@end:
  popp
  pop bp
  ret 4
endp MainRightClick


proc MainLeftClick
  push bp
  mov bp, sp
  pushh
  
  ; ================ ;
  ; SS:              ;
  ; [bp+4]: y coord  ;
  ; [bp+6]: x coord  ;
  ; ================ ;
  mov cx, [bp+6]
  mov dx, [bp+4]

  ; main page buttons
  btnrange cx, dx, MPA_START_X, MPA_BTN_WIDTH, MPA_START_Y, MPA_BTN_HEIGHT
  je @@start_button
 
  btnrange cx, dx, MPA_HELP_X, MPA_BTN_WIDTH, MPA_HELP_Y, MPA_BTN_HEIGHT
  je @@help_button

  btnrange cx, dx, MPA_STATS_X, MPA_BTN_WIDTH, MPA_STATS_Y, MPA_BTN_HEIGHT
  je @@stats_button

  jmp @@end

  @@start_button:
  ; start!
  mov [current_page], 1
  jmp @@end

  @@help_button:
  ; help!
  mov [current_page], 3
  jmp @@end

  @@stats_button:
  ; stats!
  mov [current_page], 2
  jmp @@end

  @@end:
  popp
  pop bp
  ret 4
endp MainLeftClick


proc MainKeyPress
  push bp
  mov bp, sp
  pushh

  ; =====================  ;
  ; SS:                    ;
  ; [bp+4]: key pressed    ;
  ;         in lower byte  ;
  ; ====================== ;
  mov ax, [bp+4]
 
  ; nothing yet...
  cmp al, 'q'
  je @@exit_game

  jmp @@end
  

  @@exit_game:
  mov [current_page], -1
  jmp @@end




  @@end:
  popp
  pop bp
  ret 2
endp MainKeyPress


