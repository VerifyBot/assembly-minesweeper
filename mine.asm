; ===================================
;  = 1X21  Minesweeper Game  1X1    =
; =  12X1  in assembly 8086  1221  =
;  =  2331    By Nir Yona 111 1X1   =
; =   1XX1       2022     1X1 111  =
; ===================================

IDEAL


; NOW TODO NOW

; fixme LIST OF BUGS fixme
; ...

; xxx TODO xxx
; * when hover on button change image



; <?> possible features <?>
; * (OPTIONAL P) option to change bombs count on main screen
; * (OPTIONAL P) a gamemode where you can see where the bombs are for a few seconds at the beginning of the game
; * (OPTIONAL LP) when losing dont show real grid, first put among asses and then if the player clicks on all among asses in 5s then they can play again
; * (OPTIONAL LP) instead of static clock image make a "shaon hol" (hebrew) and the sands fill slowly

; /V DONE /V
; -V- mine numbers generation algorithm
; -V- putting flags
; -V- stopwatch counter
; -V- screens switching
; -V- air flood algorithm and show numis near (thanks LIAM for sitting with me and helping me debug instead of sleeping <3)
; -V- losing function (clicking a bomb)
; -V- winning function (popped cells = all cells - bombs count OR flag on every bomb)
; -V- stats page to display wins,loses,points,best time/best points,top names
; -V- sounds for putting a flag, popping a cell
; -V- a tool where you can press DOWN ARROW KEY to toggle between grid and grid_ui

MODEL small
STACK 256

macro playSound vnote, length
push vnote length
call PlaySoundProc
endm

DATASEG  
  ; As a general assumption, 0 = FALSE and 1 = TRUE


  ; Disable spam right/left clicks detection; only detect clicks if PUSHED->RELEASED->PUSHED AGAIN
  holding_left_click  db 0
  holding_right_click db 0
  
  ; Game settings
  AMONGUS_ON_GAMEOVER db 1  ; (JOKE) turning this on spams amongasses when you lose

  ; PAGES
  ; -1-EXIT 0-MAIN 1-GAME 2-STATS 3-HELP
  current_page db 0    ; change this in code, this will be the page you want to switch to
  shown_page   db -1   ; **DONT CHANGE THIS** this will be the page you currently see and it updates based on current_page
 

  stop_music dw -1
  start_music_time dw ?
CODESEG
include "tools.asm"   ; general tools (display, logic, many more...)
include "bmpUtil.asm" ; tools for drawing assets on the screen
include "random.asm"  ; tools for generating random numbers

include "game.asm"    ; game page procedures, constants and variables
include "stats.asm"   ; stats page procedures, constants and variables
include "main.asm"    ; main page procedures, constants and variables
include "help.asm"    ; help page procedures, constants and variables


proc PlaySoundProc
  push bp
  mov bp, sp
  pushh

  ; SS:
  ; [bp+4]: length
  ; [bp+6]: note

  mov ax, [bp+4]
  mov [stop_music], ax
  mov ah, 0
  int 1Ah
  mov [start_music_time], dx

  ; open speaker
  in al, 61h
  or al, 00000011b
  out 61h, al
  ; send control word to change frequency
  mov al, 0B6h
  out 43h, al
  ; play frequency 131Hz
  mov ax, [bp+6]
  out 42h, al ; Sending lower byte
  mov al, ah
  out 42h, al ; Sending upper byte
  

  popp
  pop bp
  ret 4
endp PlaySoundProc



start:
  mov ax, @data
  mov ds, ax

  call videomode  ; switch to video mode
  call clearvideo ; clear screen

  ; timer implementation from tools.asm for "async" clock in game page
  mov [stop_timer], 0
  resetTimer clock_timer

  ; LOAD MAIN PAGE
  mov [current_page], 0

  ; GAME LOOP
  game_loop:
    checkTimer clock_timer, 18, UpdateClock  ; 18 ticks is ~~ 1 second
    
    call MusicManager

    call PageLoop

    call ShowPage  ; really calls the display page procedures when a page changes

    ; === Keyboard ===
    getkey  ; get current pressed key CF=1 if pressed else CF=0
    jnc no_key

    push ax  ; key in al
    call KeyPress  ; this procedure will call the right keypress procedure based on the page you are in

    cmp [current_page], -1
    je EXIT

    jmp mouse_checks

    ; === Mouse ===
    no_key:
    mov [holding_reveal_key], 0
    mov [holding_hint_key], 0

    mouse_checks:
    cursor  ; get cursor data

    ; check left click
    cmp bx, 1
    je left

    mov [holding_left_click], 0  ; release
    
    ; check right click
    cmp bx, 2
    je right
    
    mov [holding_right_click], 0  ; release
    
    jmp after_clicks

    ; mouse left click procedure
    left:
    push cx dx
    call LeftClick
    jmp after_clicks
    
    right:
    push cx dx
    call RightClick

    after_clicks:


    ; === Game Loop ===
    jmp game_loop

  

EXIT:
  call clearvideo  ; clear screen
  call textmode    ; back to text mode
  mov ax, 4C00h ; returns control to dos
  int 21h


; procedure to manage playing sounds
proc PageLoop
  pushh
 
  mov dl, [shown_page]
  
  ;cmp dl, 0
  ;je @@main
  
  cmp dl, 1
  je @@game

  ;cmp dl, 2
  ;je @@stats

  ;cmp dl, 3
  ;je @@help

  jmp @@end

  @@game:
  call GamePageLoop
  jmp @@end


  @@end:
  popp

  ret
endp PageLoop

proc MusicManager
  cmp [stop_music], -1
  je @@end

  mov ah, 0
  int 1Ah
  sub dx, [start_music_time]
  cmp dx, [stop_music]
  jl @@end 

  mov [stop_music], -1

  ; stop music
  ; close the speaker
  in al, 61h
  and al, 11111100b
  out 61h, al

  @@end:
  ret
endp

; === GLOBAL EVENTS === ;
proc KeyPress
  push bp
  mov bp, sp
  pushh

  ; =====================  ;
  ; SS:                    ;
  ; [bp+4]: key pressed    ;
  ;         in lower byte  ;
  ; ====================== ;
  mov ax, [bp+4]
  
  mov dl, [shown_page]
  
  cmp dl, 0
  je @@main_press
  
  cmp dl, 1
  je @@game_press

  cmp dl, 2
  je @@stats_press

  cmp dl, 3
  je @@help_press

  jmp @@end

  @@main_press:
  push ax
  call MainKeyPress
  jmp @@end

  @@game_press:
  push ax
  call GameKeyPress
  jmp @@end

  @@stats_press:
  push ax
  call StatsKeyPress
  jmp @@end

  @@help_press:
  push ax
  call HelpKeyPress
  jmp @@end


  @@end:
  popp
  pop bp
  ret 4
endp KeyPress


proc LeftClick
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
 
  ; avoid spamming left click
  cmp [holding_left_click], 1
  jne @@left_click
  jmp @@end

  @@left_click:
  mov [holding_left_click], 1

  mov al, [shown_page]
 
  cmp al, 0
  je @@main_left
  
  cmp al, 1
  je @@game_left

  cmp al, 2
  je @@stats_left

  cmp al, 3
  je @@help_left

  jmp @@end


  @@main_left:
  push cx dx
  call MainLeftClick
  jmp @@end

  @@game_left:
  push cx dx
  call GameLeftClick
  jmp @@end

  @@stats_left:
  push cx dx
  call StatsLeftClick
  jmp @@end

  @@help_left:
  push cx dx
  call HelpLeftClick
  jmp @@end


  @@end:
  popp
  pop bp
  ret 4
endp LeftClick


proc RightClick
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
 
  cmp [holding_right_click], 1
  jne @@right_click
  jmp @@end

  @@right_click:
  mov [holding_right_click], 1

  
  mov al, [shown_page]
 
  cmp al, 0
  je @@main_right
  
  cmp al, 1
  je @@game_right

  cmp al, 2
  je @@stats_right

  cmp al, 3
  je @@help_right

  jmp @@end


  @@main_right:
  push cx dx
  call MainRightClick
  jmp @@end

  @@game_right:
  push cx dx
  call GameRightClick
  jmp @@end

  @@stats_right:
  push cx dx
  call StatsRightClick
  jmp @@end

  @@help_right:
  push cx dx
  call HelpRightClick
  jmp @@end


  
  @@end:
  popp
  pop bp
  ret 4
endp RightClick


; shows current page based on current_page variable
proc ShowPage
  mov al, [current_page]

  ; already in needed page
  cmp al, [shown_page]
  je @@end

  mov [shown_page], al

  cmp al, 0
  je @@show_main

  cmp al, 1
  je @@show_game

  cmp al, 2
  je @@show_stats

  cmp al, 3
  je @@show_help

  jmp @@end

  @@show_main:
  call DrawMainPage
  jmp @@end

  @@show_game:
  call DrawGamePage
  jmp @@end

  @@show_stats:
  call DrawStatsPage
  jmp @@end

  @@show_help:
  call DrawHelpPage
  jmp @@end

  @@end:
  ret
endp ShowPage




END start
