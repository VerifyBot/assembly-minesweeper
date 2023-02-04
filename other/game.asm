macro Center c
  ((320 - c) / 2)
endm

DATASEG
  CELL_SIZE equ 8
  GRID_SIZE equ 20  ; 20x20

  GRID_START_X equ ((320 - CELL_SIZE*GRID_SIZE) / 2)
  GRID_START_Y equ ((200 - CELL_SIZE*GRID_SIZE) / 2) + 15

  GRID_BOMB  equ 11
  GRID_CELL equ 0
  GRID_AIR  equ 10
  GRID_FLAG equ 12

  ; every byte in the grid should represent cell type:
  ; 0 (cell) 10 (air) 1-8 (nums) 11 (bomb)
  grid db (GRID_SIZE*GRID_SIZE) dup(10)     ; grid with real data
  grid_ui db (GRID_SIZE*GRID_SIZE) dup(0)  ; grid with revealed data and flags


  BOMBS_COUNT equ 35;45;45;45;45;35;45;45;45;5  ; 45 is good
  added_bombs dw 0
  bombs_buff dw BOMBS_COUNT dup(?)  ; needed to make sure the bombs are in unique locations

  flags_count dw 0

  blood db "blood.bmp",0,15,15

  loser db "loser.bmp",0,120,50
  LOSER_WIDTH  equ ((320 - 120) / 2)
  LOSER_HEIGHT equ ((200 - 50) / 2)

  winner db "winner.bmp",0,138,74
  WINNER_WIDTH  equ ((320 - 138) / 2)
  WINNER_HEIGHT equ ((200 - 74) / 2)

  ; game page assets
  GAMEPAGE  db "gamef.bmp", 0
  gamepsize dw 320, 200
  flagclock db "flgclc.bmp", 0, 18, 42

  
  stopwatch dw 0
  clock_timer dw ?
  stop_timer db 0

  game_over db 0

  ; useful to know which buttons to detect
  gameover_mode db 0  ; 1-LOSE, 2-WIN

  showNums dw ?
  showed db ?

  ; keep track of how many cells are popped
  popped dw 0
  
  ; save score with name
  NAME_MAX_LENGTH equ 5
  nameLength db ?
  nameBuffer dw NAME_MAX_LENGTH dup(?)
  
  scoresFileHandle dw ?
  scoresFileName db "top.txt", 0
  
  readNamesBuffer db 2+1+NAME_MAX_LENGTH+2 dup(?)
  isDup db 0

  statsFileHandle dw ?
  statsFileName db "stats.txt", 0

  points dw 0
  doubleDot db ":"
  newlineBytes db 10,13 

  readStatsBuffer db 6*3 dup(?)
  oldPoints dw ?
  oldWins   dw ?
  oldLoses  dw ?
  
  
  holding_reveal_key db 0  ; disable spam click
  revealPressed db 0

  holding_hint_key db 0
  hintPressed db 0

CODESEG
; === GAME PAGE PROCEDURES ===
proc GamePageLoop
  call CheckWin  ; check if you win in the game page

  ret
endp GamePageLoop


proc DrawGamePage
  call clearvideo  ; clear screen
  call ResetGameData  ; stopwatch, flags count, grid, grid_ui

  assetBig GAMEPAGE, 0, 0
  asset flagclock, 1, 81 

  mov [byte stop_timer], 0 
  mov [revealPressed], 0
  mov [hintPressed], 0
  ;push 10
  ;call UpdateClock

  push offset grid_ui
  call DrawGrid    ; draw the grid before the information is set
  call CreateMine  ; mine algorithm generation

  ;asset STARTS BTNS_START_X, BTNS_START_Y
  ;asset HELPS BTNS_START_X, BTNS_START_Y+40
  ;asset EXITS BTNS_START_X, BTNS_START_Y+80

  ret
endp DrawGamePage

; description: update flags count visualization on the screen
proc UpdateFlagsCount
  push ax

  cmp [byte shown_page], 1
  jne @@end
  

  mov ax, BOMBS_COUNT
  sub ax, [flags_count]
  
  cmp ax, 0
  jl @@end

  push ax
  push 22 106  ; position, on right to flag
  call PrintNumber

  
  @@end:
  pop ax
  ret
endp UpdateFlagsCount


; description: update clock visualization
proc UpdateClock
  ; check if timer runs
  cmp [stop_timer], 0
  je @@update
  jmp @@end
  
  @@update:
  cmp [byte shown_page], 1
  jne @@end
 
  cmp [word stopwatch], 9999+1
  je @@end
  
  push [word stopwatch]
  push 22 85  ; position, on right to clock
  call PrintNumber

  inc [word stopwatch]
  
  @@end:
  ret
endp UpdateClock

; description: reset game data for a new game
proc ResetGameData
  push bx

  mov [stopwatch], 0
  mov [flags_count], 0
  mov [word popped], 0 
  mov [game_over], 0
  mov [gameover_mode], 0

  ; reset grid
  xor bx, bx
  @@iter_grid:
    cmp bx, GRID_SIZE*GRID_SIZE
    je @@end

    ;mov [byte grid], 10
    mov [byte grid_ui + bx], 0

    inc bx
    jmp @@iter_grid

  @@end:
  pop bx
  ret
endp ResetGameData

; index to X,Y in CX,DX
macro index_to_xy idx
  push ax bx
  ; index to x,y =  y: idx//GRID_SIZE
  ;                 x: idx%GRID_SIZE
  
  xor dx, dx
  mov ax, idx
  mov bx, GRID_SIZE
  div bx  ; % in DX, // in AX

  mov cx, dx  ; %
  mov dx, ax  ; //

  pop bx ax
endm

; index to X*8,Y*8 in CX,DX
macro index_to_xy_with_shl idx
  push ax bx
  
  mov ax, idx
  xor dx, dx
  mov bx, GRID_SIZE
  div bx  ; % in DX, // in AX

  mov cx, dx  ; %
  mov dx, ax  ; //

  shl cx, 3
  shl dx, 3

  pop bx ax
endm



; X,Y to index in DI (DIV BY 8 FIRST)
macro xy_to_index_shr_first x, y
  push ax bx cx dx
  
  ; x,y to index = y*GRID_SIZE+x
  mov ax, y
  shr ax, 3
  mov bx, GRID_SIZE
  mul bx
  mov cx, x
  shr cx, 3
  add ax, cx
  mov di, ax

  pop dx cx bx ax
endm


; X,Y to index in DI
macro xy_to_index x, y
  push ax bx dx
  
  ; x,y to index = y*GRID_SIZE+x
  mov ax, y
  mov bx, GRID_SIZE
  mul bx
  add ax, x
  mov di, ax

  pop dx bx ax
endm


; description: runs every time a user right clicks
proc GameRightClick
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

  cmp [byte game_over], 0
  je @@grid_handle

  ; game over = 1, no rightclick handlers needed
  jmp @@end

  @@grid_handle:  ; GRID HANDLERS
  ; reset relative grid pos (set to 0,0)
  ; also check if in range (check CF to see if x/y goes to minus)
  sub cx, GRID_START_X
  jc @@invalid  ; in x range (negative)
  sub dx, GRID_START_Y
  jc @@invalid  ; in y range (negative)
  cmp cx, CELL_SIZE*GRID_SIZE  ; in x range (positive)
  jge @@invalid
  cmp dx, CELL_SIZE*GRID_SIZE  ; in y range (positive)
  jge @@invalid

  jmp @@valid
  
  @@invalid:
  jmp @@end

  
  @@valid:
  ; same algorithm as in LeftClick, read desc from there
  shr cx, 3
  shr dx, 3 
  xy_to_index cx, dx
  shl cx, 3
  shl dx, 3
  
  push di cx dx
  call PutFlag

  @@end:
  popp
  pop bp
  ret 4
endp GameRightClick

; description: runs every time a user left clicks
proc GameLeftClick
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

  cmp [game_over], 0
  jne @@gameover_btns_check
  jmp @@grid_handler
  
  @@gameover_btns_check:
  cmp [byte gameover_mode], 1  ; lose mode
  je @@lose_btns

  cmp [byte gameover_mode], 2  ; win mode
  jxe @@win_btns  ; jump equal extended

  jmp @@end

  @@lose_btns:
  inrange LOSER_WIDTH+17, cx, LOSER_WIDTH+17+38
  jne @@lose_quit_range_check
  inrange LOSER_HEIGHT+30, dx, LOSER_HEIGHT+30+15
  jne @@lose_quit_range_check
  
  ; in play again range
  call RestartGame
  jmp @@end

  @@lose_quit_range_check:
  inrange LOSER_WIDTH+65, cx, LOSER_WIDTH+65+38
  jne @@done_lose_btns
  inrange LOSER_HEIGHT+30, dx, LOSER_HEIGHT+30+15
  jne @@done_lose_btns

  ; in quit range
  mov [byte current_page], 0
  mov [byte game_over], 0

  @@done_lose_btns:
  jmp @@end

  
  @@win_btns:

  ; set name range check
  inrange WINNER_WIDTH+70, cx, WINNER_WIDTH+70+41
  jne @@after_win_range_check
  inrange WINNER_HEIGHT+57 , dx, WINNER_HEIGHT+57+11
  jne @@after_win_range_check
  
  ; set name
  call SetName
  jmp @@end
  
  @@after_win_range_check:
  jmp @@end

  @@grid_handler:
  ; GRID HANDLERS
  ; reset relative grid pos (set to 0,0)
  ; also check if in range (check CF to see if x/y goes to minus)
  sub cx, GRID_START_X
  jc @@invalid  ; in x range (negative)
  sub dx, GRID_START_Y
  jc @@invalid  ; in y range (negative)
  cmp cx, CELL_SIZE*GRID_SIZE  ; in x range (positive)
  jge @@invalid
  cmp dx, CELL_SIZE*GRID_SIZE  ; in y range (positive)
  jge @@invalid

  jmp @@valid
  
  @@invalid:
  jmp @@end

  
  @@valid:
  ; goal: get top left (x,y) of clicked cell
  ; problem: x=12 should be interperted as first cell (8, [<--12] 16, 24)
  ; method: divide by 8 (ignore remainder!) and then multiply by 8 to get fixed (12/8 = 1, * 8 = 8)


  ;printvv "[$,$]", cx, dx

  ; divide by 8 (cell size)
  shr cx, 3
  shr dx, 3 
  
  ; (x,y) to grid index in di
  xy_to_index cx, dx

  ; and then multiply by 8 to get fixed 
  shl cx, 3
  shl dx, 3
  
  ; we only care about clicking on grid cell cells
  cmp [grid_ui+di], GRID_CELL
  je @@not_revealed
  jmp @@end
  
  ; cell is not already revealed, pop it
  @@not_revealed:

  ; we want to make sure that we dont accidentally pop a flag
  ; so check in the grid_ui that we are not clicking on a flag
  mov ah, [grid_ui + di]
  cmp ah, GRID_FLAG
  jne @@call_reveal
  jmp @@after_reveal

  @@call_reveal:
  ; if its air, we want to flood fill it
  mov al, [byte grid + di]
  cmp al, GRID_AIR
  jne @@not_air
  
  playSound 1193180/100000, 1

  ; its air
  push di cx dx
  call FloodFill
  jmp @@after_reveal

  @@not_air:
  push di cx dx
  call PopCell
  jmp @@after_reveal


  @@after_reveal:

  @@end:
  popp
  pop bp
  ret 4
endp GameLeftClick

; description: run every time a user presses a key on the keyboard
proc GameKeyPress
  ; key in al
  push bp
  mov bp, sp
  pushh

  ; =====================  ;
  ; SS:                    ;
  ; [bp+4]: key pressed    ;
  ;         in lower byte  ;
  ;         and bios scan  ;
  ;         in upper byte  ;
  ; ====================== ;
  mov ax, [bp+4]


  cmp al, 'q'
  je @@exit_sign

  cmp al, 'h'
  je @@flag_hint
  mov [holding_hint_key], 0

  cmp ah, 80
  je @@reveal_secret
  mov [holding_reveal_key], 0

  jmp @@end

  @@exit_sign:
  mov [current_page], 0
  jmp @@end
  
  @@reveal_secret:
  cmp [holding_reveal_key], 1
  je @@end
  mov [holding_reveal_key], 1
  xor [revealPressed], 0001b
  call RevealSecretHandler
  jmp @@end

  @@flag_hint:
  cmp [holding_hint_key], 1
  je @@end
  mov [holding_hint_key], 1
  xor [hintPressed], 1
  call RevealHintFlag
  jmp @@end


  @@end:
  popp
  pop bp
  ret 2
endp GameKeyPress


; restart game drawings
proc RestartGame
  mov [game_over], 0
  call DrawGamePage

  ret
endp RestartGame

; description: toggle easter egg to show real grid on the screen  - triggers when pressing the down key.
proc RevealSecretHandler
  cmp [revealPressed], 1
  je @@do_reveal

  ; hide reveal
  ; UI = grid_ui
  ; open numbers 
  push offset grid_ui
  call DrawGrid

  jmp @@end

  @@do_reveal:
  ; UI = grid
   ; open numbers 
  push offset grid
  call DrawGrid 

  @@end:
  ret
endp RevealSecretHandler


; description: put a flag on a bomb (easteregg)
proc RevealHintFlag

  ; put flag on random bomb location
  ; * make sure there isnt a flag there already!! ! :)
  cmp [flags_count], BOMBS_COUNT
  je @@end  ; already found all

  @@find_good_bomb:
    ; get random bomb
    randomWord 0, BOMBS_COUNT  ; random value in ax 
    ; mul by 2 for word access
    mov bx, ax
    shl bx, 1
    mov si, [bombs_buff + bx]
    cmp [grid_ui + si], GRID_FLAG
    je @@find_good_bomb   ; already flag there :(
    
  index_to_xy si
  shl cx, 3
  shl dx, 3

  push si cx dx
  call PutFlag

  xor bx, bx
  @@iter_bombs:
    cmp bx, BOMBS_COUNT
    jxe @@flag_win
    
    ; get bombs locations indexes
    push bx
    shl bx, 1
    mov di, [word bombs_buff + bx]
    pop bx
    ;index_to_xy_with_shl di
    
    ; check if there is a flag on the bombs
    mov al, [grid_ui + di]
    cmp al, GRID_FLAG
    jne @@end  ; no flag on a bomb, :(

    inc bx
    jmp @@iter_bombs
  
  @@flag_win:

  @@end:
  ret
endp RevealHintFlag

; ------------------------------------------------------
; draws the grid on the screen based on values in the  ;
; given grid variable                                  ;
; ------------------------------------------------------
proc DrawGrid
  push bp
  mov bp, sp
  pushh

  ; SS:
  ; [bp+4]: grid var

  
  xor dx, dx
  xor di, di
  @@new_row:
    push dx
    mov cx, 0
    @@draw_cell:
      push [bp+4] di cx dx
      call DrawCell
      inc di

      add cx, CELL_SIZE
      cmp cx, CELL_SIZE*GRID_SIZE
      jl @@draw_cell
    
    pop dx
    add dx, CELL_SIZE
    cmp dx, CELL_SIZE*GRID_SIZE
    jl @@new_row

  popp
  pop bp
  ret 2
endp DrawGrid


; put a flag on a cell if there are any left
proc PutFlag
  push bp
  mov bp, sp
  pushh
  
  ; ================== ;
  ; SS:                ;
  ; [bp+4]: y coord    ;
  ; [bp+6]: x coord    ;
  ; [bp+8]: grid index ;
  ; ================== ;
  mov di, [bp+8]
  mov cx, [bp+6]
  mov dx, [bp+4]
  
  ; - did we reveal this position?
  ;  YES:
  ;     - do nothing
  ;  NO:
  ;     - is there a flag?
  ;       YES:
  ;                                                - remove the flag from the flags array
  ;          - remove the flag from grid_ui
  ;          - restore cell asset
  ;       NO:
  ;                                                - add the flag to the flags array
  ;          - put flag in the grid_ui
  ;          - put flag drawing
  

  mov al, [grid_ui + di]

  cmp al, GRID_CELL   ; did we reveal this position?
  jne @@maybe_its_flag
  jmp @@put_flag

  @@maybe_its_flag:
  cmp al, GRID_FLAG
  jne @@already_revealed
  jmp @@remove_flag
  
  @@already_revealed:
  jmp @@end  ; yes, do nothing

  ; flag switching
  @@put_flag:  ; is there a flag? NO
  playSound 1193180/30000, 2


  mov ax, BOMBS_COUNT
  sub ax, [flags_count]
  cmp ax, 0
  je @@end

  mov [grid_ui + di], GRID_FLAG
  push offset grid_ui
  push di cx dx
  call DrawCell
  inc [flags_count]
  call UpdateFlagsCount

  jmp @@end
  
  @@remove_flag:  ; is there a flag? YES 
  mov [grid_ui + di], GRID_CELL
  push offset grid_ui
  push di cx dx
  call DrawCell
  dec [flags_count]
  call UpdateFlagsCount
  
  


  @@end:
  popp
  pop bp
  ret 6
endp PutFlag


; open a cell
proc PopCell
  push bp
  mov bp, sp
  pushh
  
  ; ================== ;
  ; SS:                ;
  ; [bp+4]: y coord    ;
  ; [bp+6]: x coord    ;
  ; [bp+8]: grid index ;
  ; ================== ;
  mov di, [bp+8]
  mov cx, [bp+6]
  mov dx, [bp+4]
  
  mov si, offset grid  ; we are reading from the real grid. we will also update the grid_ui variable

  ; get cell type in that location
  mov al, [grid + di]
   
  @@do_pop:
  ; set the ui grid cell to that type
  mov [grid_ui + di], al


  cmp al, GRID_AIR
  jne @@maybe_bomb
  jmp @@air

 
  @@maybe_bomb:
  cmp al, GRID_BOMB
  jne @@maybe_number
  jmp @@bomb
  
  @@maybe_number:
  inrange 1, al, 8
  jne @@maybe_warn
  jmp @@number

  ; if al=GRID_CELL then it means the grid isn't initialized
  ; and that shouldn't happen
  @@maybe_warn:
  cmp al, 0
  jne @@idk
  jmp @@warn
  
  @@idk:
  jmp @@end


  @@warn:
  print '!!! GRID ISNT INITIALIZED, WHY IS THIS PROCEDURE BEING CALLED? !!!'
  waitkey
  jmp @@end

  @@air:
  playSound 1193180/1000, 2

  inc [word popped]
  push si di cx dx
  call DrawCell
  jmp @@end

  @@number:
  playSound 1193180/20, 2

  inc [word popped]
  push si di cx dx
  call DrawCell
  jmp @@end
  
  @@bomb:  ; clicked on bomb
  playSound 1193180/400, 5


  push si di cx dx
  call DrawCell
  call Lose
  jmp @@end


  @@end:
  popp
  pop bp
  ret 6
endp PopCell





; ------------------------------------------------------------- ;
; Desc: This procedure generates the mine with correct values   ;
; ------------------------------------------------------------- ;
proc CreateMine 

  ; empty cells (for debug before making the bombs)
  xor bx,bx
  @@iter_:
    mov [byte grid + bx], GRID_AIR
    inc bx
    cmp bx, GRID_SIZE*GRID_SIZE
    jl @@iter_



  ; [STEP #1: GENERATE BOMBS IN UNIQUE RANDOM LOCATION]
  mov [added_bombs], 0
  @@random_bomb:
    cmp [added_bombs], BOMBS_COUNT
    jne @@do_random_bomb
    jmp @@after_bombs
    
    @@do_random_bomb:
    randomWord 0, ((GRID_SIZE*GRID_SIZE)-1)  ; random value in AX

    ; check if the number exists in the array already
    ;  - if it does jmp to the random_bomb label again
    xor bx, bx
    @@check_bomb_exists:
      cmp bx, [added_bombs]
      je @@after_bomb_check
      
      push bx
      shl bx, 1  ; access word index
      mov cx, [word bombs_buff + bx]  
      pop bx
      
      ; overlapping. try again
      cmp cx, ax
      jne @@no_overlap
      jmp @@random_bomb
      
      @@no_overlap:
      inc bx
      jmp @@check_bomb_exists

    @@after_bomb_check:

    ; add number to array
    push bx
    mov bx, [added_bombs]
    shl bx, 1
    mov [word bombs_buff + bx], ax
    pop bx
    
    mov bx, ax

    
    mov [byte grid + bx], GRID_BOMB
   ; call drawgrid
    
    ; run until needed bombs value    
    inc [added_bombs]
    jmp @@random_bomb


  @@after_bombs:
  ; [STEP #2: algorithm]

  ; iter on each cell, count bombs around it and put correct number
  xor bx,bx
  @@iter:
    mov al, [byte grid + bx]
    cmp al, GRID_BOMB  ; if block == 'X':
    je @@next          ;   continue
    
    push bx
    call ScanAround  ; return number of bombs around index

    cmp al, 0  ; if no bombs around, put air
    jne @@not_air
    mov al, GRID_AIR

    @@not_air:
    mov [byte grid + bx], al

    @@next:
    inc bx
    cmp bx, GRID_SIZE*GRID_SIZE
    jl @@iter
 

  @@end: 
  ret
endp CreateMine





; checks if there is a bomb at (x,y), if there is then al++
macro add_bomb_count x, y
  local endmac
  push cx dx

  mov cx, x
  mov dx, y
  xy_to_index cx, dx  ; index in di

  ; 1. is it in grid???
  inrange 0, cx, GRID_SIZE-1
  jne endmac
  inrange 0, dx, GRID_SIZE-1
  jne endmac

  ; 2. is it a bomb???
  cmp [byte grid + di], 11
  jne endmac

  inc al

  endmac:
  pop dx cx
endm


; this procedure returns the number of bombs around a cell in al
; the cell is an index and it is given as an argument
proc ScanAround
  push bp
  mov bp, sp
  push bx cx dx

  ; SS:
  ; [bp+4]: index of cell
  mov bx, [bp+4]

  index_to_xy bx  ; cx=x dx=y
  
  xor al, al  ; bombs counter
  
  after_mac:
  ; 8 scans (max 8 bombs)  !! xxx check borders xxx !!
  ; [x] TOP LEFT (x-1, y-1)
  push cx dx 
  dec cx
  dec dx
  add_bomb_count cx, dx
  pop dx cx

  ; [x] TOP MID (x, y-1)
  push cx dx 
  dec dx
  add_bomb_count cx, dx
  pop dx cx

  ; [x] TOP RIGHT (x+1, y-1)
  push cx dx 
  inc cx
  dec dx
  add_bomb_count cx, dx
  pop dx cx

  ; [x] MID LEFT (x-1, y)
  push cx dx 
  dec cx
  add_bomb_count cx, dx
  pop dx cx

  ; [x] MID RIGHT (x+1, y)
  push cx dx 
  inc cx
  add_bomb_count cx, dx
  pop dx cx


  ; [x] BOT LEFT (x-1, y+1)
  push cx dx 
  dec cx
  inc dx
  add_bomb_count cx, dx
  pop dx cx


  ; [x] BOT MID (x, y+1)
  push cx dx 
  inc dx
  add_bomb_count cx, dx
  pop dx cx


  ; [x] BOT RIGHT (x+1, y+1)
  push cx dx
  inc cx
  inc dx
  add_bomb_count cx, dx
  pop dx cx
 
  
  pop dx cx bx
  pop bp
  ret 2
endp ScanAround


; ---------------------------------------------------------------------------------------- ;
; Desc: This procedure draws a cell on the screen from given arguments                     ;
; Args: [1] index of cell type from grid array [2] x coord top left [3] y coord top left   ;
; ---------------------------------------------------------------------------------------- ;
proc DrawCell
  push bp
  mov bp, sp
  pushh

  ; SS:
  ; [bp+4]: y
  ; [bp+6]: x
  ; [bp+8]: type
  ; [bp+10]: grid variable

  mov cx, [bp+6]
  mov dx, [bp+4]

  add cx, GRID_START_X
  add dx, GRID_START_Y

  mov di, [bp+8]
  mov si, [bp+10]
  add si, di
  mov al, [si]

  cmp al, 0
  jne @@maybe_air
  jmp @@cell
  
  @@maybe_air:
  cmp al, 10
  jne @@maybe_bomb
  jmp @@air
  
  @@maybe_bomb:
  cmp al, 11
  jne @@maybe_flag
  jmp @@bomb
  
  @@maybe_flag:
  cmp al, 12
  jne @@idk
  jmp @@flag

  @@idk:
  jmp @@nums

  @@cell: 
  mov ax, 1
  asset cell, cx, dx  ; draw image
  jmp @@end

  @@air:
  asset air, cx, dx
  jmp @@end

  @@bomb:
  asset bomb, cx, dx
  jmp @@end
 
  @@flag:
  asset flag, cx, dx
  jmp @@end

  @@nums:  ; 1-9
  mov bx, offset numbers_sign
  inc bx
  xor ah, ah
  shl al, 3
  add bx, ax
  sub bx, 8 

  push bx cx dx
  call asseteasy
  
  
  @@end:
  popp
  pop bp
  ret 8
endp DrawCell






; ==================================================================
; Description: Utility procedure for floodfill. given x movement (up/down/none) and y
;							 movement (up/down/none) the procedure does bound checks and reveals
;							 the cells accordingly.
; Input  : (By order) [1] x movement, [2] y movement.
; Output : Pops a cell according to the input locations.
; Example:
;		push x movement
;		push y movement
;		call CheckAndPop
; ==================================================================
proc CheckAndPop
  push bp
  mov bp, sp
  push cx dx di
  
	; ---------------------------
	; Stack State:
	; |  [bp + 4]  |  [bp + 6]  |
	; | y movement | x movement |
	; ---------------------------

  
  ; X BOUNDS JUMPS CONTROLLER
  cmp [word bp+6], -1
  je @@x_up_check
  cmp [word bp+6], 1
  je @@x_down_check
  jmp @@after_x_check


  @@x_up_check:  ; (x: -1)
  cmp cx, CELL_SIZE
  jl @@end

  sub cx, CELL_SIZE
  jmp @@after_x_check


  @@x_down_check: ; (x: 1)
  cmp cx, (CELL_SIZE*GRID_SIZE)-CELL_SIZE
  jge @@end

  add cx, CELL_SIZE
  jmp @@after_x_check

  @@after_x_check:


  ; Y BOUNDS JUMPS CONTROLLER
  cmp [word bp+4], -1
  je @@y_up_check
  cmp [word bp+4], 1
  je @@y_down_check
  jmp @@after_y_check


  @@y_up_check: ; (y: -1)
  cmp dx, CELL_SIZE
  jl @@end

  sub dx, CELL_SIZE
  jmp @@after_y_check

  @@y_down_check: ; (y: 1)
  cmp dx, (CELL_SIZE*GRID_SIZE)-CELL_sIZE
  jge @@end 
  add dx, CELL_SIZE
  jmp @@after_y_check

  @@after_y_check:


  ; passed range checks
  xy_to_index_shr_first cx, dx
  push di cx dx
  call FloodFill

  @@end:
  pop di dx cx
  pop bp
  ret 4
endp CheckAndPop

; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; desc: uses FLOOD FILL algorithm to open all air cells when clicking on air
; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
proc FloodFill
  push bp
  mov bp, sp
  pushh

  ; ================== ;
  ; SS:                ;
  ; [bp+4]: y coord    ;
  ; [bp+6]: x coord    ;
  ; [bp+8]: grid index ;
  ; ================== ;
  mov di, [bp+8]
  mov cx, [bp+6]
  mov dx, [bp+4]
 
  ; if the cell was already revealed, exit
  mov al, [grid_ui + di]
  cmp al, GRID_CELL
  je @@not_revealed_yet
  jmp @@end

  @@not_revealed_yet:
  mov al, [grid + di]  ; get cell type
  
  ; is it air?
  cmp al, GRID_AIR
  je @@pop_air
  jmp @@not_air ; pop nums near this

  @@pop_air:
  ; set the ui grid cell to air
  inc [word popped]
  mov [grid_ui + di], GRID_AIR
  mov si, offset grid
  push si di cx dx
  call DrawCell


  ; [UL] [UU] [UR]
  ; [LL] [MM] [RR]
  ; [DL] [DD] [DR]


  ; UL
  push -1
  push -1
  call CheckAndPop
  
  ; DL
  push -1
  push 1
  call CheckAndPop
  
  ; UR
  push 1
  push -1
  call CheckAndPop
    
  ; DR
  push 1
  push 1
  call CheckAndPop

  ; LL
  push -1
  push 0
  call CheckAndPop

  ; RR
  push 1
  push 0
  call CheckAndPop
  
  ; UU
  push 0
  push -1
  call CheckAndPop

  ; DD
  push 0
  push 1
  call CheckAndPop

 
  jmp @@end 

  @@not_air:
  inc [word popped]
  mov al, [grid + di]
  mov [grid_ui + di], al
  mov si, offset grid
  push si di cx dx
  call DrawCell
 

  @@end:
  popp
  pop bp
  ret 6
endp FloodFill


; handle losing, gets triggered when pressing on a bomb
proc Lose
  pushh
  
  ; stop timer
  mov [stop_timer], 1  

  ; game over
  mov [game_over], 1

  ; open bombs
  xor bx, bx
  @@iter_bombs:
    cmp bx, [added_bombs]
    jne @@do_iter
    jmp @@after_open_bombs
    
    @@do_iter:
    push bx
    shl bx, 1
    mov di, [word bombs_buff + bx]
    index_to_xy_with_shl di
    
    mov [grid_ui + di], GRID_BOMB
    mov si, offset grid
    push si di cx dx
    call DrawCell
    pop bx
    
    inc bx
    jmp @@iter_bombs
  
  @@after_open_bombs:
  ; open numbers 
  push offset grid
  call DrawGrid
  

  ; show sus
  cmp [byte AMONGUS_ON_GAMEOVER], 1
  je @@do_sus

  ; no sus
  assetTrans blood, 0, 200-15-1

  jmp @@after_sus

  @@do_sus:
  xor bx, bx
  @@sus:
    randomWord 0, (320-15-1)
    mov cx, ax
    randomWord 0, (200-15-1) 
    mov dx, ax
    assetTrans blood, cx, dx 

    inc bx
    cmp bx, 30
    jge @@after_sus
    jmp @@sus

  @@after_sus:
  ; you lost message
  assetTrans loser, LOSER_WIDTH, LOSER_HEIGHT


  ; update file (++loses)
  ; loses count is on the second line at index 2-3 (word): 6+2

  ; first get loses count

  ; 3 stats types, 6 chars in each line
  openr statsFileName, statsFileHandle  ; open read
  
  readf [statsFileHandle], 6*3, readStatsBuffer ; read contents
  mov ax, [word readStatsBuffer + 8]  ; 6+2
  mov [oldLoses], ax
  
  closef [statsFileHandle]  ; close file

  ; now write new value
  openw statsFileName, statsFileHandle  ; open write
  seekf [statsFileHandle], 0, 6+2  ; seek from start, at index 2
  inc [oldLoses]
  writef [statsFileHandle], 2, oldLoses  ; write to file
  closef [statsFileHandle]  ; close file


  mov [gameover_mode], 1

  @@end:
  popp
  ret  
endp Lose

; check if the player won
proc CheckWin
  cmp [byte game_over], 0
  je @@check_ingame
  jmp @@end
  

  @@check_ingame:
  ; check if in game page
  cmp [byte shown_page], 1
  je @@check
  jmp @@end
  
  @@check:
  ; check if total-popped = bombs count
  mov ax, GRID_SIZE*GRID_SIZE
  sub ax, [popped]
  cmp ax, BOMBS_COUNT
  jne @@check_flags

  call Win
  jmp @@end

  @@check_flags:

  ; used flags count == bombs count? if yes check win, if not exit
  mov ax, [flags_count]
  cmp ax, BOMBS_COUNT
  je @@start_iter
  jmp @@end
  
  @@start_iter:
  ; yes, so now check if there is a flag in grid_ui where there is bomb in grid

  xor bx, bx
  @@iter_bombs:
    cmp bx, BOMBS_COUNT
    jxe @@flag_win
    
    ; get bombs locations indexes
    push bx
    shl bx, 1
    mov di, [word bombs_buff + bx]
    pop bx
    ;index_to_xy_with_shl di
    
    ; check if there is a flag on the bombs
    mov al, [grid_ui + di]
    cmp al, GRID_FLAG
    jne @@end  ; no flag on a bomb, :(

    inc bx
    jmp @@iter_bombs
  
   @@flag_win:
   call Win

  @@end:
  ret
endp CheckWin

; triggers every win
proc Win
  ; stop timer
  mov [stop_timer], 1  

  ; game over
  mov [game_over], 1

  assetTrans winner, WINNER_WIDTH, WINNER_HEIGHT
  

  ; calculate score by time took to win

  mov ax, 300
  sub ax, [stopwatch]
  
  jnc @@set_score

  mov ax, 0  ; if it took more than 300s, make sure no minus score
  jmp @@set_score

  @@set_score:
  add ax, BOMBS_COUNT   ; more bombs -> more score
  shr ax, 1
 

  ;debug: mov ax, 150
  mov [points], ax

  push ax
  push WINNER_WIDTH+76 WINNER_HEIGHT+36  ; position, on right to flag
  call PrintNumber
 
  ; update file (++win, +=points)
  ; wins count is on first line at index 2-3 (word): 2
  ; loses count is on the second line at index 2-3 (word): 6+2
  ; points count is on the third line at index 2-3 (word): 6+6+2

  ; first get wins count and points count

  ; 3 stats types, 6 chars in each line
  openr statsFileName, statsFileHandle  ; open read
  
  readf [statsFileHandle], 6*3, readStatsBuffer ; read contents
  mov ax, [word readStatsBuffer + 2]
  mov [oldWins], ax
  mov ax, [word readStatsBuffer + 14]
  mov [oldPoints], ax
  
  closef [statsFileHandle]  ; close file

  ; now write new values
  
  openw statsFileName, statsFileHandle  ; open write
  seekf [statsFileHandle], 0, 2  ; seek from start, at index 2
  inc [oldWins]
  writef [statsFileHandle], 2, oldWins  ; write to file
  seekf [statsFileHandle], 0, 6+6+2  ; seek from start, at index 2 on line 3
  mov ax, [points]
  add [oldPoints], ax
  writef [statsFileHandle], 2, oldPoints
  closef [statsFileHandle]  ; close file


  mov [gameover_mode], 2


  ret
endp Win

; set name for points
proc SetName
  pushh

  ; bg
  mov ax, 2  ; hide cur
  int 33h

  mov dx, WINNER_HEIGHT+56
  @@y:
    mov cx, WINNER_WIDTH+66
    @@x:
      mov al, BLACK
      mov ah, 0Ch
      int 10h
      inc cx
      cmp cx, WINNER_WIDTH+110
      jl @@x

    inc dx
    cmp dx, WINNER_HEIGHT+67
    jl @@y



  ;call clearvideo
    
  mov ax, [word points]

  ; set cur pos
  mov ah, 2
  mov dh, 15
  mov dl, 20
  int 10h

  push NAME_MAX_LENGTH
  push offset nameBuffer
  push offset nameLength
  call KeyboardInput
 
  println 'updating leaderboard...'

  call AddScore
  
  print '...done!'

  ; go to main screen
  mov [byte current_page], 0
  mov [byte game_over], 0 

  ; put back cur
  mov ax, 1
  int 33h


  popp
  ret
endp SetName

; description: adds user score and name to the top users files.
proc AddScore
  pushh

 
  ; pad name with dollars so its length = NAME_MAX_LENGTH
  xor dx, dx
  mov dl, [nameLength]  ; 1 byte

  mov bx, NAME_MAX_LENGTH
  @@padRightName:
    cmp bx, dx
    jxe @@afterPad
    mov [byte nameBuffer + bx - 1], "$"

    dec bx
    jmp @@padRightName

  @@afterPad:


  ; check if dup
  mov [isDup], 0  ;default to 0

  openr scoresFileName, scoresFileHandle
    
  xor bx, bx  ; line counter
  @@read_names:
    push bx

    ; read file (we dont use macro because we need result)
    mov ah, 03Fh
    mov bx, [scoresFileHandle]
    mov cx, 2+1+NAME_MAX_LENGTH+2
    mov dx, offset readNamesBuffer
    int 21h 

    ; EOF
    cmp ax, cx
    jl @@after_read_names 

    ; compare strings
    cld
    mov ax, ds
    mov es, ax
    mov si, offset readNamesBuffer+3  ; compare name in file
    mov di, offset nameBuffer         ; to saving name
    mov cx, NAME_MAX_LENGTH
    repe cmpsb
    jnz @@next_name
      

    ; name dup here!
    mov [isDup], 1
    jmp @@after_read_names

    @@next_name:
    pop bx
    add bx, 2+1+NAME_MAX_LENGTH+2
    jmp @@read_names

  @@after_read_names:
  pop bx
  closef [scoresFileHandle]

  openw scoresFileName, scoresFileHandle

  cmp [isDup], 1
  jne @@seek_end

  ; seek special
  seekf [scoresFileHandle], 0, bx ; seek to dup position
  jmp @@write_name

  @@seek_end:
  seekf [scoresFileHandle], 2, 0  ; seek to end offset 0

  @@write_name:
  ; now write data in this format:
  ; VALUE_BYTE_1,VALUE_BYTE_2,":",NAME,10,13
  ;            points          :  NAME NEWLINE

  writef [scoresFileHandle], 2, points ; points
  writef [scoresFileHandle], 1, doubleDot ; ":"

  ; NAME


  writef [scoresFileHandle], NAME_MAX_LENGTH, nameBuffer  ; name
  writef [scoresFileHandle], 2, newlineBytes  ; newline

  closef [scoresFileHandle]

  @@end:
  popp
  ret
endp AddScore


