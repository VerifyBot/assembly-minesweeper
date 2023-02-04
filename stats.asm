DATASEG
  STATSPAGE  db "statsf.bmp", 0
  statspsize dw 320, 200

  ; stats file
  STATSFN db "stats.txt",0
  statsfh dw ? 

  ; top users file
  tfn db "top.txt",0
  tfh dw ?
  writeBuffer db ?,?,":",NAME_MAX_LENGTH dup(?),10,13
  LINE_LENGTH = $ - writeBuffer
  readBuffer db LINE_LENGTH dup(?)
  
  ; for sorting
  MAX_NAMES equ 50
  values_length dw ?
  old_values dw MAX_NAMES dup(?)
  values dw MAX_NAMES dup(?)
  
  ; symbol, =, value word, newline
  st_writeLineBuffer db ?,"=",?,?,10,13
  STATS_LINE_LENGTH = $ - st_writeLineBuffer
  st_readLineBuffer db STATS_LINE_LENGTH dup(?)


  TOP_COUNT equ 4
  ; stats vars
  ;blahblash db 5 dup(?)
  st_top db NAME_MAX_LENGTH*TOP_COUNT dup(?)
  top_mapping_count dw 0
  top_mapping dw MAX_NAMES dup(0)
  
  ; time value, :, name, newline
  topu_writeLineBuffer db ?,?,":",NAME_MAX_LENGTH dup(?),10,13
  TOPU_LINE_LENGTH = $ - topu_writeLineBuffer
  topu_readLineBuffer db TOPU_LINE_LENGTH dup(?)
  

  st_wins   dw ?
  st_loses  dw ?
  st_bestTime dw ?  ; seconds
  st_points dw ?


CODESEG

; ==============================
; Description: Stats page first-run drawer.
; Example:
;		call DrawStatsPage
; ==============================
proc DrawStatsPage
  call clearvideo

  assetBig STATSPAGE, 0, 0
  call GetStats   ; updates stats variables
  call DrawStats  ; uses stats variables

  ret
endp DrawStatsPage


; ======================================================
; Description: Stats page keypress handler.
; Input  : (By order) [1] KeyPressedInLSB.
; Example:
;		push KeyPressedInLSB
;		call StatsKeyPress
; ======================================================
proc StatsKeyPress
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
endp StatsKeyPress

; =============================================================
; Description: Stats page left click handler.
; Input  : (By order) [1] xCoord, [2] yCoord.
; Example:
;		push xCoord
;		push yCoord
;		call StatsLeftClick
; =============================================================
proc StatsLeftClick
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
endp StatsLeftClick


; =============================================================
; Description: Stats page right click handler.
; Input  : (By order) [1] xCoord, [2] yCoord.
; Example:
;		push xCoord
;		push yCoord
;		call StatsRightClick
; =============================================================
proc StatsRightClick
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
endp StatsRightClick


; =====================================================================
; Description: Get stats from stats files.
; Output : Updates stats variables like top1, top2, wins, loses, etc...
; Example:
;		call GetStats
; =====================================================================
proc GetStats
  ; open stats file
  push offset statsfn
  push offset statsfh
  call OpenRead  
  cmp [statsfh], -1
  jne @@start_reading
  
  print 'error reading stats :('
  jmp @@read_top_users


  @@start_reading:
  ; read stats from file 4 times (total 4 stat types)
  mov cx, 3
  @@read_stats:
    push cx

    mov ah, 03Fh
    mov bx, [statsfh]
    mov cx, STATS_LINE_LENGTH
    mov dx, offset st_readLineBuffer
    int 21h

    mov ax, [word st_readLineBuffer + 2]

    cmp [byte st_readLineBuffer], 'w'
    je @@set_wins
    cmp [byte st_readLineBuffer], 'l'
    je @@set_loses
    ;cmp [byte st_readLineBuffer], 'b'
    ;je @@set_bestTime
    cmp [byte st_readLineBuffer], 'p'
    je @@set_points

    @@set_wins:
    mov [st_wins], ax
    jmp @@next_stat
    @@set_loses:
    mov [st_loses], ax
    jmp @@next_stat
    ;@@set_bestTime:
    ;mov [st_bestTime], ax
    ;jmp @@next_stat
    @@set_points:
    mov [st_points], ax
    jmp @@next_stat

    @@next_stat:
    pop cx
    loop @@read_stats


  ; close stats file
  push [statsfh]
  call CloseFile

  @@read_top_users:  ; todo file might contain less than 4 users

  ; load top file
  openr tfn tfh
  mov [values_length], 0
  xor bx, bx
  read_values:
    push bx
    ; read from file (not using macro because we need length returned)
    mov ah, 03Fh
    mov bx, [tfh]
    mov cx, LINE_LENGTH
    mov dx, offset readBuffer
    int 21h
    pop bx

    cmp ax, LINE_LENGTH  ; check length returned (for EOF)
    jl done_reading_values

    mov ax, [word readBuffer]  ; get value

    push bx
    shl bx, 1  ; fit to word
    mov [old_values + bx], ax
    mov [values + bx], ax
    inc [values_length]
    pop bx

    inc bx
    cmp bx, MAX_NAMES
    jge done_reading_values
    jmp read_values

  done_reading_values:  ; todo fuck:
  closef [tfh]

  ; after loading values we should sort the array
  mov [top_mapping_count], 0
  push offset values
  push [values_length]
  call SortArray
  
  
  ; get top 4 *NAMES* from values  

  ; load top file
  openr tfn tfh
  ; read needed values
  xor bx, bx  ; counter
  xor dx, dx
  @@read_names:

    ; get top player from the end (since the array is sorted min->max)
    push si 
    mov si, [top_mapping_count]
    dec si
    sub si, bx
    shl si, 1  ; xxx word index

    mov dx, [top_mapping + si]
    mov ax, LINE_LENGTH
    mul dx
    seekf [tfh], 0, ax
    pop si

    ; read from file (not using macro because we need length returned)
    push bx
    mov ah, 03Fh
    mov bx, [tfh]
    mov cx, LINE_LENGTH
    mov dx, offset readBuffer
    int 21h
    pop bx

    cmp ax, LINE_LENGTH  ; check length returned (for EOF)
    jge @@get_name
    jmp @@done_reading_names
  
    @@get_name:
    push bx
    shl bx, 1
    mov cx, [values + bx]
    mov ax, [word readBuffer]
    pop bx

    cld
    push es
    mov ax, ds
    mov es, ax
    mov si, offset readBuffer + 3  ; name buffer stars from here

    mov ax, NAME_MAX_LENGTH
    mul bx

    mov di, offset st_top
    add di, ax
    mov cx, NAME_MAX_LENGTH
    rep movsb
    pop es

    inc bx
    cmp bx, TOP_COUNT
    jge @@done_reading_names
    jmp @@read_names

  @@done_reading_names:
  closef [tfh]





  @@end:
  ret
endp GetStats

proc DrawStats
  ;mov [word st_wins], 150
  ;mov [word st_loses], 250
  ;mov [word st_bestTime], 993
  ;mov [word st_points], 34122
  
  mov [byte NOPAD], 1
  push [st_wins] 188 117
  call PrintNumber
  
  mov [byte NOPAD], 1
  push [st_loses] 196 134
  call PrintNumber

  ; ?! best time is retrieved when sorting the values
  mov ax, [values]
  mov [st_bestTime], ax
  mov [byte NOPAD], 1
  push [st_bestTime] 222 151
  call PrintNumber

  mov [byte NOPAD], 1
  push [st_points] 204 168
  call PrintNumber

  ; V DONE
  ; leaderboard names writing plan
  ; - either figuring out how to
  ;   print with custom bgcolors
  ;   because it doesnt work for
  ;   some reason
  ; - or just print with black bg
  ;   and replace all black with
  ;   bg color

  
  ; top 1
  xor ch, ch  ;top 4
  @@iter_top:
    xor cl, cl
    @@iter_place: 
      ; get char index
      mov al, ch
      mov bl, NAME_MAX_LENGTH
      mul bl
      add al, cl
      mov bl, al
      xor bh, bh

      mov al, [st_top + bx]
     
      cmp al, -1  ; if not available
      je @@break_place
      cmp al, '$' ; stop at dollar
      je @@break_place

      ; get x location based on cl
      mov dl, cl
      add dl, 5
      ; get y location based on ch
      mov dh, ch
      shl dh, 1
      add dh, 6

      ; write char
      push cx
      ; set cur pos
      mov ah, 02h
      int 10h
      ; write char with clr
      mov ah, 09h
      mov bl, 0ffh
      mov cx, 1
      int 10h
      pop cx

      inc cl
      cmp cl, NAME_MAX_LENGTH
      jl @@iter_place

    @@break_place:
    inc ch
    cmp ch, TOP_COUNT
    jl @@iter_top 

    
    mov dx, 46
    @@y:
      mov cx, 33
      @@x:
        mov ah, 0Dh
        int 10h

        cmp al, 0  ; if black
        jne @@next

        ; => replace
        mov al, 055h
        mov ah, 0Ch
        int 10h

        @@nexT:
        inc cx
        cmp cx, 90
        jl @@x

      inc dx
      cmp dx, 107
      jl @@y



      

 
  ret
endp DrawStats


;--
; finds the minimum value in array, result is index in bx
proc FindMin
  push bp
  mov bp, sp
  
  push ax cx dx si di  ; save registers

  ; ------------------------------
  ; Stack State:
  ; |    bp + 4   |    bp + 6    |
  ; | arrayLength | offset array |
  ; ------------------------------
  
  mov si, [bp + 6]  ; holds the array offset
  mov dx, [si] ; min value = first array value
  mov ax, 0  ; min value index = 0 (first item)
  
  mov di, 1  ; curr index
  @@iter_arr:
    cmp di, [bp+4]  ; until end of array
    je @@end

    mov bx, di
    shl bx, 1  ; xxx word index
    mov cx, [si + bx]  ; value we are going to compare to min

    cmp cx, dx
    jl @@set_min
    jmp @@next
  
    @@set_min:
    mov dx, cx  ; set new min value
    mov ax, di  ; set new min index
    
    @@next:
    inc di
    jmp @@iter_arr

  
  
  @@end:
  mov bx, ax
  pop di si dx cx ax  ; recover registers

  pop bp
  ret 4
endp FindMin

; =====================================================
; Descrpt: Swaps two variables.
; Input  : (By order) [1] offset var2, [2] offset var1.
; Output : Swapped variables in dseg.
; =====================================================
proc Swap
  push bp
  mov bp, sp
  
  push ax bx cx si ; save registers

  ; -----------------------------
  ; Stack State:
  ; |    bp + 4   |    bp + 6   | 
  ; | offset var2 | offset var1 |
  ; -----------------------------
  mov si, [bp + 6]  ; var1 offset
  mov bx, [bp + 4]  ; var2 offset
  
  mov ax, [si] ; var1 value 
  mov cx, [bx] ; var2 value 

  mov [si], cx
  mov [bx], ax

  pop si cx bx ax ; recover registers

  pop bp
  ret 4
endp Swap

; ======================================================
; Descrpt: Sorts an array.
; Input  : (By order) [1] arrayLength, [2] offset array.
; Output : Given array is now sorted
; ======================================================
proc SortArray
  push bp
  mov bp, sp

  ; ------------------------------
  ; Stack State:
  ; |    bp + 4   |    bp + 6    |
  ; | arrayLength | offset array |
  ; ------------------------------

  ; feel map array with its own index
  xor bx, bx
  @@fill_map:
    mov si, bx
    shl si, 1
    mov [top_mapping + si], bx
    inc bx
    cmp bx, MAX_NAMES
    jl @@fill_map

  
  mov di, 0  ; array index
  @@iter_array:
    cmp di, [bp+4]
    jxe @@end
    push di
    
    ; we only want the part of the array after what we already sorted so
    mov bx, di
    shl bx, 1  ; xxx word index
    mov si, [bp+6]
    add si, bx

    ; so part length aswell
    mov cx, [bp+4]
    sub cx, di

    push si  ; part array offset
    push cx  ; part array length
    call FindMin
    
    add bx, di  ; add missing index offset
    
     
    @@do_swap:  ; swap between sorting array values
    shl bx, 1  ; xxx word index
    shl di, 1  ; xxx word index

    push bx di
    add bx, [bp+6]  ; add array offset
    add di, [bp+6]  ; add array offset
    push bx
    push di
    call Swap
    pop di bx

    ; swap between t indexes array for names matching
    add bx, offset top_mapping
    add di, offset top_mapping
    push bx
    push di
    call Swap
    inc [top_mapping_count] 

    @@next:
    pop di
    inc di
    jmp @@iter_array

  @@end:
  pop bp
  ret 4
endp SortArray
