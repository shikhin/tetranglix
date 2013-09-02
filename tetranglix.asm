; 16 bits, starting at 0x7C00.
BITS 16
ORG 0x7C00

BSS             EQU 0x504     ; The byte at 0x500 is also used, so align on next dword bound.
BSS_SIZE        EQU 290

CUR_TETRAMINO   EQU BSS       ; 16 bytes.
ROT_TETRAMINO   EQU BSS + 16  ; 16 bytes.
OFFSET          EQU BSS + 32  ; 2 bytes.
STACK           EQU BSS + 34  ; 250 bytes.
COUNTER         EQU BSS + 284 ; 2 bytes.
BIOS_PIT_HANDLER EQU BSS + 286 ; 4 bytes.

LEFT_SCANCODE   EQU 75
RIGHT_SCANCODE  EQU 77

UP_SCANCODE     EQU 72
DOWN_SCANCODE   EQU 80

; TODO: main event loop, stack_join, scoring (if plausible).
;       README.md, release, ..., PROFIT!

CPU 386

; Entry point.
;     dl    -> the drive number.
;     cs:ip -> linear address 0x7C00.
start:
    jmp 0x0000:.flush_CS                    ; Some BIOS' may load us at 0x0000:0x7C00, while others at 0x07C0:0x0000.

    .flush_CS:
        ; Set up segments.
        xor bx, bx

        ; Stack.
        mov ss, bx
        mov sp, start
    
        mov ds, bx
        mov es, bx

    ; Clear direction flag.
    cld

    ; Clear BSS
    mov di, BSS
    mov cx, BSS_SIZE
    xor ax, ax
    rep stosb

    ; Save the current BIOS PIT handler's address.
    mov si, 0x8 * 4
    mov di, BIOS_PIT_HANDLER
    movsd

    ; Hook up to PIT, to fire pit_handler instead of the BIOS handler.
    cli
    mov word [si - 4], pit_handler
    mov [si - 2], bx            ; Zero, from above.
    sti

    ; Set to mode 0x03, or 80x25 text mode (ah is zero from above).
    mov al, 0x03
    int 0x10

    ; Hide the hardware cursor.               
    mov ch, 0x26
    mov al, 0x03                ; Some BIOS crash without this.
    inc ah
    int 0x10

    mov ax, 0xB800
    mov es, ax

    ; White spaces on black background.
    xor di, di
    mov cx, 80*25
    mov ax, 0x0F00
    rep stosw

    ; Cleared dx implies "load new tetramino".
    xor dl, dl
    mov si, OFFSET

    .event_loop:
        cmp byte [si + (COUNTER - OFFSET)], 0
        jne .event_loop

        ; If we don't need to load a new tetramino, yayy!
        test dl, dl
        jnz .input

        ; Loaded.
        inc dl

        xor bl, bl
        call tetramino_load

        mov word [si], 0x0003
        jmp .next_iter

        ; Check for input.
        .input:
            ; Check for keystroke.
            mov ah, 0x01
            int 0x16

            ; If no keystroke, increment vertical offset.
            jz .vertical_increment

            ; Clear the keyboard buffer.
            xor ah, ah
            int 0x16

            ; Save current x co-ordinate.
            mov bx, [si]

        ; Go left.
        .left:
            cmp ah, LEFT_SCANCODE
            jne .right

            dec byte [si]
            call tetramino_collision_check
            jc .restore

        ; Go right.
        .right:
            cmp ah, RIGHT_SCANCODE
            jne .rotate

            inc byte [si]
            call tetramino_collision_check
            jc .restore

        ; Rotate it.
        .rotate:
            cmp ah, UP_SCANCODE
            jne .vertical_increment

            call tetramino_rotate
            call tetramino_collision_check
            jnc .vertical_increment

            ; To restore, just rotate 3 more times.
            call tetramino_collision_check
            call tetramino_collision_check
            call tetramino_collision_check           

        .restore:
            mov [si], bx
        
        .vertical_increment:
            ; Check if we can go below one byte, successfully.
            inc byte [si + 1]
            call tetramino_collision_check

            jnc .next_iter

            ; If we can't, we need a new tetramino.
            dec byte [si + 1]
            xor dl, dl

        .next_iter:             
            ; Display the stack.
            call stack_display

            call tetramino_display

            ; Wait for four ticks, per "frame". 
            ; No science, feels good to play with this.
            add byte [si + (COUNTER - OFFSET)], 4
            jmp .event_loop

; Handles the PIT interrupt, also calling the BIOS' handler.
; Output:
;     Decrements COUNTER, unless 0.
pit_handler:
    pusha

    ; If the lock was already set, return.
    xor cx, cx
    xchg cl, [pit_lock]
    jcxz .ret

    mov cx, [COUNTER]
    jcxz .unlock
    dec word [COUNTER]

    ; Unlock the handler.
    .unlock:
        mov byte [pit_lock], 1

    .ret:
        popa
        ; Call the BIOS handler.
        jmp far [BIOS_PIT_HANDLER]

; Load a tetramino to CUR_TETRAMINO, from the compressed bitmap format.
;     bl -> tetramino index.
tetramino_load:
    pusha

    ; Get the address of the tetramino (in bitmap format).
    xor bh, bh
    shl bl, 1
    add bx, tetraminos

    ; Load tetramino bitmap in ax.
    mov ax, [bx]

    ; Convert from bitmap to array.
    mov dx, 0x8000
    mov bx, CUR_TETRAMINO

    .loop:
        test ax, dx
        jz .zero
    
            mov byte [bx], 0xDB     ; Full block.
            jmp .next_iter
    
        .zero:
            mov byte [bx], 0x00     ; ' '
    
        .next_iter:
            inc bx

            shr dx, 1
            jnz .loop
    
    popa
    ret

; Displays CUR_TETRAMINO at current OFFSET.
tetramino_display:
    pusha

    ; Calculate first index into screen.
    mov al, [OFFSET + 1]
    mov cl, 80
    mul cl

    movzx bx, byte [OFFSET]
    shl bx, 1

    add bx, 30
    add bx, ax

    ; One character takes 2 bytes in video memory.
    shl bx, 1

    ; Loops for 16 input characters.
    mov cl, 0x10
    mov si, CUR_TETRAMINO

    .loop:
        test cl, cl
        jz .ret
        
        dec cl

        mov dl, [si]
        test dl, dl
        je .next_iter

        ; Output two characters for "squarish" output.
        mov [es:bx], dl
        mov [es:bx + 2], dl

        .next_iter:
            inc si
            add bx, 4

            test cl, 0b11
            jnz .loop

            ; Since each tetramino input is 4x4, we must go to next line
            ; at every multiple of 4.
            ; Since we output 2 characters for one input char, cover offset of 8.
            add bx, (80 - 8) * 2
            jmp .loop

    .ret:
        popa
        ret

; Rotates CUR_TETRAMINO 90 degrees clock-wise.
; Output:
;     CUR_TETRAMINO -> rotated tetramino.
tetramino_rotate:
    pusha
    push es

    ; Reset ES for movs*.
    xor ax, ax
    mov es, ax

    mov si, CUR_TETRAMINO
    mov cx, 4

    .loop:
        ; The vertical line from ROT_TETRAMINO is (cx - 1).
        mov di, ROT_TETRAMINO - 1
        add di, cx

        mov dl, 4

        .line:
            movsb

            ; For one vertical line from ROT_TETRAMINO, get to next horizontal line.
            add di, 3

            dec dl
            jnz .line

        loop .loop

    mov si, ROT_TETRAMINO
    mov di, CUR_TETRAMINO
    mov cl, 4*4/2       ; CH would be zero, from above.
    rep movsw

    pop es
    popa
    ret

; Detects if CUR_TETRAMINO at OFFSET is colliding with any thing.
; Output:
;     Carry set if colliding.
tetramino_collision_check:
    pusha

    ; Clear carry.
    clc

    ; Get offset.
    call stack_get_offset
    mov bx, ax
    add bx, STACK + 10

    ; Loops for 16 input characters.
    mov cl, 0x10
    mov si, CUR_TETRAMINO

    .loop:
        test cl, cl
        jz .ret
        
        dec cl

        lodsb
        cmp al, 0xDB
        jne .next_iter

        cmp di, bx
        jae .error

        cmp di, STACK + 250
        jae .error

        .check_collision:
            cmp al, [di]
            jnz .next_iter

        ; Colliding!
        stc
        jmp .ret

        .next_iter:
            inc di

            test cl, 0b11
            jnz .loop

            ; Go to next line in stack.
            add di, 10 - 4
            add bx, 10

            jmp .loop

    .error:
        stc

    .ret:
        popa
        ret

; Gets the offset into stack (i.e., address) into di.
; Output:
;     si -> points at OFFSET.
;     di -> address into stack.
;     ax -> offset just according to the y-coordinate.
;     Trashes bx, cl.
stack_get_offset:
    ; Don't allow overflow.
    mov si, OFFSET

    ; Calculate first index into screen.
    mov al, [si + 1]
    mov cl, 10
    mul cl

    movzx bx, byte [si]
    lea di, [STACK + bx]
    add di, ax

    ret

; Displays stack.
stack_display:
    pusha

    ; Add 30 characters padding in the front.
    mov di, 60
    mov si, STACK

    .loop:
        ; Frame characters: one character before 30, one character after 50.
        mov byte [es:di - 2], 0xBA
        mov byte [es:di + 40], 0xBA

        ; Copy 20 characters.
        mov cx, 10

        .line:
            lodsb

            ; Store one character as two -- to make stack "squarish" on 80x25 display.
            stosb
            inc di
            stosb
            inc di

            loop .line

        ; Handle remaining 30 characters in row, and starting 30 in next row.
        add di, 120
        cmp di, (25 * 160)          ; If we go beyond the last row, we're over.
        jb .loop

    popa
    ret

; All tetraminos in bitmap format.
tetraminos:
    dw 0b0000111100000000   ; I
    dw 0b0000111000100000   ; J
    dw 0b0000001011100000   ; L
    dw 0b0000011001100000   ; O
    dw 0b0000001101100000   ; S
    dw 0b0000111001000000   ; T
    dw 0b0000011000110000   ; Z

; Lock over PIT handling code -- not re-entrant.
pit_lock: 
    db 1

; Padding.
times 510 - ($ - $$)            db 0

BIOS_signature:
    dw 0xAA55

; Pad to floppy disk.
times (1440 * 1024) - ($ - $$)  db 0
