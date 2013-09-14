; 16 bits, starting at 0x7C00.
BITS 16
ORG 0x7C00

BSS             EQU 0x504     ; The byte at 0x500 is also used, so align on next dword bound.
BSS_SIZE        EQU 438

CUR_TETRAMINO   EQU BSS       ; 16 bytes.
ROT_TETRAMINO   EQU BSS + 16  ; 16 bytes.
OFFSET          EQU BSS + 32  ; 2 bytes.
STACK           EQU BSS + 38  ; 4 bytes reserved in beginning, 400 bytes.

LEFT_SCANCODE   EQU 75
RIGHT_SCANCODE  EQU 77

UP_SCANCODE     EQU 72
DOWN_SCANCODE   EQU 80

PIT_TICS_WAIT   EQU 4

; TODO: main event loop, stack_join, scoring (if plausible).
;       README.md, release, ..., PROFIT!

CPU 686

; Entry point.
;     dl    -> the drive number.
;     cs:ip -> linear address 0x7C00.
start:
    jmp 0x0000:.CS_flush                    ; Some BIOS' may load us at 0x0000:0x7C00, while others at 0x07C0:0x0000.

    .CS_flush:
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

    ; Set to mode 0x03, or 80x25 text mode (ah is zero from above).
    mov al, 0x03
    int 0x10

    ; Hide the hardware cursor.               
    mov ch, 0x26
    mov al, 0x03                ; Some BIOS crash without this.
    inc ah
    int 0x10

    mov di, STACK
    mov eax, 0xDBDBDBDB

    .borders_init:
        mov dword [di - 3], eax
        mov word [di + 1], ax

        add di, 16
        cmp di, STACK + 400
        jbe .borders_init

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

    sti
    .event_loop:
        mov bx, [0x046C]
        add bx, 3

        .busy_loop:
            cmp [0x046C], bx
            jne .busy_loop

        ; If we don't need to load a new tetramino, yayy!
        test dl, dl
        jnz .input

        ; Loaded.
        inc dl

        ;xor bl, bl
        mov bl, 5
        ; REPLACE BY RAND.
        call tetramino_load

        mov word [si], 0x0006
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

            ; Join the tetramino, and join any complete lines.
            call stack_join

        .next_iter:             
            ; Display the stack.
            call stack_display

            call tetramino_display

            jmp .event_loop

; Load a tetramino to CUR_TETRAMINO, from the compressed bitmap format.
;     bl -> tetramino index.
tetramino_load:
    pusha

    ; Get the address of the tetramino (in bitmap format).
    xor bh, bh
    shl bl, 1

    ; Load tetramino bitmap in ax.
    mov bx, [bx + tetraminos]

    ; Convert from bitmap to array.
    mov di, CUR_TETRAMINO
    mov si, 0xDB
    mov cx, 0x10

    .loop:
        xor al, al

        shl bx, 1
        
        ; If the bit we just shifted off was set, store 0xDB.
        cmovc ax, si
        mov [di], al
        inc di

        loop .loop
    
    popa
    ret

; Displays CUR_TETRAMINO at current OFFSET.
tetramino_display:
    pusha
 
    ; Calculate first index into screen.
    mov al, [OFFSET + 1]
    mov cl, 80
    mul cl
 
    movzx di, byte [OFFSET]
    shl di, 1
 
    add di, 24
    add di, ax
 
    ; One character takes 2 bytes in video memory.
    shl di, 1
 
    ; Loops for 16 input characters.
    mov cl, 0x10
    mov si, CUR_TETRAMINO
 
    mov ah, 0x0F

    .loop:
        test cl, cl
        jz .ret
        
        dec cl

        lodsb
        test al, al
 
        ; Output two characters for "squarish" output.
        cmovz ax, [es:di]
        stosw
        stosw

        test cl, 0b11
        jnz .loop
 
        ; Since each tetramino input is 4x4, we must go to next line
        ; at every multiple of 4.
        ; Since we output 2 characters for one input char, cover offset of 8.
        add di, (80 - 8) * 2
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

    ; Reset ES.
    push ds 
    pop es

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
    rep ds movsw

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

        cmp di, STACK + 400
        jae .error

        .check_collision:
            cmp al, [di]
            jne .next_iter

        ; Colliding!
        stc
        jmp .ret

        .next_iter:
            inc di

            test cl, 0b11
            jnz .loop

            ; Go to next line in stack.
            add di, 16 - 4

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
;     Trashes bx.
stack_get_offset:
    ; Don't allow overflow.
    mov si, OFFSET

    ; Calculate first index into screen.
    xor ah, ah
    mov al, [si + 1]
    shl ax, 4

    movzx bx, byte [si]
    lea di, [STACK + bx]
    add di, ax

    ret

; Displays stack.
stack_display:
    pusha

    ; Add 24 characters padding in the front.
    mov di, 48
    mov si, STACK

    .loop:
        ; Copy 32 characters.
        mov cx, 16

        .line:
            lodsb

            ; Store one character as two -- to make stack "squarish" on 80x25 display.
            stosb
            inc di
            stosb
            inc di

            loop .line

        ; Handle remaining 24 characters in row, and starting 24 in next row.
        add di, 96
        cmp di, (25 * 160)          ; If we go beyond the last row, we're over.
        jb .loop

    popa
    ret

; Joins the current tetramino to the stack, and any complete lines together.
stack_join:
    pusha
    
    call stack_get_offset
    mov si, CUR_TETRAMINO
    mov cl, 0x10

    .loop:
        test cl, cl
        jz .ret
        
        dec cl

        lodsb
        or [di], al

        .next_iter:
            inc di

            test cx, 0b11
            jnz .loop

            ; Go to next line in stack.
            add di, 16 - 4

            jmp .loop
    
    .ret:
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

; Padding.
times 510 - ($ - $$)            db 0

BIOS_signature:
    dw 0xAA55

; Pad to floppy disk.
times (1440 * 1024) - ($ - $$)  db 0
