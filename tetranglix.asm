; 16 bits, starting at 0x7C00.
BITS 16
ORG 0x7C00

BSS             EQU 0x504     ; The byte at 0x500 is also used, so align on next dword bound.
BSS_SIZE        EQU 284       ; 16 for CUR_TETRAMINO, 16 for ROT_TETRAMINO, 2 for (x, y) into field.
                              ; 250 for stack.

CUR_TETRAMINO   EQU BSS       ; 16 bytes.
ROT_TETRAMINO   EQU BSS + 16  ; 16 bytes.
OFFSET          EQU BSS + 32  ; 2 bytes.
STACK           EQU BSS + 34  ; 250 bytes.

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
    mov ax, BSS
    mov di, ax
    mov cx, BSS_SIZE
    xor ax, ax
    rep stosb
    
    ; Set to mode 0x03, or 80x25 text mode.
    xor ah, ah
    mov al, 0x03
    int 0x10

    ; Hide the hardware cursor.               
    mov ch, 0x26
    inc ah
    int 0x10

    mov ax, 0xB800
    mov es, ax

    ; White spaces on black background.
    xor di, di
    mov cx, 80*25
    mov ax, 0x0F00
    rep stosw

    jmp $

; Load a tetramino to CUR_TETRAMINO, from the compressed bitmap format.
;     al -> tetramino index.
tetramino_load:
    pusha

    ; Get the address of the tetramino (in bitmap format).
    xor ah, ah
    shl al, 1
    add ax, tetraminos

    ; Load tetramino bitmap in ax.
    mov bx, ax
    mov ax, [bx]

    ; Convert from bitmap to array.
    mov dx, 0x8000
    mov bx, CUR_TETRAMINO

    .loop:
        test ax, dx
        jz .zero
    
            mov byte [bx], 0xDB     ; Full block.
            jmp .loop_end
    
        .zero:
            mov byte [bx], 0x20          ; ' '
    
    .loop_end:
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

        ; Output two characters for "squarish" output.
        mov [es:bx], dl
        mov [es:bx + 2], dl

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
        xor al, [di]
        jnz .next

        ; Colliding!
        stc
        jmp .ret

    .next:
        inc di

        test cl, 0b11
        jnz .loop

        ; Go to next line in stack.
        add di, 10 - 4
        jmp .loop

    .ret:
        popa
        ret

; Gets the offset into stack (i.e., address) into di.
; Output:
;     di -> address into stack.
;     Trashes ax, bx, cl.
stack_get_offset:
    ; Calculate first index into screen.
    mov al, [OFFSET + 1]
    mov cl, 10
    mul cl

    movzx bx, byte [OFFSET]
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

; Padding.
times 510 - ($ - $$)            db 0

BIOS_signature:
    dw 0xAA55

; Pad to floppy disk.
times (1440 * 1024) - ($ - $$)  db 0
