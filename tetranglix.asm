; 16 bits, starting at 0x7C00.
BITS 16
ORG 0x7C00

BSS             EQU 0x504     ; The byte at 0x500 is also used, so align on next dword bound.
BSS_SIZE        EQU 34        ; 16 for CUR_TETRAMINO, 16 for ROT_TETRAMINO, 2 for (x, y) into field.

CUR_TETRAMINO   EQU BSS       ; 16 bytes.
ROT_TETRAMINO   EQU BSS + 16  ; 16 bytes.
OFFSET          EQU BSS + 32  ; 2 bytes.

; TODO: tetramino_collision_check, stack_display, stack_join.

CPU 186

; Entry point.
;     dl    -> the drive number.
;     cs:ip -> linear address 0x7C00.
start:
    jmp 0x0000:.flush_CS                    ; Some BIOS' may load us at 0x0000:0x7C00, while others at 0x07C0:0x0000.

    ; Generic error procedure.
    .error:
        ; Display al.
        xor bx, bx
        mov ah, 0x0E
        int 0x10

        .hlt:
            hlt
            jmp .hlt

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
            mov byte [bx], 0x52
    
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

    mov bx, ax
    add bx, [OFFSET]
    add bx, 35

    ; One character takes 2 bytes in video memory.
    shl bx, 1

    ; Loops for 16 characters.
    mov cl, 0x10
    mov si, CUR_TETRAMINO

    .loop:
        test cl, cl
        jz .ret
        
        dec cl

        mov dl, [si]
        mov [es:bx], dl

        inc bx
        inc bx
        inc si

        test cl, 0b11
        jnz .loop

        ; Since each tetramino is 4x4, we must go to next line
        ; at every multiple of 4.
        add bx, (80 - 4) * 2
        jmp .loop

    .ret:
        popa
        ret

; Rotates CUR_TETRAMINO 90 degrees clock-wise.
tetramino_rotate:
    pusha
    push es

    xor ax, ax
    mov es, ax

    mov si, CUR_TETRAMINO
    mov cx, 4

    .loop:
        mov di, ROT_TETRAMINO - 1
        add di, cx

        mov dl, 4

        .line:
            movsb

            ; One vertical line to write to ROT_TETRAMINO.
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