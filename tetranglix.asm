; 16 bits, starting at 0x7C00.
BITS 16
ORG 0x7C00

; Contrary to widespread information, the byte at 0x500 is also used.
BSS         EQU 0x504
BSS_SIZE    EQU 34

CPU 186

;     dl    -> the drive number.
;     cs:ip -> linear address 0x7C00.
start:
    jmp 0x0000:.flush_CS                    ; Some BIOS' may load us at 0x0000:0x7C00, while others at 0x07C0:0x0000. Let's just make this uniform.

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
    mov ax, 0x0F20
    rep stosw

    jmp $

; Padding.
times 510 - ($ - $$)            db 0

BIOS_signature:
    dw 0xAA55

; Pad to floppy disk.
times (1440 * 1024) - ($ - $$)  db 0