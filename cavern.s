        processor 6502
        include "vcs.h"
        include "macro.h"

COUNT   EQU $80
SCOLOR  EQU $81

        SEG
        ORG $F000

Reset
        sta SCOLOR
StartOfFrame

        ; Vertical blanking
        lda #0
        sta VBLANK
        lda #2
        sta VSYNC

        ; 3 scanlines of vsync
        sta WSYNC
        sta WSYNC
        sta WSYNC

        lda #0
        sta VSYNC

        ; 37 scanlines of vertical blank
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC

        ; 192 scanlines of picture
        ldx #0
        ldy #0
        REPEAT 192; scanlines
          inx
          stx COLUBK
          nop
          nop
          nop
          nop
          nop
          nop
          nop
          nop
          nop
          nop
          dey
          sty COLUBK
          sta WSYNC
        REPEND
        inc SCOLOR

        lda #%01000010
        sta VBLANK        ; end of scren

        ; 30 scanlines of overscan
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        
        jmp StartOfFrame

        ORG $FFFA

        .word Reset         ; NMI
        .word Reset         ; RESET
        .word Reset         ; IRQ

        END
