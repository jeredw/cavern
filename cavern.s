        processor 6502
        include "vcs.h"
        include "macro.h"

PATTERN EQU $80
TIMECH  EQU 20

        SEG
        ORG $F000

Reset
        CLEAN_START

        sta PATTERN
        lda #$45
        sta COLUPF

StartOfFrame
        ; Enable vertical sync generation.
        lda #2
        sta VSYNC

        ; Draw 3 scanlines of vsync to trigger retrace.
        ; VBLANK bit 1 should be set to black the screen since some TVs don't
        ; understand vsync unless blacking.
        sta WSYNC
        sta WSYNC
        sta WSYNC

        ; Turn off vertical sync gen.
        lda #0
        sta VSYNC

        ; Draw 37 scanlines of vertical blank.
        ldx #37
VBlank  sta WSYNC
        dex
        bne VBlank
        stx VBLANK     ; Re-enable video output.

        iny
        cpy TIMECH
        bne notyet
        ldy #0
        inc PATTERN
notyet

        ; 192 scanlines of picture
        ldx #192
Picture stx COLUPF
        sta WSYNC
        lda PATTERN
        sta PF1
        SLEEP 40
        lda #0
        sta PF1
        dex
        bne Picture

        lda #%01000010
        sta VBLANK        ; end of scren

        ; 30 scanlines of overscan
        ldx #30
Overscan
        sta WSYNC
        dex
        bne Overscan
        
        jmp StartOfFrame

        ORG $FFFA

        .word Reset         ; NMI
        .word Reset         ; RESET
        .word Reset         ; IRQ

        END
