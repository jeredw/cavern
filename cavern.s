        processor 6502
        include "vcs.h"
        include "macro.h"

START_X = 3
END_X   = 150
SPRITE_HEIGHT = 10
DIR_STOP = 0
DIR_N    = 1
DIR_E    = 2
DIR_S    = 3
DIR_W    = 4
SPEED    = 4

        SEG.U vars
        ORG $80
SPRITE_X ds 1
SPRITE_Y ds 1
SPRITE_P ds 2
SPRITE_C ds 2
DIR     ds 1
WALKF   ds 1
STEPC   ds 1

        SEG
        ORG $F000

Reset
        CLEAN_START

        sta SWACNT     ; Configure PORT A as input.
        lda #$56
        sta COLUP0
        lda START_X
        sta SPRITE_X
        lda #<SpriteWalkF1
        sta SPRITE_P
        lda #>SpriteWalkF1
        sta SPRITE_P+1
        lda #<SpriteColor
        sta SPRITE_C
        lda #>SpriteColor
        sta SPRITE_C+1
        lda #50
        sta SPRITE_Y

StartOfFrame
        ; Vertical retrace leaving A=0.
        VERTICAL_SYNC

        ; Draw 37 total scanlines of vertical blank.
        ; For the first 35, just blank.
        ldx #35 + 1
VBlank  sta WSYNC
        dex
        bne VBlank

        ; Position sprite 0 (x=0).
        lda SPRITE_X
        jsr XPos       ; Line 36 of blanking.
        sta WSYNC      ; Line 37 of blanking.
        sta HMOVE

        ; Turn off vertical blanking.
        sta WSYNC
        stx VBLANK     ; Re-enable video output.

        ; 192 scanlines of picture
        ldx #192
Picture
        sec            ; 2
        txa            ; 2
        sbc SPRITE_Y   ; 3
        adc #SPRITE_HEIGHT ; 2
        bcs .MBDraw3   ; 2(3)
        nop            ; 2
        nop            ; 2
        sec            ; 2
        bcs .skipMBDraw3 ; 3
.MBDraw3
        tay
        lda (SPRITE_P),y ; 5
        sta GRP0       ; 3
        lda (SPRITE_C),y ; 5
        sta COLUP0
.skipMBDraw3
        sta WSYNC
        dex
        bne Picture

        sta WSYNC
        ; Black the picture.
        lda #%01000010
        sta VBLANK        ; end of scren

        ; 30 total lines of overscan. (More logic below.)
        ldx #28+1
Overscan
        sta WSYNC
        dex
        bne Overscan
        
        ; Line 29. Decode direction from joystick.
        ; X is DIR_STOP.
DecodeDir
        lda #$10
        bit SWCHA
        bpl .left
        bvc .right
        beq .up
        asl
        bit SWCHA
        bne .stop
        ldx #DIR_S
        .byte #$2c
.right  ldx #DIR_W
        .byte #$2c
.left   ldx #DIR_E
        .byte #$2c
.up     ldx #DIR_N
.stop   txa
        sta DIR        ; Worst case 28 cycles.
Step    bne .step      ; If moving, count steps.
        lda #SPEED+1
        sta STEPC
.step   dec STEPC
        sta WSYNC

        ; Line 30. Animate.
Animate lda DIR        ; Stand still if not moving.
        bne .chk
        lda #-1
        sta WALKF
        bne .cycle
.chk    lda STEPC
        bne .adone     ; Not time to animate yet.
        lda #SPEED     ; Reset step counter.
        sta STEPC
        lda WALKF
        and #1         ; Move on frame 0 and 2.
        beq .cycle
.move   lda DeltaX,x
        clc
        adc SPRITE_X
        sta SPRITE_X
        lda DeltaY,x
        clc
        adc SPRITE_Y
        sta SPRITE_Y
.cycle  lda WALKF
        clc
        adc #1
        and #3
        sta WALKF
        asl
        tax
        lda SpriteData,x
        sta SPRITE_P 
        lda SpriteData+1,x
        sta SPRITE_P+1
.adone  sta WSYNC

        jmp StartOfFrame

; Position object X in column A.
XPos    SUBROUTINE
        sec                 ; 2
        sta WSYNC           ; 3, line 1
.Divide
        sbc #15             ; 2
        bcs .Divide         ; 54 max
        eor #7              ; 2
        asl                 ; 2
        asl                 ; 2
        asl                 ; 2
        asl                 ; 2
        sta HMP0,x          ; 4       68
        sta RESP0,x         ; 4       72
        rts

        ;     s  N  E  S  W
DeltaX  .byte 0, 0, 1, 0, -1
DeltaY  .byte 0, 1, 0, -1, 0

SpriteData
        .word SpriteWalkF1
        .word SpriteWalkF2
        .word SpriteWalkF3
        .word SpriteWalkF4

SpriteWalkF1
        .byte #%00000000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00111000
        .byte #%00010000
        .byte #%00111000
        .byte #%01111100
        .byte #%00111000
SpriteWalkF2
        .byte #%00000000
        .byte #%00010000
        .byte #%00011000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00011000
        .byte #%00111000
        .byte #%01111100
        .byte #%00111000
SpriteWalkF3
        .byte #%00000000
        .byte #%00011000
        .byte #%00011000
        .byte #%00010000
        .byte #%00010000
        .byte #%00011000
        .byte #%00110000
        .byte #%00111000
        .byte #%01111100
        .byte #%00111000
SpriteWalkF4
        .byte #%00000000
        .byte #%00011000
        .byte #%00011000
        .byte #%00001000
        .byte #%00001000
        .byte #%00001000
        .byte #%00011000
        .byte #%00011100
        .byte #%00111110
        .byte #%00011100

SpriteColor
        .byte #$56, #$26, #$25, #$82, #$37
        .byte #$26, #$25, #$82, #$33, #$56

        ORG $FFFA

        .word Reset         ; NMI
        .word Reset         ; RESET
        .word Reset         ; IRQ

        END
