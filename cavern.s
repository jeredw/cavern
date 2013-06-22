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
SPEED    = 3
CLBLACK  = $00
CLWALL   = $44

        SEG.U vars
        ORG $80
SPRITE_X ds 1
SPRITE_Y ds 1
SPRITE_P ds 2
DIR     ds 1
WALKF   ds 1
STEPC   ds 1
FB0     ds 12
FB1     ds 12
FB2     ds 12
FB3     ds 12
FB4     ds 12
FB5     ds 12

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
        lda #150
        sta SPRITE_Y

        ldx #0
.testfb:
        lda TestPat3,x
        sta FB3,x
        lda TestPat4,x
        sta FB4,x
        lda TestPat5,x
        sta FB5,x
        inx
        cpx #12
        bne .testfb

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
        stx VBLANK     ; 0 +3
        lda #CLBLACK   ; 3 +2
        sta COLUPF     ; 5 +3

        ; Wait til the end of the scanline to setup for Picture loop.
        ldx #9         ; 8 +2
PrePic  dex            ; 10 +5(n-1)+4
        bne PrePic     ;
        SLEEP 3        ; 54 +3

        ; 191 scanlines.
        ldx #191 + 1   ; 57 +2
        lda SPRITE_Y   ; 59 +3
        tay            ; 62 +2
        lda #CLBLACK   ; 64 +2
        dey            ; 66 +2
Picture SLEEP 3        ; 68 +3
        sta COLUPF     ; 71 +3
        nop            ; 74 +2
        bmi .nosp1         ; 0 +2
        cpy #SPRITE_HEIGHT ; 2 +2
        bcs .nosp2         ; 4 +2
.sprite lda (SPRITE_P),y   ; 6 +5
        sta GRP0       ; 11 +3
        lda SpriteColor,y ; 14 +4
        sta COLUP0     ; 18 +3
        ; Cycle 21.
        ; There are no interrupts and we're not using stack.
.pf     txs            ; 21 +2
        lda FBRow,x    ; 23 +4
        tax            ; 27 +2
        lda FB3,x      ; 29 +4
        sta PF0        ; 33 +3
        lda FB4,x      ; 36 +4
        sta PF1        ; 40 +3
        lda #CLWALL    ; 43 +2
        sta COLUPF     ; 45 +3
        lda FB5,x      ; 48 +4
        sta PF2        ; 52 +3
        tsx            ; 55 +2
        dex            ; 57 +2
        beq StartBlank ; 59 +2
        lda #CLBLACK   ; 61 +2
        dey            ; 63 +2
        jmp Picture    ; 65 +3
.nosp1  SLEEP 4        ; 3 +4
.nosp2  SLEEP 11       ; 7 +11
        jmp .pf        ; 18 +3

StartBlank
        ; Reset stack pointer.
        ldx #$ff       ; 61 +2
        txs            ; 63 +2
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

        ; Force the branch and target to be on the same page.
        ALIGN 8
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
DeltaY  .byte 0, -1, 0, 1, 0

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

TestPat3
        .byte #%11110000, #%00110000, #%00110000
        .byte #%00110000, #%00110000, #%00110000
        .byte #%00110000, #%00110000, #%00110000
        .byte #%00110000, #%00110000, #%11110000
TestPat4
        .byte #%11111111
        .byte 0,0,0,0,0,0,0,0,0,0
        .byte #%11111111
TestPat5
        .byte #%00111111, #%00110000, #%00110000
        .byte #%00110000, #%00110000, #%00110000
        .byte #%00110000, #%00110000, #%00110000
        .byte #%00110000, #%00110000, #%00111111

        ORG $F300
FBRow
.POS    SET 0
        REPEAT 192
        .byte .POS/16
.POS    SET .POS + 1
        REPEND
        .byte #$0b

        ORG $FFFA

        .word Reset         ; NMI
        .word Reset         ; RESET
        .word Reset         ; IRQ

        END
