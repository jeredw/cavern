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
CLWALL   = $44
SCRH     = 192
FBH      = 16
ROWH     = SCRH / FBH

        SEG.U vars
        ORG $80
SPRITE_X ds 1
SPRITE_Y ds 1
SPRITE_P ds 2
DIR     ds 1
WALKF   ds 1
STEPC   ds 1
FB0     ds FBH
FB1     ds FBH
FB2     ds FBH
FB3     ds FBH

        SEG
        ORG $F000

Reset
        CLEAN_START
        jsr Init

StartOfFrame
        ; Vertical retrace leaving A=0.
        VERTICAL_SYNC

        ; Draw 37 total scanlines of vertical blank.
        ; For the first 35, just blank.
        ldx #35
VBlank  sta WSYNC
        dex
        bne VBlank

        ; Position sprite 0 (x=0).
        lda SPRITE_X
        jsr XPos       ; Note that x=0 here. Waits.
        ; Line 36 of blanking.
        sta WSYNC
        ; Line 37 of blanking.
        sta HMOVE

        ; Turn off blanking and spend one line to set up the main loop.
        sta WSYNC
        stx VBLANK     ; 0  +3
        ; No playfield on setup line.
        stx PF1        ; 3  +3
        stx PF2        ; 6  +3

        ; The Picture loop begins at the end of a scanline, so wait until the
        ; end of the setup line to do loop setup.
        ldx #9         ; 9  +2
PrePic  dex            ; 11 +5(n-1)+4
        bne PrePic     ; 
        SLEEP 3        ; 55 +3

        ; 191 scanlines.
        ldx #191 + 1   ; 58 +2
        lda SPRITE_Y   ; 60 +3
        tay            ; 63 +2
        lda.b FB0+#FBH-1 ; 65 +3 Set up initial PF1 data.
        dey            ; 68 +2
        jmp Picture    ; 70 +3

.newr   sta PF2        ; 45 +3
        lda FB3,x      ; 48 +4
        SLEEP 4        ; 52 +4
        sta PF1        ; 56 +3
        lda FB0-1,x    ; 59 +4
        SLEEP 4        ; 63 +4
        tsx            ; 65 +2
        dex            ; 67 +2
        dey            ; 69 +2
Picture sta PF1        ; 73 +3 Store PF1 data.
        bmi .nosp1     ; 0  +2 If y < 0, not in sprite.
        cpy #SPRITE_HEIGHT ; 2  +2 Test y against sprite height.
        bcs .nosp2         ; 4  +2 If y > H, not in sprite.
.sprite lda (SPRITE_P),y   ; 6  +5 Load sprite bitmap row.
        sta GRP0           ; 11 +3 Store sprite bitmap row.
        lda SpriteColor,y  ; 14 +4 Load sprite row color.
        sta COLUP0         ; 18 +3 Store sprite row color.
        ; Render playfield from framebuffer.
.pf     lda FBRow,x    ; 21 +4 Look up FB row for this line.
        lsr            ; 25 +2 Set carry if row changes next line.
        txs            ; 27 +2 Save X. Not using stack, no interrupts.
        tax            ; 29 +2 Get FB row in X.
        lda FB1,x      ; 31 +4 Get data for PF2.
        sta PF2        ; 35 +3 Store PF2 data.
        lda FB2,x      ; 38 +4 Get data for new PF2.
        bcs .newr      ; 42 +2 Branch if next line is new row.
.samer  sta.w PF2      ; 44 +4 Set new PF2 just before it starts.
        lda FB3,x      ; 48 +4 Get data for PF1.
        SLEEP 3        ; 52 +3
        sta PF1        ; 55 +3 Store PF1 data.
        lda FB0,x      ; 58 +4 Get next line PF1 data.
        tsx            ; 62 +2 Restore line number.
        dex            ; 64 +2 Next line.
        beq StartBlank ; 66 +2
        dey            ; 68 +2 Next sprite line.
        jmp Picture    ; 70 +3
.nosp1  SLEEP 4        ; 3  +4 Delay to match sprite lines.
.nosp2  SLEEP 11       ; 7  +11
        jmp .pf        ; 18 +3

StartBlank
        ; Reset stack pointer which was clobbered in the Picture loop.
        ldx #$ff       ; 69 +2
        txs            ; 71 +2
        ; Begin line 1 of overscan.
        sta WSYNC

        ; Black the picture.
        lda #%01000010 ; Black and latch input.
        sta VBLANK     ; Turn on blacking.

        ; 30 total lines of overscan.
        ; Take off lines as needed for logic and add below.
        ldx #28
Overscan
        dex            ; Count line.
        sta WSYNC      ; Start new line.
        bne Overscan
        
        ; Line 29. Decode direction from joystick.
        ; On entry, X is DIR_STOP=0.
DecodeDir
        lda #$10       ; +2 Check bit D4.
        bit SWCHA      ; +3 Test input latch.
        bpl .left      ; +2 D7: Left joystick switch closed.
        bvc .right     ; +2 D6: Right joystick switch closed.
        beq .up        ; +2 D4: Upwards joystick switch closed.
        asl            ; +2 Now check D5.
        bit SWCHA      ; +3 Test input latch again.
        bne .stop      ; +2 If D5 not set, stop moving.
        ldx #DIR_S     ; +2 Moving down.
        .byte #$2c     ; +3 Skip the next instruction.
.right  ldx #DIR_W     ; ?2 Moving left.
        .byte #$2c     ; +3 Skip the next instruction.
.left   ldx #DIR_E     ; ?2 Moving right.
        .byte #$2c     ; +3 Skip the next instruction.
.up     ldx #DIR_N     ; ?2 Moving up.
.stop   txa            ; +2 Get dir in A.
        sta DIR        ; [33] +3 Store dir. [Worst case cycle 33.]
Step    bne .step      ; +2 If moving, count steps.
        lda #SPEED+1   ; +2 Not moving. Reset step counter.
        sta STEPC      ; +3
.step   dec STEPC      ; [48] +5 Count step.
        ; Set sprite direction properly. If facing west, mirror the sprite,
        ; because the east-west sprite is drawn facing east.
        lda #0         ; +2
        sta REFP0      ; +3 Default, do not mirror.
        lda DIR        ; +3
        cmp #DIR_W     ; +2 Test if facing west.
        bne .fdone     ; +2 If not, done with line.
        lda #%1000     ; +2 Mirror.
        sta REFP0      ; +3 
        ; Timing is a little conservative, since worst case above is actually
        ; south not west.
.fdone  sta WSYNC      ; [70] +3

        ; Line 30. Animate the sprite.
        ; X has the current motion direction.
Animate cpx #DIR_STOP  ; +2 Moving?
        bne .cstep     ; +2 If so, then check step counter.
        lda #-1        ; +2 Otherwise reset frame to stationary frame.
        sta WALKF      ; +3
        bne .cycle     ; +2 Always taken.
.cstep  lda STEPC      ; +3 Check step counter.
        bne .adone     ; +2 If nonzero, not time to animate yet.
        lda #SPEED     ; +2 Reset step counter.
        sta STEPC      ; +3
        lda WALKF      ; +2
        and #1         ; +2 Move on frame 0 and 2.
        beq .cycle     ; +2
.move   lda DeltaX,x   ; +4 Get x motion amount for dir.
        clc            ; +2
        adc SPRITE_X   ; +3 Add to x coordinate.
        sta SPRITE_X   ; +3
        lda DeltaY,x   ; +4 Get y motion amount for dir.
        clc            ; +2
        adc SPRITE_Y   ; +3 Add to y coordinate.
        sta SPRITE_Y   ; +3
        ; Set up next frame of animation.
.cycle  lda WALKF      ; +2 Get current frame number.
        clc            ; +2
        adc #1         ; +2 Next frame.
        and #3         ; +2 Wrap between frames 0..3.
        sta WALKF      ; +3 Save new frame number.
        asl            ; +2 Index pointers in SpriteData.
        tax            ; +2 Save index.
        lda SpriteData,x   ; +4 Load LSB.
        sta SPRITE_P       ; +3
        lda SpriteData+1,x ; +4 Load MSB.
        sta SPRITE_P+1 ; +3
.adone  sta WSYNC      ; [72] +3

        jmp StartOfFrame

; Initialize machine state.
Init    SUBROUTINE
        sta SWACNT     ; Configure PORT A as input.

        lda START_X
        sta SPRITE_X
        lda #<SpriteWalkF1
        sta SPRITE_P
        lda #>SpriteWalkF1
        sta SPRITE_P+1
        lda #150
        sta SPRITE_Y
        ; D0: REF=1, reflect playfield.
        ; D1: SCORE=0, do not color left/right differently.
        ; D2: PFP=0, player is in front of pf.
        ; D4-D5: BALL SIZE 0, 1 clock.
        lda #%00001
        sta CTRLPF
        ; PF0 is always blank.
        lda #0
        sta PF0
        lda #CLWALL
        sta COLUPF

        ldx #0
        lda #$ff
.testfb:
        lda TestPat0,x
        sta FB0,x
        lda TestPat1,x
        sta FB1,x
        lda TestPat2,x
        sta FB2,x
        lda TestPat3,x
        sta FB3,x
        inx
        cpx #FBH
        bne .testfb

        rts

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

TestPat0
        .byte #%10101010, #%01010101, #%10101010, #%01010101
        .byte #%10101010, #%01010101, #%10101010, #%01010101
        .byte #%10101010, #%01010101, #%10101010, #%01010101
        .byte #%10101010, #%01010101, #%10101010, #%01010101
TestPat1
        .byte #%01010101, #%10101010, #%01010101, #%10101010
        .byte #%01010101, #%10101010, #%01010101, #%10101010 
        .byte #%01010101, #%10101010, #%01010101, #%10101010
        .byte #%01010101, #%10101010, #%01010101, #%10101010
TestPat2
        .byte #%10101010, #%01010101, #%10101010, #%01010101
        .byte #%10101010, #%01010101, #%10101010, #%01010101
        .byte #%10101010, #%01010101, #%10101010, #%01010101
        .byte #%10101010, #%01010101, #%10101010, #%01010101
TestPat3
        .byte #%01010101, #%10101010, #%01010101, #%10101010
        .byte #%01010101, #%10101010, #%01010101, #%10101010 
        .byte #%01010101, #%10101010, #%01010101, #%10101010
        .byte #%01010101, #%10101010, #%01010101, #%10101010

        ORG $F300
FBRow
.POS    SET 0
        REPEAT SCRH
        ; The low bit says whether the next line will be from the next FB row.
        ; Setting the bit for line 1 to make Picture loop termination work.
        .byte ((.POS / ROWH) << 1) + ((.POS % ROWH) == 0)
.POS    SET .POS + 1
        REPEND
        .byte (2 * (#FBH - 1))

        ORG $FFFA

        .word Reset         ; NMI
        .word Reset         ; RESET
        .word Reset         ; IRQ

        END
