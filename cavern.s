        processor 6502
        include "vcs.h"
        include "macro.h"

START_X  = 160/2 - 5
START_Y  = 73
SPRITE_HEIGHT = 10
DIR_STOP = 0
DIR_N    = 1
DIR_E    = 2
DIR_S    = 3
DIR_W    = 4
SPEED    = 3
CLDARK   = $00
LINES    = 192
CAVEW    = 128
CAVEH    = 64
FBW      = 32
FBH      = 16
ROWH     = LINES / FBH
TOS      = $FF
PREVCOL  = -1
NEXTCOL  = 1
PREVROW  = -CAVEW/8
NEXTROW  = CAVEW/8 
LBOUND   = 40
RBOUND   = 120

        ; 128 bytes of RAM 
        SEG.U vars
        ORG $80

        ; Drawing state: 16 bytes $80 to $8F.
SPRITE_X ds 1          ; Screen X position of sprite.
SPRITE_Y ds 1          ; Screen Y position of sprite.
SCROLL_R ds 1          ; Top row of cave shown, [0, 47].
SCROLL_C ds 1          ; Leftmost column of cave shown, [0, 95].
SCREEN_M ds 1          ; Screen column bit mask.
SPRITE_P ds 2          ; Pointer to sprite row.
DIR     ds 1           ; The current motion direction.
WALKF   ds 1           ; The current walk frame number.
STEPC   ds 1           ; Counter for advancing frame number.
CAVE_P  ds 2           ; Pointer to cave row.
ATWALL  ds 1
        ds 4           ; Spare.

        ; Framebuffer: 64 bytes $90 to $CF.
        ; Each slice maps to PF1 or PF2 at one position per scanline.
        ; xxxx76543210012345677654321001234567xxxx
        ;     |<---->||<---->||<---->||<---->|
        ;       FB0     FB1     FB2     FB3
FB0     ds FBH
FB1     ds FBH
FB2     ds FBH
FB3     ds FBH 

        ; Game state: 24 bytes $D0 to $E7.
        ds 24          ; Spare.

        ; Stack: 24 bytes $E8 to $FF.
        ; The lowest stack bytes are shared for transient storage.
R1      ds 1
R2      ds 1
R3      ds 1
R4      ds 1
R5      ds 1
R6      ds 1
R7      ds 1
R8      ds 1
        ds 16          ; Stack!

        SEG
        ORG $F000

Reset
        CLEAN_START
        jsr Init

StartOfFrame
        ; Vertical retrace leaving A=0.
        VERTICAL_SYNC

        ; Scroll the screen, consuming some number of scanlines.
        ldx #35
        lda DIR
        cmp #DIR_E
        bne .dirw
        lda SPRITE_X
        cmp #RBOUND
        bcc .noscrl
        jsr PanRight
        bcc .noscrl
        lda #RBOUND-4
        sta SPRITE_X
        jmp .noscrl
.dirw   cmp #DIR_W
        bne .noscrl
        lda SPRITE_X
        cmp #LBOUND
        bcs .noscrl
        jsr PanLeft
        bcc .noscrl
        lda #LBOUND+2
        sta SPRITE_X
        jmp .noscrl
;.dirn   cmp #DIR_N
;        bne .dirs
;        jsr PanUp
;        jmp .noscrl
;.dirs   cmp #DIR_S
;        bne .dirs
;        jsr PanDown
.noscrl

        stx R3
        lda ATWALL
        beq .grav
        lda DIR
        cmp #DIR_N
        bne .grav
        lda SPRITE_Y
        sec
        sbc #3
        tay 
        ldx ATWALL
        lda DeltaX,x
        asl
        asl
        clc
        adc SPRITE_X
        tax
        jsr GetBit
        bcs Ground
        jmp Ledge
        ; Get tile beneath current position.
.grav   ldx SPRITE_X
        ldy SPRITE_Y
        jsr GetBit
        bcs Ground
        inc SPRITE_Y
        lda #0
        sta WALKF
Ground  ldx R3

        ; Draw 37 total scanlines of vertical blank.
        ; For the first 35, just blank.
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

        ldx #0         ; 55 +2
        ldy SPRITE_Y   ; 57 +3
        lda #$80       ; 60 +2
        sta R1         ; 62 +3
        lda FB0        ; 65 +3 Set up initial PF1 data.
        dey            ; 68 +2
        jmp Picture    ; 70 +3

; X is the framebuffer row number in 0..15.
; R1 is a one-hot counter which sets the carry bit every 8 cycles
; to trigger incrementing the framebuffer row.
.newr   nop            ; 43 +2
        sta PF2        ; 45 +3
        lda FB3,x      ; 48 +4
        inx            ; 52 +2
        cpx #16        ; 54 +2
        sta PF1        ; 56 +3
        beq UnderCave  ; 59 +2
        lda #$80       ; 61 +2
        sta.w R1       ; 63 +4
        lda FB0,x      ; 67 +4
        dey            ; 71 +2
Picture sta PF1        ; 73 +3 Store PF1 data.
        bmi .nosp1     ; 0  +2 If y < 0, not in sprite.
        cpy #SPRITE_HEIGHT ; 2  +2 Test y against sprite height.
        bcs .nosp2         ; 4  +2 If y >= H, not in sprite.
.sprite lda (SPRITE_P),y   ; 6  +5 Load sprite bitmap row.
        sta GRP0           ; 11 +3 Store sprite bitmap row.
        lda SpriteColor,y  ; 14 +4 Load sprite row color.
        sta COLUP0         ; 18 +3 Store sprite row color.
        ; Render playfield from framebuffer.
.pf     lda R1         ; 21 +3 Get ring counter.
        lsr            ; 24 +2 Count line, set carry if row changes.
        sta R1         ; 26 +3 Save ring counter.
        lda FB1,x      ; 29 +4 Get data for PF2.
        sta PF2        ; 33 +3 Store PF2 data.
        lda FB2,x      ; 36 +4 Get data for new PF2.
        bcs .newr      ; 40 +2 Branch if next line is new row.
        SLEEP 3        ; 42 +3
.samer  sta PF2        ; 45 +3 Set new PF2 just before it starts.
        lda FB3,x      ; 48 +4 Get data for PF1.
        SLEEP 3        ; 52 +3
        sta PF1        ; 55 +3 Store PF1 data.
        lda FB0,x      ; 58 +4 Get next line PF1 data.
        SLEEP 6        ; 62 +6
        dey            ; 68 +2 Next sprite line.
        jmp Picture    ; 70 +3
.nosp1  SLEEP 4        ; 3  +4 Delay to match sprite lines.
.nosp2  cpy #20        ; 7  +2
        bcs .bl        ; 9  +2
        lda PFCol,y    ; 11 +4
        sta COLUPF     ; 15 +3
        jmp .pf        ; 18 +3
.bl     lda #CLDARK    ; 12 +2
        sta.w COLUPF   ; 14 +4
        jmp .pf        ; 18 +3

UnderCave
        ldx #192 - #128
.fill
        sta WSYNC
        lda #0
        sta PF1
        sta PF2
        dex
        bne .fill

        ; Begin line 1 of overscan.
        sta WSYNC

        ; Black the picture.
        lda #%01000010 ; Black and latch input.
        sta VBLANK     ; Turn on blacking.

        ; 30 total lines of overscan.
        ; Take off lines as needed for logic and add below.
        ldx #27
Overscan
        dex            ; Count line.
        sta WSYNC      ; Start new line.
        bne Overscan

        ; Line 28. Check for collisions.
ChkColl lda CXP0FB
        bpl .cdone
        lda DIR
        cmp #DIR_N
        bne .bump
        inc SPRITE_Y
        jmp .cdone
.bump   tax 
        ldy OppDir,x
        lda DeltaX,y
        clc
        adc SPRITE_X
        sta SPRITE_X
        lda DIR
        sta ATWALL
        lda #DIR_STOP
        sta DIR
        sta STEPC
.cdone  sta CXCLR
        sta WSYNC
        
        ; Line 29. Decode direction from joystick.
        ; On entry, X is DIR_STOP=0.
DecodeDir
        lda #$10       ; +2 Check bit D4.
        bit SWCHA      ; +3 Test input latch.
        bpl .left      ; +2 D7: Left joystick switch closed.
        bvc .right     ; +2 D6: Right joystick switch closed.
        beq .up        ; +2 D4: Upwards joystick switch closed.
        jmp .stodir
.right  lda #%1000     ; +2 Mirror sprite.
        sta REFP0      ; +3 
        ldx #DIR_W     ; +2 Moving left.
        jmp .stodir
.left   lda #0         ; +2 Do not mirror sprite.
        sta REFP0      ; +3
        ldx #DIR_E     ; +2 Moving right.
        jmp .stodir
.up     lda ATWALL
        beq .stodir
        ldx #DIR_N     ; ?2 Moving up.
.stodir txa            ; +2 Get dir in A.
        sta DIR        ; [33] +3 Store dir. [Worst case cycle 33.]
Step    bne .step      ; +2 If moving, count steps.
        lda #SPEED+1   ; +2 Not moving. Reset step counter.
        sta STEPC      ; +3
.step   dec STEPC      ; [48] +5 Count step.
        sta WSYNC      ; [70] +3

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
        tay
        clc            ; +2
        adc SPRITE_X   ; +3 Add to x coordinate.
        sta SPRITE_X   ; +3
        cpy #0
        beq .movey
        lda #0
        sta ATWALL
.movey  lda DeltaY,x   ; +4 Get y motion amount for dir.
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

        lda #0
        sta ATWALL
        lda #<SpriteWalkF1
        sta SPRITE_P
        lda #>SpriteWalkF1
        sta SPRITE_P+1
        lda #START_X
        sta SPRITE_X
        lda #START_Y
        sta SPRITE_Y
        lda #<CaveData
        sta CAVE_P
        lda #>CaveData
        sta CAVE_P+1
        ; D0: REF=1, reflect playfield.
        ; D1: SCORE=0, do not color left/right differently.
        ; D2: PFP=0, player is in front of pf.
        ; D4-D5: BALL SIZE 0, 1 clock.
        lda #%00001
        sta CTRLPF
        ; PF0 is always blank.
        lda #0
        sta PF0
        lda #CLDARK
        sta COLUPF

        ldx #0
        ldy #0
.testfb:
        jsr CopyRow
        jsr StorRow
        tya
        clc
        adc #16
        tay
        inx
        cpx #FBH
        bne .testfb

        rts

; This chunk of code is here so the picture loop is on one page.
Ledge   ldx ATWALL
        stx DIR
        lda DeltaX,x
        asl
        clc
        adc SPRITE_X
        sta SPRITE_X
        lda #0
        sta ATWALL
        lda SPRITE_Y
        sec
        sbc #3
        sta SPRITE_Y
        jmp Ground

        ; Force the branch and target to be on the same page.
        ALIGN 8
; Position object X in column A.
XPos    SUBROUTINE
        sec            ; 2
        sta WSYNC      ; 3, line 1
.Divide
        sbc #15        ; 2
        bcs .Divide    ; 54 max
        eor #7         ; 2
        asl            ; 2
        asl            ; 2
        asl            ; 2
        asl            ; 2
        sta HMP0,x     ; 4       68
        sta RESP0,x    ; 4       72
        rts

; Gets the framebuffer bit at screen position X,Y.
; Carry is set if bit is set, zero otherwise.
GetBit  SUBROUTINE
        txa            ; Get x coordinate in A.
        sbc #16 - #1   ; Subtract PF0 and offset center of sprite.
        sta R2         ; Save in R2.
        lsr            ; Compute (x/32)*16.
        and #$f0       ; 
        sta R1         ; Save x part of offset in R1.
        tya            ; Get y coordinate in A.
        sec            ; Subtract empty space from sprite.
        sbc #1         ;
        lsr            ; Compute y/8.
        lsr            ;
        lsr            ;
        ora R1         ; Combine x and y offsets.
        tax            ; Get FB offset in x.
        lda FB0,x      ; Read from framebuffer.
        sta R1         ; Save data in x.
        lda R2         ; Restore screen x from R2.
        lsr            ; Compute x/4.
        lsr            ;
        and #7         ; Get column bit number.
        sta R2         ; Save it in R2.
        txa            ; Get bit number in A.
        ; 0-15, 16-31, 32-47, 48-63
        ; norm  mirr   norm   mirr
        and #$10       ; Is this a mirrored byte?
        cmp #$10       ; Set carry if so.
        lda R2         ; Get column number from R2.
        bcc .nomirr    ; Not mirrored.
        eor #7         ; Mirror.
.nomirr tax            ; Get column number in x.
        lda lmask,x    ; Load column mask.
        sta R2         ; Store mask.
        and R1         ; Get bit.
        cmp R2         ; Set carry if set, else clear it.
        rts

; Reverse the bits in A (65 cycles).
Reverse sta R8         ; +3
        REPEAT 8       ; +56
          asl R8       ;   +5 abcdefgh | ????????
          ror          ;   +2 bcdefgh0 | a???????
        REPEND
        rts            ; +6

; Copies 5 bytes from the cave bitmap at CAVE_P+y into R1:R5.
; 20 bytes.
CopyRow SUBROUTINE
        txa
        pha
        tya
        pha
        ldx #0
.copy   lda (CAVE_P),y
        iny
        sta R1,x
        inx
        cpx #5
        bne .copy
        pla
        tay
        pla
        tax
        rts

; Shifts R1:R5 so that bit 7 of R1 in R1:R5 corresponds to SCROLL_C.
ShftRow SUBROUTINE
        lda SCROLL_C
        and #7
        tax
.shift  beq .out
        asl R5
        rol R4
        rol R3
        rol R2
        rol R1
        dex
        jmp .shift
.out    rts

; Copies row from temporary in R1:R4 to row X of FB0:FB3.
; Reverses data for FB1 and FB3 for PF2 and PF1-mirrored.
StorRow SUBROUTINE
        lda R1
        sta FB0,x
        lda R2
        jsr Reverse
        sta FB1,x
        lda R3
        sta FB2,x
        lda R4
        jsr Reverse
        sta FB3,x
        rts

; Adds X:A to cave bitmap pointer.
AddCave SUBROUTINE
        clc            ;
        adc CAVE_P     ; LSB of cave row.
        sta CAVE_P     ; Store LSB.
        txa            ; Carry into MSB.
        adc CAVE_P+1   ; MSB of row.
        sta CAVE_P+1   ; Store MSB.
        rts

; Pans the screen down by one row.
PanDown SUBROUTINE
        lda SCROLL_R   ; Get screen start row.
        cmp #CAVEH-#FBH ; Compare to max start row.
        beq .out       ; If equal, do not pan down.
        inc SCROLL_R   ; Pan down.
        lda <#NEXTROW  ; Offset of next bitmap row.
        ldx >#NEXTROW  ;
        jsr AddCave    ; Add one row.
        ; Move framebuffer contents up one row.
        ldx -#FBH+1
.moveup lda FB0+16,x   ; i.e., from 1, 2, ..., 15
        sta FB0+15,x   ;         to 0, 1, ..., 14.
        lda FB1+16,x
        sta FB1+15,x
        lda FB2+16,x
        sta FB2+15,x
        lda FB3+16,x
        sta FB3+15,x
        inx            ; Next row.
        bne .moveup    ; Keep copying.

        ; Copy new framebuffer row.
        ldy (#CAVEW/8)*(#FBH-1)  ; Start of the last row.
        jsr CopyRow    ; Copy new row into temporary storage.
        jsr ShftRow    ; Shift the row into place.
        ldx #FBH-1     ; Replace the last FB row.
        jsr StorRow    ; Store the new row into FB.
        ldx #24
.out    rts

; Pans the screen up by one row.
PanUp   SUBROUTINE
        lda SCROLL_R   ; Get start screen row.
        beq .out       ; If zero, do not pan up.
        dec SCROLL_R   ; Pan up.
        lda #<PREVROW  ; Offset of previous bitmap row.
        ldx #>PREVROW  ;
        jsr AddCave    ; Subtract one row.
        ; Move framebuffer contents down one row.
        ldx #FBH-1
.movedn lda FB0-1,x    ; i.e. from 14, 13, ..., 0
        sta FB0,x      ;        to 15, 14, ..., 1.
        lda FB1-1,x
        sta FB1,x
        lda FB2-1,x
        sta FB2,x
        lda FB3-1,x
        sta FB3,x
        dex
        bne .movedn

        ; Copy new framebuffer row.
        ldy #0         ; Start of the first row.
        jsr CopyRow    ; Copy new row into temporary storage.
        jsr ShftRow    ; Shift the row into place.
        ldx #0         ; Replace row 0.
        jsr StorRow    ; Store the new row into FB.
        ldx #24
.out    rts

; Pans the screen left by one column.
PanLeft SUBROUTINE
        lda SCROLL_C   ; Get screen start column.
        bne .scrl      ; If not at 0, scroll.
        clc            ; Clear carry since didn't scroll.
        rts            ; Return.
.scrl   dec SCROLL_C   ; Pan left.
        lda SCROLL_C   ; Get new left column.
        and #7         ; Get start pixel of left column.
        sta R1         ; Stash it.
        cmp #7         ; Pixel 7 implies a new leftmost column byte.
        bne .ptrok     ; Else current cave ptr is ok.
        lda <#PREVCOL  ; Offset to previous cave byte.
        ldx >#PREVCOL  ;
        jsr AddCave    ; Scan left one column.
.ptrok  lda R1         ; Start column (mod 8).
        tax            ;
        lda lmask,x    ; Index mask table.
        sta R1         ; Store mask for start column.
        ldy #0         ; y indexes left col cave bytes (0, 16, ..., 240).
        ldx -#FBH      ; x+16 indexes framebuffer rows.
        ; Shift in one new bit on the left of each framebuffer row.
.shift  lda (CAVE_P),y ; Load leftmost cave byte.
        and R1         ; Mask the newly visible bit.
        cmp R1         ; Set carry if bit is set, else clear.
        ror FB0+16,x   ; Carry goes into leftmost pixel (bit 7).
        rol FB1+16,x   ; FB1 is reversed because PF2 is.
        ror FB2+16,x   ; FB2 is a mirror of PF2, so _not_ reversed.
        rol FB3+16,x   ; FB3 is a mirrored PF1.
        tya            ; Next cave row. 
        clc
        adc #CAVEW/8   ; Bytes per cave row.
        tay
        inx            ; Next framebuffer row.
        bne .shift
        ldx #24
        sec
        rts

; Pans the screen right by one column.
PanRight SUBROUTINE
        lda SCROLL_C   ; Get screen start column.
        cmp #CAVEW-#FBW ; Test against rightmost column.
        bne .scrl      ; If at rightmost column don't pan right.
        clc            ; Flag did not scroll.
        rts            ; Return.
.scrl   inc SCROLL_C   ; Else pan right.
        ;0123456701234567012345670123456701234567
        ;       |                              |
        ;aaaaaaaabbbbbbbbccccccccddddddddeeeeeeee
        ldy #4         ; Rightmost column is usually +4 bytes away.
        lda SCROLL_C   ; Load scroll start.
        and #7         ; Get start pixel of left column.
        sta R1         ; Stash it.
        bne .ptrok     ; Pixel 0 implies a new leftmost column byte.
        lda <#NEXTCOL  ; Offset to next cave byte.
        ldx >#NEXTCOL  ;
        jsr AddCave    ; Scan right one column.
        ;0123456701234567012345670123456701234567
        ;        |                              |
        ;aaaaaaaabbbbbbbbccccccccddddddddeeeeeeee
        ldy #3         ; In this case, rightmost column is +3 bytes.
.ptrok  lda R1         ; Start column (mod 8).
        tax            ;
        lda rmask,x    ; Index mask table.
        sta R1         ; Store mask for start column.
        ldx -#FBH      ; x+16 indexes framebuffer rows.
        ; Shift in one new bit on the right of each framebuffer row.
.shift  lda (CAVE_P),y ; Load rightmost cave byte.
        and R1         ; Mask the newly visible bit.
        cmp R1         ; Set carry if bit is set, else clear.
        ror FB3+16,x   ; Carry goes into rightmost pixel (bit 7).
        rol FB2+16,x   ; Leftmost FB3 -> rightmost FB2.
        ror FB1+16,x   ; Leftmost FB2 -> rightmost FB1.
        rol FB0+16,x   ; Leftmost FB1 -> rightmost FB0.
        tya            ; Next cave row. 
        clc
        adc #CAVEW/8   ; Bytes per cave row.
        tay
        inx            ; Next framebuffer row.
        bne .shift
        ldx #24
        sec
        rts

        ; Bitmasks selecting left/right pixel based on start column (mod 8).
rmask   .byte #%00000001
lmask   .byte #%10000000, #%01000000, #%00100000, #%00010000
        .byte #%00001000, #%00000100, #%00000010, #%00000001

        ;     s  N  E  S  W
DeltaX  .byte 0, 0, 1, 0, -1
DeltaY  .byte 0, -1, 0, 1, 0
OppDir  .byte 0, #DIR_S, #DIR_W, #DIR_N, #DIR_E

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
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00000000
        .byte #%00000000

SpriteWalkF2
        .byte #%00000000
        .byte #%00010000
        .byte #%00011000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00000000
        .byte #%00000000

SpriteWalkF3
        .byte #%00000000
        .byte #%00001000
        .byte #%00011000
        .byte #%00010000
        .byte #%00010000
        .byte #%00011000
        .byte #%00010000
        .byte #%00010000
        .byte #%00000000
        .byte #%00000000

SpriteWalkF4
        .byte #%00000000
        .byte #%00010000
        .byte #%00011000
        .byte #%00001000
        .byte #%00001000
        .byte #%00001000
        .byte #%00001000
        .byte #%00001000
        .byte #%00000000
        .byte #%00000000

       ORG $F400
SpriteColor
        .byte #$56, #$26, #$25, #$82, #$37
        .byte #$26, #$25, #$82, #$33, #$56

PFCol
        .ds 10
        .byte $24, $26, $26, $24, $24, $22, $22, $20, $20, $20

        ORG $F500
CaveData
        #include "cave.s"

        ORG $FFFA

        .word Reset         ; NMI
        .word Reset         ; RESET
        .word Reset         ; IRQ

        END
