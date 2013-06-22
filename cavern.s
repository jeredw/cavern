; CAVERN. Copyright 2013 by Jered Wierzbicki.
; 4kB game for Atari 2600.
 
        processor 6502
        include "vcs.h"
        include "macro.h"

START_O2 = 20          ; Amount of oxygen at start of game.
START_X  = 29          ; Starting x coordinate.
START_Y  = 10          ; Starting y coordinate.
WIN_Y    = 8           ; Reaching this y coordinate wins.
SPRITEH  = 10          ; Height of sprite in scanlines.
DIR_STOP = 0           ; DIR when standing still.
DIR_U    = 1           ; DIR when climbing.
DIR_E    = 2           ; DIR when moving right.
DIR_D    = 3           ; DIR when moving down. TODO: Delete.
DIR_W    = 4           ; DIR when moving left.
SPEED    = 3           ; Frames between motion.
LINES    = 192         ;
CAVEW    = 128         ; Width of the cave in bits.
CAVEH    = 48          ; Height of the cave in rows.
FBW      = 32          ; Width of the framebuffer in bits.
FBH      = 16          ; Height of the framebuffer in rows.
ROWH     = LINES / FBH ; Scanlines per row.
PREVCOL  = -1          ; Offset in bytes to previous cave column.
NEXTCOL  = 1           ; Offset in bytes to next cave column.
PREVROW  = -CAVEW/8    ; Offset in bytes to previous cave row.
NEXTROW  = CAVEW/8     ; Offset in bytes to next cave row.

OXYMX    = 120         ; x coordinate of oxygen meter.

LSEDGE   = 16+31       ; x coordinate of left scroll edge.
TSEDGE   = 32          ; y coordinate of top scroll edge.
RSEDGE   = 160-16-32   ; x coordinate of right scroll edge.
BSEDGE   = 128-32      ; y coordinate of bottom scroll edge.

BUMPTIME = 17          ; How many 64us ticks to play impact sounds for.
STILL_O2 = 1           ; Oxygen burn rate for standing still.
WALK_O2  = 2           ; Oxygen burn rate for walking.
CLIMB_O2 = 3           ; Oxygen burn rate for climbing.
O2_RATE  = 5           ; MSB of oxygen counter when a bar expires.

MAGIC    = $89         ; This is a VERY magic number.
BORING   = $80         ; This is a less magic number.
TSTART   = 56          ; Sprite x,y on title screen.
TCOLOR   = $94         ; Color of title graphics.

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
WALLDIR ds 1           ; Dir of wall collision or 0.
MREFP0  ds 1           ; Copy of REFP0.
PFINV   ds 1           ; Playfield color outside visible part.
FING    ds 1           ; Set if falling and clear otherwise.
SPLASH  ds 1           ; If set, showing splash screen.

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
OXYGEN  ds 1
OXYCNT  ds 2
MAX_R   ds 1
        ds 20          ; Spare.

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

StartOfFrame
        VERTICAL_SYNC  ; Vertical retrace leaving A=0.

        ; Scroll the screen around the cave depending on player position.
        ; Leaves the number of remaining VBLANK scanlines in X.
        jmp Scroll

        ; Look at one cave square near the player to see if they should either
        ; fall or climb over a ledge and stop climbing.
ChkCave stx R3         ; This many lines of VBLANK remain.
        lda WALLDIR    ; Direction of most recent wall collision.
        beq Grav       ; If none, not climbing, so apply gravity.
        lda DIR        ;
        cmp #DIR_U     ; Moving up? 
        bne Grav       ; Not climbing, apply gravity.
        ; The player is climbing. Check the tile above and to direction
        ; WALLDIR to see if it is a ledge.
        lda SPRITE_Y   ; The player wins the game if they climb up
        cmp #WIN_Y     ; out of the cave, i.e. y is below WIN_Y.
Win     bcc Win        ; Cheesy win screen.
        sbc #3         ; Carry is set here.
        tay            ; Look at y coordinate sprite_y - 3.
        ldx WALLDIR    ; Get direction to wall being climbed.
        lda DeltaX,x   ; Load x delta to wall.
        asl            ; Multiply x delta by 4.
        asl            ;
        clc            ; Carry is set for x delta -1.
        adc SPRITE_X   ; Look at x coordinate sprite_x + 4*dx.
        tax            ; Put x coordinate in X.
        jsr GetBit     ; Check if a ledge.
        bcs Ground     ; If still a wall, keep climbing.
        jmp Ledge      ; Hop up onto the ledge.
        ; Get tile below current position.
Grav    ldx SPRITE_X   ; sprite_x is left of sprite, GetBit adjusts.
        ldy SPRITE_Y   ; sprite_y is below sprite.
        jsr GetBit     ; Get tile beneath sprite.
        bcs HitGrnd    ; If ground, the player hit ground.
        inc SPRITE_Y   ; Move the player down (gravity).
        lda #1         ; 
        sta WALKF      ; 
        sta FING       ; Flag that the player is falling.
        bne Ground
HitGrnd lda FING       ; Was the player falling?
        beq Ground     ; If not falling, 
        dec FING       ; Mark as not falling.
        sta AUDV1      ; Play a "thud" sound, since player just landed.
        lda #BUMPTIME  ; For some duration.
        sta TIM64T     ;
Ground  lda INTIM      ; See about turning off sounds.
        cmp #BUMPTIME  ; Done?
        bcc IntoVB     ; If not, jump into vblank.
StopSnd lda #0         ; Turn off sounds.
        sta AUDV1      ;
        sta AUDV0      ;
IntoVB  ldx R3         ; Get number of lines to blank.

        ; Draw 37 total scanlines of vertical blank.
        ; For the first 35, just blank.
VBlank  sta WSYNC
        dex
        bne VBlank

        ; Position sprite 0 (x=0).
        lda SPRITE_X
        jsr XPos       ; Note that x=0 here. Waits.
        clv
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
        bne Picture    ; 70 +3

; X is the framebuffer row number in 0..15.
; R1 is a one-hot counter which sets the carry bit every 8 cycles
; to trigger incrementing the framebuffer row.
.newr   nop            ; 43 +2
        sta PF2        ; 45 +3
        lda FB3,x      ; 48 +4
        inx            ; 52 +2
        cpx #16        ; 54 +2
        sta PF1        ; 56 +3
        beq Status     ; 59 +2
        lda #$80       ; 61 +2
        sta.w R1       ; 63 +4
        lda FB0,x      ; 67 +4
        dey            ; 71 +2
Picture sta PF1        ; 73 +3 Store PF1 data.
        bmi .nosp1     ; 0  +2 If y < 0, not in sprite.
        cpy #SPRITEH   ; 2  +2 Test y against sprite height.
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
        bvc Picture    ; 70 +3 Always taken.
.nosp1  SLEEP 4        ; 3  +4 Delay to match sprite lines.
.nosp2  cpy #20        ; 7  +2 20 pixels above sprite or less?
        bcs .bl        ; 9  +2 No, black out playfield.
        lda PFCol,y    ; 11 +4 Get color for this height.
        sta COLUPF     ; 15 +3 Set playfield color.
        bvc .pf        ; 18 +3 Always taken.
.bl     lda PFINV      ; 12 +3 Get invisible playfield color.
        sta COLUPF     ; 15 +3 Set playfield color.
        bvc .pf        ; 18 +3 Always taken.

; Draw status display beneath cave showing the amount of oxygen left.
Status  sta WSYNC      ; Wait for end of last cave line.
        ldx #0         ;
        stx REFP0      ; Do not reflect "O2" label.
        stx PF1        ; Don't show playfield.
        stx PF2        ;
        sta WSYNC      ; Boundary beneath cave.
        lda #OXYMX     ;
        jsr XPos       ; Position oxygen meter.
        sta WSYNC      ;
        ; Draw the label for the oxygen meter.
.lbl    lda OxyLab,x   ; Get row of "O2" bitmap.
        sta GRP0       ; Set row as sprite data.
        inx            ; Next bitmap row.
        sta WSYNC      ; Wait for end of line.
        cpx #7         ; Last line?
        bne .lbl       ; If not, draw another line.
        ; Draw a vertical oxygen meter then empty lines.
        ldx #192-#128+#8 ; This many total drawn lines remain.
        ldy OXYGEN     ; Get oxygen count in Y.
        iny            ; So dey can set flags.
.fill   dey            ; Count one more bar displayed.
        bmi .nox       ; Oxygen meter all drawn.
        txa            ; 
        and #3         ; Get line number (mod 4).
        cmp #1         ; If == 1, draw a tick mark,
        bne .nomark    ; else no mark.
        lda #%01110000 ; Bar with black tick mark.
        .byte $2c      ; Skip next instruction.
.nomark lda #%01111100 ; Full bar.
        sta GRP0       ; Set sprite data.
        bne .wait      ; Done with line.
.nox    lda #0         ; No sprite data to draw.
        sta GRP0       ; Set (empty) sprite data.
.wait   sta WSYNC      ; Wait for next line.
        dex            ; Count next line.
        bne .fill      ; If more lines remain, continue.
        lda MREFP0     ; Restore saved mirror state so
        sta REFP0      ; player is mirrored if appropriate.

        ; Begin line 1 of overscan.
        sta WSYNC

        ; Black the picture.
        lda #%01000010 ; Black and latch input.
        sta VBLANK     ; Turn on blacking.

        ; 30 total lines of overscan.
        ; Take off lines as needed for logic and add below.
        ldx #26
Overscan
        dex            ; Count line.
        sta WSYNC      ; Start new line.
        bne Overscan

        ; Line 27. Update oxygen supply counters.
        lda SCROLL_R   ; Get current top row of map.
        cmp MAX_R      ; Compare top row to max top row seen.
        bcc .deco2     ; If cur top not a new max, skip down.
        beq .deco2     ;
        sta MAX_R      ; Set the new maximum top row.
        inc OXYGEN     ; There is more oxygen deeper in the cave.
.deco2  lda DIR        ; 
        cmp #DIR_STOP  ; Is the player moving?
        beq .o2nom     ; No, consume the normal breathing rate.
        cmp #DIR_U     ; Is the player climbing?
        beq .o2cli     ; Climbing, so use a lot of oxygen.
        lda #WALK_O2   ; Consume walking oxygen.
        .byte $2c      ; Skip next instruction.
.o2nom  lda #STILL_O2  ; Consume standing oxygen.
        .byte $2c      ; Skip next instruction.
.o2cli  lda #CLIMB_O2  ; Consume climbing oxygen.
        clc            ; Set by cmp.
        adc OXYCNT     ; Add consumption to oxygen LSB.
        sta OXYCNT     ; Store oxygen LSB.
        txa            ; Carry into MSB (X is zero.)
        adc OXYCNT+1   ; Add consumption to oxygen MSB.
        sta OXYCNT+1   ; Store oxygen MSB.
        cmp #O2_RATE   ; Check MSB against burn counter.
        bcc .o2done    ; If MSB less than 
        lda OXYGEN     ; Check oxygen.
        beq Reset2     ; If zero, player is dead.
        dec OXYGEN     ; Use up one bar of oxygen.
        lda #%0010     ; Play a blip sound.
        sta AUDV0      ;
        lda #BUMPTIME  ;
        sta TIM64T     ;
        stx OXYCNT     ; Zero the oxygen counter.
        stx OXYCNT+1   ;
.o2done sta WSYNC      ;

        ; Line 28. Check for collisions.
ChkColl lda CXP0FB     ; Read collision register.
        bpl .cdone     ; If player did not collide with PF, ok.
        lda DIR        ; Get last travel direction.
        cmp #DIR_U     ; Climbing?
        bne .bump      ; Bumped against wall.
        inc SPRITE_Y   ; If climbing, must have hit head.
        bne .cdone     ;
.bump   cmp #DIR_W     ; Moving left?
        bne .bumpw     ; If not, must be right.
        inc SPRITE_X   ; Bump back to the right.
        .byte $2c      ; Skip next instruction.
.bumpw  dec SPRITE_X   ; Bump back to the left.
        sta WALLDIR     ; Save the collision direction.
        lda #DIR_STOP  ;
        sta DIR        ; Stop moving.
        sta CXCLR      ; Flag collision cleared.
        beq .rstep     ; Reset step counter.
.cdone  sta CXCLR      ; Flag collision cleared.
        sta WSYNC      ; Wait for rest of line.
        
        ; Line 29. Decode direction from joystick.
        ; On entry, X is DIR_STOP=0.
DecodeDir
        lda #$10       ; Check bit D4.
        bit SWCHA      ; Test input latch.
        bpl .right     ; D7: Right joystick switch closed.
        bvc .left      ; D6: Left joystick switch closed.
        bne .stodir    ; If D4 set, not pressing up.
.up     lda WALLDIR    ; Against a wall?
        beq .stodir    ; If not, can't climb.
        ldx #DIR_U     ; Set new dir as up.
        bne .stodir    ; Always taken.
.left   lda #%1000     ;
        sta REFP0      ; Mirror sprite.
        sta MREFP0     ; Save mirror sprite.
        ldx #DIR_W     ; Moving left.
        bne .stodir    ; Always taken.
.right  stx REFP0      ; Do not mirror sprite.
        stx MREFP0     ; Save mirror state.
        ldx #DIR_E     ; Moving right.
.stodir stx DIR        ; Store dir.
        cpx #DIR_STOP  ; Moving?
Step    bne .step      ; If moving, count steps.
.rstep  lda #SPEED+1   ; Not moving. Reset step counter.
        sta STEPC      ;
.step   dec STEPC      ; Count step.
        sta WSYNC      ;

        jmp Animate

; Initialize machine state.
Reset   SUBROUTINE
        clc
Reset2  CLEAN_START
        sta SWACNT     ; Configure PORT A as input.

        lda #%0100
        sta AUDC0
        lda #%0111
        sta AUDC1
        lda #%01111
        sta AUDF0
        sta AUDF1
        lda #<SpriteWalkF1
        sta SPRITE_P
        lda #>SpriteWalkF1
        sta SPRITE_P+1
        ; D0: REF=1, reflect playfield.
        ; D1: SCORE=0, do not color left/right differently.
        ; D2: PFP=0, player is in front of pf.
        ; D4-D5: BALL SIZE 0, 1 clock.
        lda #%00001
        sta CTRLPF

        lda #START_O2
        sta OXYGEN
        lda #START_X
        sta SPRITE_X
        lda #START_Y
        sta SPRITE_Y
        ; This is fixed to zero.
        ;lda #<CaveData
        ;sta CAVE_P
        lda #>CaveData
        sta CAVE_P+1
        bcs .blit

        lda #<Title
        sta CAVE_P
        lda #>Title
        sta CAVE_P+1
        lda #TCOLOR
        sta PFINV
        lda #TSTART
        sta SPRITE_X
        sta SPRITE_Y
        stx OXYGEN
        lda #O2_RATE-1
        sta OXYCNT+1

.blit   jsr CopyRow
        jsr StorRow
        tya
        clc
        adc #CAVEW/8
        tay
        inx
        cpx #FBH
        bne .blit

        jmp StartOfFrame

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
        tay            ;
        clc            ; +2
        adc SPRITE_X   ; +3 Add to x coordinate.
        sta SPRITE_X   ; +3
        cpy #0
        beq .movey
        lda #0
        sta WALLDIR
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

; Lift up onto a ledge and stop climbing.
; This chunk of code is here so the picture loop is on one page.
Ledge   ldx WALLDIR    ; Get the direction to the wall.
        stx DIR        ; Make the player face that way.
        lda DeltaX,x   ; Get delta to wall.
        asl            ; dx*2
        clc            ; Carry was set if -1.
        adc SPRITE_X   ; Move player to left/right edge of ledge.
        sta SPRITE_X   ; Update x coord.
        lda SPRITE_Y   ; Get sprite y (halfway up).
        sec            ; 
        sbc #3         ; Move the rest of the way up the wall.
        sta SPRITE_Y   ;
        lda #0         ; No longer against a wall.
        sta WALLDIR    ;
        jmp Ground

; Moves the screen around the cave based on the player's position and motion.
;
; The screen scrolls if possible when the player sprite is up against an
; invisible bounding box about thirty pixels from its edges, leaving the player
; sprite in the same relative position. So the player can only go outside this
; box at the extreme edges of the cave.
;
; This is located here to keep the Picture loop on one page.
Scroll  SUBROUTINE
        ldx #35        ; 35 scanlines of blanking.
        lda DIR        ; Get current motion direction.
        cmp #DIR_E     ; Moving east?
        bne .checkw    ; No, check west.
        lda SPRITE_X   ; 
        cmp #RSEDGE    ; Is sprite X >= the scroll boundary?
        bcc .checku    ; No, check up.
        jsr PanRight   ; Scroll right if possible.
        bcc .checku    ; Could not scroll.
        lda #RSEDGE-4  ; Move sprite left one tile,
        sta SPRITE_X   ; to keep it in the same relative spot.
        bne .checku    ; Always taken.
.checkw cmp #DIR_W     ; Moving west?
        bne .checku    ; No, check up.
        lda SPRITE_X   ; 
        cmp #LSEDGE    ; Is sprite X < the scroll boundary?
        bcs .checku    ; No, check up.
        jsr PanLeft    ; Scroll left if possible.
        bcc .checku    ; Could not scroll.
        lda #LSEDGE+3  ; Move the sprite right half a tile.
        sta SPRITE_X   ; Fall through to up/down scrolling check.
.checku lda DIR        ; Get motion direction.
        cmp #DIR_U     ; Moving up?
        bne .checkd    ; No, check down i.e. falling.
        lda SPRITE_Y   ;
        cmp #TSEDGE    ; Is sprite Y < the scroll boundary?
        ldx #34        ;   (34 lines of blanking remain.)
        bcs .out       ; No. Also don't scroll down while climbing.
        jsr PanUp      ; Scroll up.
        bcc .out       ; Couldn't scroll.
        lda SPRITE_Y   ; Scrolled up, move sprite down
        clc            ; to keep it in the same relative position.
        adc #8         ;
        sta SPRITE_Y   ; 
        bne .out       ; Always taken.
.checkd lda SPRITE_Y   ; 
        cmp #BSEDGE    ; Is sprite Y >= the scroll boundary?
        bcc .out       ; No, done.
        ldx #34        ;   (34 lines of blanking remain.)
        jsr PanDown    ; Scroll down.
        bcc .out       ; Couldn't scroll.
        lda SPRITE_Y   ; Scrolled down, move sprite up
        sec            ; to keep it in the same relative position.
        sbc #8         ;
        sta SPRITE_Y   ;
.out    jmp ChkCave    ; Next, check nearby cave conditions.

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

; Positions object X in column A.
; Note: .div15 and branch must be on the same page.
XPos    SUBROUTINE
        sec            ; 2
        sta WSYNC      ; 3, line 1
.div15  sbc #15        ; 2
        bcs .div15     ; 54 max
        eor #7         ; 2
        asl            ; 2
        asl            ; 2
        asl            ; 2
        asl            ; 2
        sta HMP0,x     ; 4       68
        sta RESP0,x    ; 4       72
        rts            ; 6

; Gets the framebuffer bit at screen position X,Y.
; Carry is set if bit is set, zero otherwise.
GetBit  SUBROUTINE
        lda MREFP0     ; Load sprite mirror state.
        beq .noinc     ; If not mirrored, use given x.
        inx            ; Else player is actually at x+1.
.noinc  txa            ; Get x coordinate in A.
        sec
        sbc #16        ; Subtract PF0 and offset center of sprite.
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

; Reverses the bits in A (65 cycles).
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
; Assumes carry is set properly on entry.
AddCave SUBROUTINE
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
        clc            ; Assume did not scroll.
        beq .out       ; If equal, do not pan down.
        inc SCROLL_R   ; Pan down.
        lda <#NEXTROW  ; Offset of next bitmap row.
        ldx >#NEXTROW  ;
        ; Carry is clear because of clc above.
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
        ldx #22
        sec            ; Scrolled.
.out    rts

; Pans the screen up by one row.
PanUp   SUBROUTINE
        lda SCROLL_R   ; Get start screen row.
        clc            ; Assume did not scroll.
        beq .out       ; If zero, do not pan up.
        dec SCROLL_R   ; Pan up.
        lda #<PREVROW  ; Offset of previous bitmap row.
        ldx #>PREVROW  ;
        ; Carry is clear because of clc above.
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
        ldx #22
        sec            ; Scrolled.
.out    rts

; Pans the screen left by one column.
PanLeft SUBROUTINE
        lda SCROLL_C   ; Get screen start column.
        clc            ; Assume didn't scroll.
        beq .out       ; If not at 0, scroll.
        dec SCROLL_C   ; Pan left.
        lda SCROLL_C   ; Get new left column.
        and #7         ; Get start pixel of left column.
        sta R1         ; Stash it.
        cmp #7         ; Pixel 7 implies a new leftmost column byte.
        bne .ptrok     ; Else current cave ptr is ok.
        lda <#PREVCOL-1; Offset to previous cave byte,
        ldx >#PREVCOL  ; including carry.
        ; Carry is set because A >= 7, so use PREVCOL-1.
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
.out    rts

; Pans the screen right by one column.
PanRight SUBROUTINE
        lda SCROLL_C   ; Get screen start column.
        cmp #CAVEW-#FBW ; Test against rightmost column.
        clc            ; Assume did not scroll.
        beq .out       ; If at rightmost column don't pan right.
        inc SCROLL_C   ; Else pan right.
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
        ; Carry is clear from clc above.
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
        cmp #MAGIC     ; Is it magic?
        bne .mask      ; If not magic, interpret normally.
        sta AUDV0      ; Turn on sound volume.
        tya            ; Get a reasonable timeout value (3 or 4).
        sta TIM64T     ; Start timer.
        lda #BORING    ; Cave in the wall so right side is passable.
.mask   and R1         ; Mask the newly visible bit.
        cmp R1         ; Set carry if bit is set, else clear.
        ror FB3+16,x   ; Carry goes into rightmost pixel (bit 7).
        rol FB2+16,x   ; Leftmost FB3 -> rightmost FB2.
        ror FB1+16,x   ; Leftmost FB2 -> rightmost FB1.
        rol FB0+16,x   ; Leftmost FB1 -> rightmost FB0.
        tya            ; Next cave row. 
        clc            ;
        adc #CAVEW/8   ; Bytes per cave row.
        tay            ;
        inx            ; Next framebuffer row.
        bne .shift     ;
        ldx #23        ; This many scanlines of VBLANK remain.
        sec            ; Flag that scrolling happened.
.out    rts

; Bitmasks selecting left/right pixel based on start column (mod 8).
rmask   .byte #%00000001
lmask   .byte #%10000000, #%01000000, #%00100000, #%00010000
        .byte #%00001000, #%00000100, #%00000010, #%00000001

; The label for the oxygen level meter.
OxyLab  .byte #%01000000
        .byte #%10101100
        .byte #%10100010
        .byte #%10100100
        .byte #%01001000
        .byte #%00001110
        .byte #%00000000

; Deltas to move by one step in each direction.
;             s  U  E  D  W
DeltaX  .byte 0, 0, 1, 0, -1
DeltaY  .byte 0, -1, 0, 1, 0

; Pointers to walk/climb animation frames.
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

; One color per scanline of sprite.
; First color byte is not shown, but is used for oxygen meter.
SpriteColor
        .byte #$44, #$26, #$25, #$82, #$37
        .byte #$26, #$25, #$82, #$33, #$56

; Playfield colors relative to player sprite position.
PFCol   EQU . - 10
        .byte $24, $26, $26, $24, $24, $22, $22, $20, $20, $20

        ORG $F500
; The game map.
CaveData
        #include "map.s"

; The title screen, in the lower right of the cave bitmap.
Title   EQU  CaveData + (CAVEW*(CAVEH-FBH) + (CAVEW-FBW))/8

        ORG $FFFA

        .word Reset         ; NMI
        .word Reset         ; RESET
        .word Reset         ; IRQ

        END
