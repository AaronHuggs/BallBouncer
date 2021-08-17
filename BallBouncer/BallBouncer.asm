.segment "HEADER"
    .byte "NES"
    .byte $1a
    .byte $02
    .byte $01
    .byte %00000000
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00,$00,$00,$00,$00

.segment "STARTUP"
.segment "ZEROPAGE"
;------------------VARIABLES----------------------------
SPEED = 1 ;Constant variable for speed of sprite
buttons: .res 1
backgroundselect: .res 2
spriteselect: .res 2


STATETITLE = 0 ; displaying title screen
STATEPLAYING = 1 ; move player/ball, check for collisions
STATEPAUSED = 2 ; wait for player to press start again
STATEGAMEOVER = 3 ; displaying game over screen

gamestate: .res 1 ;stores current game state
ballx: .res 1 ;ball horizontal position
bally: .res 1 ;ball vertical position
ballup: .res 1 ;1 = ball moving up
balldown: .res 1 ;1 = ball moving down
ballleft: .res 1 ;1 = ball moving left
ballright: .res 1 ;1 = ball moving right
ballspeedx: .res 1 ;ball horizontal speed per frame
ballspeedy: .res 1 ; ball vertical speed per frame
scoreOnes: .res 1 ;byte for each digit in the decimal score
scoreTens: .res 1
scoreHundreds: .res 1

RIGHTWALL = $E9 ;Define boundaries
TOPWALL = $0F
BOTTOMWALL = $D9
LEFTWALL = $10

PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007
OAMDMA = $4014
APUSTATUS = $4015
SQ1_ENV = $4000
SQ1_LO = $4002
SQ1_HIGH = $4003

background_change: .res 1

;create variable with variableName .res #ofbytes (speed .res 1)
;-------------------------------------------------------
.segment "CODE"
;------------------SETUP CODE----------------------------
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
  rts
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40	
  STX $4017    ; disable APU frame IRQ
  LDX #$FF	
  TXS          ; Set up stack
  INX          ; now X = 0
  STX PPUCTRL    ; disable NMI
  STX PPUMASK    ; disable rendering
  STX $4010    ; disable DMC IRQs

  jsr vblankwait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
   
  jsr vblankwait

setupFunctions:
  jsr SetupAudio
  jsr SetupPalettes
  jsr SetupInitialStats
  jsr LoadTitleBackground
  jsr LoadAttribute
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA PPUCTRL
  LDA #%00001010   ; disable sprites, enable background, enable background on left side
  STA PPUMASK
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA PPUSCROLL
  STA PPUSCROLL

Forever: ;wait for vblank interrupt
  JMP Forever  

;----------------SETUP COMPLETE-------------------------------

;===================GAME CODE===================================
VBLANK: ;Runs every frame
  jsr BeginDMATransfer ;write sprites to PPU
  jsr GameEngine ;Run game engine
  ;If in the title screen, skip checking controllers and updating ppu
  lda gamestate
  cmp #STATETITLE
  beq StartScreen
  jsr ReadControllers
  jsr PPUCleanup
  StartScreen:

  rti
;==================END OF CODE==================================

;==================FUNCTIONS=====================================
SetupAudio: ;----------------------------------------------------
  lda #%00000001 ; enable square 1, disable others
  sta APUSTATUS

  lda #%10111111; Duty 10 (50%), volume F (max!)
  sta $4000
;---------------------------------------------------------------
SetupPalettes: ;----------------------------------------------
  ; Setup palettes
  lda PPUSTATUS ;Read PPU status to reset high/low byte latch to high
  lda #$3F
  sta PPUADDR ; store high byte of $3F00 address
  lda #$00
  sta PPUADDR ; store low byte of $3F00 address

  ;loop through the palette data to store it all into the ppu
  ldx #$00 ;start at 0
  LoadBackgroundPaletteLoop:
    lda background_palette, x ; load from address PaletteData+x 
    sta PPUDATA ; ppu data register
    inx ;increment x
    cpx #$10 ;compare x to decimal 16
    bne LoadBackgroundPaletteLoop

  ldx #$00
  LoadSpritePaletteLoop:
    lda sprite_palette, X
    sta PPUDATA
    inx
    cpx #$10
    bne LoadSpritePaletteLoop

  ; Palettes are ready!
  lda #$23
  sta background_change
  rts 
;---------------------------------------------------------------
SetupInitialStats: ;--------------------------------------------
  ;Initial ball stats
  lda #$01 ;start the ball moving down and right
  sta balldown
  sta ballright
  lda #$00
  sta ballup
  sta ballleft
 ;test
  lda #$30
  sta bally ;starting location of the ball

  lda #$90
  sta ballx

  lda #$02 ;ball speed
  sta ballspeedx
  sta ballspeedy

  ;Initial score value
  lda #$00
  sta scoreOnes
  sta scoreTens
  sta scoreHundreds

  ;set starting game state
  lda #STATETITLE
  sta gamestate

  rts
;---------------------------------------------------------------
LoadTitleBackground: ;-----------------------------------------------
  lda PPUSTATUS ;read PPU status to reset the high/low latch
  lda #$20 
  sta PPUADDR ;write the high byte of $2000 address
  lda #$00
  sta PPUADDR ;write the low byte of $2000 address

  lda #<TitleBackground
  sta backgroundselect
  lda #>TitleBackground
  sta backgroundselect+1
  
  ldx #$00 ;start out at 0
  ldy #$00
  LoadTitleBackgroundLoop:
    lda (backgroundselect),y
    sta PPUDATA
    iny
    cpx #$03
    bne :+
    cpy #$C0
    beq DoneLoadingTitleBackground
    
  :
    cpy #$00
    bne LoadTitleBackgroundLoop
    inx
    inc backgroundselect+1
    jmp LoadTitleBackgroundLoop
  DoneLoadingTitleBackground:
    rts
;--------------------------------------------------------------
LoadPlayingBackground: ;-----------------------------------------------
  lda PPUSTATUS ;read PPU status to reset the high/low latch
  lda #$24
  sta PPUADDR ;write the high byte of $2000 address
  lda #$00
  sta PPUADDR ;write the low byte of $2000 address

  lda #<PlayingBackground
  sta backgroundselect
  lda #>PlayingBackground
  sta backgroundselect+1
  
  ldx #$00 ;start out at 0
  ldy #$00
  LoadPlayingBackgroundLoop:
    lda (backgroundselect),y
    sta PPUDATA
    iny
    cpx #$03
    bne :+
    cpy #$C0
    beq DoneLoadingPlayingBackground
    
  :
    cpy #$00
    bne LoadPlayingBackgroundLoop
    inx
    inc backgroundselect+1
    jmp LoadPlayingBackgroundLoop
  DoneLoadingPlayingBackground:

    rts
;--------------------------------------------------------------
LoadAttribute:
  lda PPUSTATUS
  lda #$23
  sta PPUADDR ;write the high byte of $23C0 address
  lda #$C0
  sta PPUADDR ;write the low byte of $23C0 address

  ldx #$00
  LoadAttributeLoop:
    lda attribute,x
    sta PPUDATA
    inx 
    cpx #$40
    bne LoadAttributeLoop

  rts
;---------------------------------------------------------------
LoadSprites: ;---------------------------------------
  ldx #$00
  LoadSpritesLoop:
    lda sprites, x
    sta $0200, x
    inx
    cpx #$24 ; 8 player sprites, 1 ball. 9 x 8 = $24
    bne LoadSpritesLoop
  ; Sprites are ready!
  rts
;------------------------------------------------------
BeginDMATransfer: ;-------------------------------------
  lda #$00
  sta OAMADDR ;Set low byte (00) of sprite RAM address
  lda #$02
  sta OAMDMA ;Set high byte (02) of sprite RAM address and begin DMA transfer
  rts
;--------------------------------------------------------
DrawScore: ;---------------------------------------------
  lda PPUSTATUS
  lda #$20
  sta PPUADDR
  lda #$42
  sta PPUADDR ;start drawing score at ppu $2020

  lda scoreHundreds ;get first digit
  sta PPUDATA ;draw to background
  lda scoreTens ;next digit
  sta PPUDATA
  lda scoreOnes ;last digit
  sta PPUDATA

  rts

;--------------------------------------------------------
DrawBall: ;----------------------------------------------
  ;DrawBall simply gets the current position of the ball
  ;and draws the sprite there.
  ;select ball y position
    lda #$20
    sta spriteselect
    lda #$02
    sta spriteselect+1
    ;store current y position
    lda bally
    ldy #$00
    sta (spriteselect),y
    ;store current x position
    lda ballx
    ldy #$03
    sta (spriteselect),y
  rts
;--------------------------------------------------------
ReadControllers: ;---------------------------------------
  lda #$01
  sta $4016
  lda #$00
  sta $4016 ; tell both the controllers to latch buttons

  ldx #$08 ; loop 8 times for all 8 buttons
  ReadControllerLoop:
    lda $4016 ;get current buttons
    lsr a ;move bit 0 to carry
    rol buttons ;move carry into buttons register
    dex ;decrease x
    bne ReadControllerLoop
  
  ReadA:

  ReadB:

  ReadSelect:
  ReadSelectDone:

  ReadStart:

  ReadStartDone:
  ReadUp:
    lda buttons
    and #%00001000 ;bit 4 = up
    beq ReadUpDone ;branch if button is NOt pressed
    ;get sprite info
    lda #$00
    sta spriteselect
    lda #$02
    sta spriteselect+1
    MoveUp:
      ldx #$00
      ldy #$00
      MoveUpLoop:
        lda (spriteselect), y ;get y position of current sprite
        sec ;set carry
        sbc #SPEED ;subtract speed from y position
        tax ;begin checking collision
        sec
        sbc #TOPWALL ;walls+1 is top wall
        bcc MoveUpDone ;if sprite will hit the wall, don't move

        txa ;return value to A
        sta (spriteselect), y ;store new y position

        tya ;add 4 to see if there's another sprite to move
        clc
        adc #$04
        tay
        cmp #$20 ;Number of sprites * 4 
        bcc MoveUpLoop
      MoveUpDone:
  ReadUpDone:

  ReadDown:
    lda buttons
    and #%00000100 ;bit 3 = down
    beq ReadDownDone ;branch if button is NOt pressed
    ;get sprite information
    lda #$00
    sta spriteselect
    lda #$02
    sta spriteselect+1
    MoveDown:
      ldx #$00
      ldy #$1C ;number of sprites * 4 - 3
      MoveDownLoop:
        lda (spriteselect), y ;get y position of current sprite
        clc ;clear carry
        adc #SPEED ;add SPEED to y position
        tax ;save current value of A to X
        lda #BOTTOMWALL ; walls + 2 is the bottom wall
        sbc (spriteselect), y ;boundary - current position
        beq MoveDownDone ;if the sprite moving SPEED in this direction is beyond the boundary, skip the move
        
        txa ;get the value of A back from X
        sta (spriteselect), y ;store new y position

        tya ;subtract 4 from Y and see if there's another sprite to move.
        sec
        sbc #$04
        tay
        bcs MoveDownLoop
      MoveDownDone:

  ReadDownDone:

  ReadLeft:
    lda buttons 
    and #%00000010 ; bit 1 = left
    beq ReadLeftDone ;branch if button is NOT pressed
    ;Get sprite info
    lda #$03
    sta spriteselect
    lda #$02 ;Sprite stored at $0200+
    sta spriteselect+1

    MoveLeft:

      ldx #$00
      ldy #$00
      
      MoveLeftLoop:
        lda (spriteselect), y ;get x position of current sprite
        sec ;set carry
        sbc #SPEED ;subtract SPEED from x position
        tax ;save current value of A to X
        sec ;set carry
        sbc #LEFTWALL ;subtract boundary
        bcc MoveLeftDone ;if the sprite moving SPEED in this direction is beyond the boundary, skip the move
        
        txa ;get the value of A back from X
        sta (spriteselect), y ;store new x position

        tya ;Add 4 to Y and see if there's another sprite to move.
        clc
        adc #$04
        tay
        cmp #$20 ;Number of sprites * 4 
        bcc MoveLeftLoop
      MoveLeftDone:

  ReadLeftDone:
  
  ReadRight:
    lda buttons
    and #%00000001 ; bit 0 = right
    beq ReadRightDone ;branch if button is NOT pressed
    ;Get sprite info
    lda #$03
    sta spriteselect
    lda #$02 ;Sprite stored at $0200+
    sta spriteselect+1

    MoveRight:

      ldx #$00
      ldy #$1C ;Number of sprites * 4 - 3
      
      MoveRightLoop:
        lda (spriteselect), y ;get x position of current sprite
        clc ;clear carry
        adc #SPEED ;add SPEED to x position
        tax ;save current value of A to X
        lda #RIGHTWALL
        sbc (spriteselect), y ;boundary - current position
        beq MoveRightDone ;if the sprite moving SPEED in this direction is beyond the boundary, skip the move
        
        txa ;get the value of A back from X
        sta (spriteselect), y ;store new x position

        tya ;subtract 4 from Y and see if there's another sprite to move.
        sec
        sbc #$04
        tay
        bcs MoveRightLoop
      MoveRightDone:
      

  ReadRightDone:
  
  rts
;-------------------------------------------------------
PPUCleanup: ;-------------------------------------------
  ;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA PPUCTRL
  LDA #%00011010   ; enable sprites, enable background, enable background on left side
  STA PPUMASK
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA PPUSCROLL
  STA PPUSCROLL
  

  rts
;------------------------------------------------------------
IncrementScore: ;--------------------------------------------
  IncOnes: 
    lda scoreOnes ;load the lowest digit of the number
    clc
    adc #$01 ;add 1
    pha
   
  ; Change Background Color
  lda PPUSTATUS ;Read PPU status to reset high/low byte latch to high
  lda #$3F
  sta PPUADDR ; store high byte of $3F10 address
  lda #$10
  sta PPUADDR ; store low byte of $3F10 address
  lda background_change
  adc scoreOnes
  sta PPUDATA
    
    pla
    sta scoreOnes
    cmp #$0A ;check if it overflowed, now equals 10
    bne IncDone ;if there was no overflow, all done
  IncTens:
    lda #$00
    sta scoreOnes
    lda scoreTens ;wrap digit to 0, load the next digit
    clc
    adc #$01 ;add one, the carry from previous digit
    sta scoreTens
    cmp #$0A
    bne IncDone
  IncHundreds:
    lda #$00
    sta scoreTens
    lda scoreHundreds
    clc
    adc #$01
    sta scoreHundreds
  IncDone:
  rts
;------------------------------------------------------------
GameEngine: ;------------------------------------------------
  lda gamestate
  cmp #STATETITLE
  beq EngineTitle ;game is displaying title screen

  lda gamestate
  cmp #STATEGAMEOVER
  beq EngineGameOver ;game is displaying ending screen

  lda gamestate
  cmp #STATEPLAYING
  beq EnginePlaying ;game is playing

;-----------------------------------------------------------
GameEngineDone: ;-------------------------------------------
  jsr DrawBall
  
  rts
;----------------------------------------------------------
EngineTitle: ;----------------------------------------------
  ;if start button pressed
  ReadStartButton:
    lda #$01
    sta $4016
    lda #$00
    sta $4016 ; tell both the controllers to latch buttons

    ldx #$08 ; loop 8 times for all 8 buttons
    ReadControllerTitle:
      lda $4016 ;get current buttons
      lsr a ;move bit 0 to carry
      rol buttons ;move carry into buttons register
      dex ;decrease x
      bne ReadControllerTitle
    ReadStartTitle:
      lda buttons
      and #%00010000 ;bit 5 = start
      beq NoStart ;branch if button is NOT pressed

      ;turn screen off
      LDX #%00   ; disable sprites(5), disable background(4), enable background on left side
      STX PPUMASK
      ;load game screen
      jsr LoadPlayingBackground
      ;set starting player/ball position
      jsr LoadSprites
      ;go to playing state
      lda #STATEPLAYING
      sta gamestate
      ;turn screen on
      LDA #%00011010   ; enable sprites(5), enable background(4), enable background on left side
      STA PPUMASK
  
  jmp GameEngineDone
  NoStart:
    rts
;-----------------------------------------------------------
EngineGameOver: ;--------------------------------------------
  ;if start button pressed
  ;turn screen off
  ;load title screen
  ;go to title state
  ;turn screen on
  jmp GameEngineDone
;-----------------------------------------------------------
EnginePlaying: ;-------------------------------------------
  
  MoveBall:
    MoveBallRight:
      lda ballright
      beq MoveBallRightDone ;if ballright = 0, skip this

      lda ballx
      clc
      adc ballspeedx ;ballx position = ballx + ballspeedx
      sta ballx

      lda ballx
      cmp #RIGHTWALL
      bcc MoveBallRightDone ;if ball x < right wall, still on screen, skip
      lda #$00
      sta ballright
      lda #$01
      sta ballleft ;bounce, ball now moving left
      jsr PlayBonk
      MoveBallRightDone:

    MoveBallLeft:
      lda ballleft
      beq MoveBallLeftDone ;if ballleft=0, skip

      lda ballx
      sec
      sbc ballspeedx ;ballx = ballx - ballspeedx
      sta ballx

      lda ballx
      cmp #LEFTWALL ;if ball x > left wall, still on screen, don't bounce
      bcs MoveBallLeftDone
      lda #$01
      sta ballright
      lda #$00
      sta ballleft ;bounce, ball now moving right
      jsr PlayBonk
      MoveBallLeftDone:

    MoveBallUp:
      lda ballup
      beq MoveBallUpDone ;if ballup = 0, skip this

      lda bally
      sec
      sbc ballspeedy ;bally -= ballspeedy
      sta bally

      lda bally 
      cmp #TOPWALL
      bcs MoveBallUpDone ;if bally > top wall, don't bounce
      lda #$01
      sta balldown
      lda #$00
      sta ballup ;bounce, ball now moving down
      jsr PlayBonk
      MoveBallUpDone:

    MoveBallDown:
      lda balldown
      beq MoveBallDownDone

      lda bally
      clc
      adc ballspeedy
      sta bally

      lda bally
      cmp #BOTTOMWALL
      bcc MoveBallDownDone
      lda #$00
      sta balldown
      lda #$01
      sta ballup
      jsr PlayBonk
      MoveBallDownDone:
  
  CatchBall:
    ;Check to see if the player catches the ball
    ;if so, move ball to different location
    ;and add a point.
    
    ;get player sprite 0 x position
    lda #$00 ;sprite 0
    sta spriteselect
    lda #$02
    sta spriteselect+1

    ldy #$03 ;x position of sprite
    lda (spriteselect),Y
    
    ;compare to ball x position
    cmp ballx

    ;if sprite 0 x > ballx, skip to the end
    bcs CatchBallDone

    ;get player sprite 1 x position
    lda #$04 ;sprite 1
    sta spriteselect
    lda #$02
    sta spriteselect+1

    ldy #$03 ;x position of sprite
    lda (spriteselect),y

    cmp ballx
    ;if sprite 1 x < ballx, skip to the end
    bcc CatchBallDone
    
    ;get player sprite 0 y position
    lda #$00 ;sprite 0
    sta spriteselect
    lda #$02
    sta spriteselect+1

    ldy #$00 ;y position of sprite
    lda (spriteselect),Y

    cmp bally
    ;if sprite 0 y > bally, skip to the end
    bcs CatchBallDone

    ;get player sprite 8 y position
    lda #$1C ;sprite 8
    sta spriteselect
    lda #$02
    sta spriteselect+1

    ldy $00 ;y position of sprite
    lda (spriteselect),y

    cmp bally
    ;if sprite 8 y < bally, skip to the end
    bcc CatchBallDone

    ;if it hasn't skipped, ball must be within the player sprite
    ;play beep
    jsr PlayBeep

    ;move ball somewhere else 
    jsr ResetBall
    ;increase score
    jsr IncrementScore

    CatchBallDone:
  
  jsr DrawScore
  jmp GameEngineDone

  PlayBeep:
    lda #%00000001 ; enable square 1, disable others
    sta APUSTATUS

    lda #%10011111; Duty 1,0 = 50%. 0 = length counter disable off. 1 = Saw envelope disable on. 1111 = Max volume (F). 
    sta $4000
    lda #$C9
    sta SQ1_LO
    lda #$00
    sta SQ1_HIGH
    rts

  PlayBonk:
    lda #%00000001 ; enable square 1, disable others
    sta APUSTATUS

    lda #%00011111; 
    sta $4000
    lda #$C4
    sta SQ1_LO
    lda #$00
    sta SQ1_HIGH
    rts

  ResetBall:
    ;get a value for new ball position that is within the boundaries
    ;get random player sprite position
    ldy #$00
    lda (spriteselect),y
    asl A
    asl A
    tax ;transfer a to x
    stx ballx

    ldy #$03
    lda (spriteselect),Y
    asl A
    asl a 
    tax
    stx bally

    rts

    ;multiply the position
    ;set ballx and bally to new position


;----------------------------------------------------------
EnginePaused: ;-----------------------------------------------
  ;paused after player pushes start button
  ;wait until player presses start again
  ;exit paused state
  ;if start button pressed
  ReadPauseButton:
    lda #$01
    sta $4016
    lda #$00
    sta $4016 ; tell both the controllers to latch buttons

    ldx #$08 ; loop 8 times for all 8 buttons
    ReadControllerPause:
      lda $4016 ;get current buttons
      lsr a ;move bit 0 to carry
      rol buttons ;move carry into buttons register
      dex ;decrease x
      bne ReadControllerPause
    ReadStartPause:
      lda buttons
      and #%00010000 ;bit 5 = start
      beq NoStartPause ;branch if button is NOT pressed
    lda #STATEPLAYING
    sta gamestate
  jmp GameEngineDone
  NoStartPause:
    rts
;-----------------------------------------------------------

;===================END OF FUNCTIONS==============================

;=================Palette colorsets============================
background_palette:
  .byte $22,$27,$17,$0F	;background palette 1 starting at $3F00
  .byte $22,$14,$17,$0F	;background palette 2
  .byte $22,$15,$21,$0F	;background palette 3
  .byte $22,$27,$17,$0F	;background palette 4
  
sprite_palette:
  .byte $22,$27,$35,$0F	;sprite palette 1 starting at $3F10
  .byte $22,$27,$17,$0F	;sprite palette 2
  .byte $22,$1C,$15,$14	;sprite palette 3
  .byte $22,$02,$38,$3C	;sprite palette 4
;===============================================================
;====================Sprite Starting Data======================
sprites: ;y-pos, tile, attributes, x-pos
  ;player sprite
  .byte $80,$00,$00,$80 ;$0200-0203
  .byte $80,$01,$00,$88 ;$0204-0207
  .byte $88,$02,$00,$80 ;$0208-020B
  .byte $88,$03,$00,$88 ;$020C-020F
  .byte $90,$04,$00,$80 ;$0210-0213
  .byte $90,$05,$00,$88 ;$0214-0217
  .byte $98,$06,$00,$80 ;$0218-021B
  .byte $98,$07,$00,$88 ;$021C-021F

  ;ball sprite
  .byte $80,$75,$01,$40 ;$0220-0223
;===============================================================
;==================Background Starting Data=====================
TitleBackground:
  .incbin "bounceBinStart.nam"
PlayingBackground: 
  .incbin "bounceBin.nam"
attribute: ;Sets the color of those tiles
  .incbin "bounceAtt.nam"

;===============================================================
;=====================VECTORS===================================
.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0
;==============================================================
;=====================CHARS====================================
.segment "CHARS"  
  .incbin "newmario.chr"
;==============================================================


