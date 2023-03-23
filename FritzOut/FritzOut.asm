!to "FRITZOUT.d64",d64

joyspeed = $03
std_irq = $ea31
music_irq = $ea31;$7113

*=$0801
;** BASIC-Zeile: 2020 SYS 2062
 !word main-2, 2020
 !byte $9e
 !text " 2304"
 !byte $00,$00,$00

!zone Main
*=$0900
main
    jsr $e544 ;clear screen
    lda #15
    sta $d418; Volle Lautstärke SID
    jsr setupgraphics

    ldx #$00
    jsr createsprite1
    ldx #$00
    jsr createsprite2

    jsr setSpriteColors
    jsr drawborders
    jsr drawInfo

    
start_init
    jsr superInitialization
    
next_level
    jsr set_idle_irq
    jsr paintstones
    
start_partial
    jsr drawlives
    jsr initialization
    jsr resetSprites
    jsr recordStonesNumber
    jsr recordScore
    jsr recordLevel
    jsr recordSpeed
    jsr waitfire
    jsr set_main_irq

mainloop
    ;Meanings for stop
    ; 1: Life lost! Go on.
    ; 2: Game Over
    ; 3: Level cleared! New Stones.
    lda stop
    cmp #$00
    beq mainloop
    
    ;jsr set_std_irq
    lda stop
    cmp #$01
    beq LifeLost
    cmp #$03
    beq next_level

    jsr preparebasic
rts

;----------------------------------------------------
!zone MainThreadMethods
LifeLost
    ;lda #7
    ;sta SongNumber
    ;jsr PrepareSong
    ;jsr $ffcf
    jsr set_idle_irq
    jmp start_partial
    

waitfire
    lda $dc00
    cmp #$6f
    bne waitfire
  waitmore
    lda $dc00
    cmp #$6f
    beq waitmore
    lda $dc00
    cmp #$6e
    bne out
    lda #1
    sta ydirection
  out
rts


superInitialization
    lda #$00
    sta Score
    sta Score+1
    lda #$08
    sta Lives
    lda #1
    sta Level
    lda #$02
    sta speed
rts


initialization
    lda #$00
    sta xdirection
    sta ydirection
    sta stop
    sta Xfloat+1
    sta Yfloat+1
    sta rateType
    sta leftCollOn
    sta DirectDispatch
    lda #$48
    sta Xfloat
    lda #$9c
    sta Yfloat
    lda #2
    sta SongNumber
rts


resetSprites
    lda #$48
    sta $d000 ; x-Koordinate Sprite 1
    lda #$9c
    sta $d001 ; y- Koordinate Sprite 1
    lda #$3e
    sta $d002 ; x-Koordinate Sprite 2
    lda #$97
    sta $d003 ; y- Koordinate Sprite 2
    lda #0
    sta $d010
rts


idleIrq
    lda $d019
    sta $d019                          ;IRQ bestätigen
    
    jsr setCorrectMultiColor
    lda $d012
    cmp #86
    bcs dispatch_std
    
    inc IrqCounter

    lda $dc00
    cmp #$7e ;hoch
    beq hoch
    cmp #$7d ;runter
    beq runter
    cmp #123 ;left
    beq left 
    cmp #119 ;right
    beq right
    jmp music_irq
  right
    lda IrqCounter
    and #%00000111
    bne dispatch_music
    lda speed
    cmp #5
    bcs dispatch_music
    inc speed
    jsr recordSpeed
    jmp music_irq
  left
    lda IrqCounter
    and #%00000111
    bne dispatch_music
    lda speed
    cmp #0
    beq dispatch_music
    dec speed
    jsr recordSpeed
    jmp music_irq
  hoch
    lda $d003
    cmp #$5c
    bcc dispatch_music
    sec
    sbc #3
    sta $d003
    lda $d001
    sec
    sbc #3
    sta $d001
    sta Yfloat
    jmp music_irq
  runter
    lda $d003
    cmp #$d3
    bcs dispatch_music
    clc
    adc #3
    sta $d003
    lda $d001
    clc
    adc #3
    sta $d001
    sta Yfloat
    jmp music_irq
    
  dispatch_std
    jmp std_irq
  dispatch_music
    jmp music_irq


preparebasic
    ;lda #21
    ;sta $d018 ;normaler Zeichensatz
    lda $d016
    eor #%00010000
    sta $d016 ;kein multicolor
    jsr set_std_irq
rts


setCorrectMultiColor
    lda $d012
    cmp #86
    bcc multiColor2Face

    lda #00
    sta $d023
    sei
    lda #$00
    sta $d012
    lda $d011
    and #%01111111
    sta $d011
    cli
    rts

  multiColor2Face
    lda #10
    sta $d023
    sei
    lda #86       ;Zeile für neuen Interrupt
    sta $d012
    lda $d011
    and #%01111111
    sta $d011
    cli
rts


set_main_irq
    sei                                ;Interrupts sperren

    lda #<rasterIrq                    ;unsere Interrupt-Routine
    sta $0314                          ;in den IRQ-Vector eintragen
    lda #>rasterIrq                    ;auch das MSB
    sta $0315

    lda $d023
    and #%00001111
    tax
    lda #0
    cpx #10
    bne putItIn_Main
    lda #86
    
  putItIn_Main
    sta $d012                          ;Raster-IRQ ausglösen bei A

    lda $d011                          ;Zur Sicherheit auch noch
    and #%01111111                     ;das höchste Bit für den
    sta $d011                          ;gewünschten Raster-IRQ löschen 

    lda $d01a                          ;IRQs vom
    ora #%00000001                     ;VIC-II aktivieren
    sta $d01a

    lda #%01111111  ; disable CIA irq: Bit 7 sets the value, Bit 0...4 selects the bits to be set
    sta $dc0d
    lda $dc0d       ; acknowledge any pending CIA irq

    cli                                ;Interrupts wieder erlauben
rts


set_idle_irq
    sei                                ;Interrupts sperren

    lda #<idleIrq                    ;unsere Interrupt-Routine
    sta $0314                          ;in den IRQ-Vector eintragen
    lda #>idleIrq                    ;auch das MSB
    sta $0315
    
    lda $d023
    and #%00001111
    tax
    lda #0
    cpx #10
    bne putItIn_Idle
    lda #86
    
  putItIn_Idle
    sta $d012                          ;Raster-IRQ ausgelösen bei A

    lda $d011                          ;Zur Sicherheit auch noch
    and #%01111111                     ;das höchste Bit für den
    sta $d011                          ;gewünschten Raster-IRQ löschen 

    lda $d01a                          ;IRQs vom
    ora #%00000001                     ;VIC-II aktivieren
    sta $d01a

    lda #%01111111  ; disable CIA irq: Bit 7 sets the value, Bit 0...4 selects the bits to be set
    sta $dc0d
    lda $dc0d       ; acknowledge any pending CIA irq

    cli                                ;Interrupts wieder erlauben
rts


set_std_irq
    sei
    lda #$31
    sta $0314
    lda #$ea
    sta $0315
    
    lda $d01a
    and #%11111110
    sta $d01a
    
    lda #%11111111  ; ensable CIA irq: Bit 7 sets the value, Bit 0...4 selects the bits to be set
    sta $dc0d
    lda $dc0d       ; acknowledge any pending CIA irq    
    cli  
rts


!zone Raster
rasterIrq
lda $d019
sta $d019                          ;IRQ bestätigen
;==========================================================
    jsr setCorrectMultiColor
    lda $d012
    cmp #86
    bcc Main_Routine_Start
    jmp std_irq;no music

Main_Routine_Start
    lda DirectDispatch
    beq go_On
    jmp music_irq
go_On   
    lda stonesNumber
    cmp #$00
    bne not_yet_cleared
    lda #$03
    sta stop
    inc Level
    jmp music_irq
    
not_yet_cleared 
    jsr joystick
    lda halt_dummy
    beq no_halt
    jmp music_irq
    
rel_init
    lda #$01
    sta stop
    sta DirectDispatch
    dec Lives
    lda Lives
    cmp #0
    beq game_over
    jmp music_irq
    
game_over
    lda #2
    sta stop
    jmp music_irq

no_halt
;updateX
    lda #<Xfloat
    sta $fb
    lda #>Xfloat
    sta $fc
    
    lda rateType
    asl
    ldx speed
    clc
    adc speeds,x
    sta rateOffset  ;rateOffset = 2*rateType + 10*speed
                    ;will also be used in updateY below
    lda #<Xrates
    clc
    adc rateOffset
    sta $fd
    lda #>Xrates
    adc #$00
    sta $fe

    ldx xdirection
    jsr addFDFEToFBFC ;Xfloat updated    
    bcc noCarry
    ; Überlauf
    lda $d010
    eor xdirection
    bne rel_init  ;ball geht links raus
    lda $d010
    eor #$01
    sta $d010
  noCarry
    lda Xfloat
    sta $d000

;;updateY
    lda #<Yfloat
    sta $fb
    lda #>Yfloat
    sta $fc
    
    lda #<Yrates
    clc
    adc rateOffset
    sta $fd   
    lda #>Yrates
    adc #$00    
    sta $fe
    
    ldx ydirection
    jsr addFDFEToFBFC ;Yfloat updated 
    lda Yfloat
    sta $d001

;-------------------XY checks
;xcheck
    lda xdirection
    eor $d010
    beq ycheck
    ldx xdirection
    bne checkleft

;checkright (rechte wand)
    lda $d000
    cmp #$48
    bcs swapXdirection
    jmp ycheck

checkleft
    lda $d000
    cmp #75
    bcs ycheck
    cmp #46
    bcc ycheck
    lda #74
    sec
    sbc $d000
    asl
    tax
    lda leftDetectionTable,x
    sta $fb
    inx
    lda leftDetectionTable,x
    sta $fc
    ldx #$01 
    lda $d001
    ldy $d003
    jsr myadd ; a=y_ball - y_bat
    tax
    clc
    adc $fb
    cmp $fc
    bcs noLeftColl ;if no collision then leave
    lda leftCollOn
    bne ycheck     ;only one collision decision
    lda #$01
    sta leftCollOn
  ; Set the rateType
    txa
    clc
    adc #13
    lsr
    lsr
    sec
    sbc #4
    sta $fb
    jsr abs
    lda $fb
    sta rateType
  ; Sets the ydirection
    txa
    sec
    sbc #5
    jsr isNegativeA
    sta ydirection
  ; Swaps the xdirection if necessary
    lda $d000
    cmp #62
    bcc ycheck
  
swapXdirection
    lda xdirection
    eor #$01
    sta xdirection
    jsr PlayBounceSound
    
noLeftColl
    lda #$00
    sta leftCollOn

ycheck
    lda $d001
    cmp #$5b
    bcc swapYdirection
    cmp #$de
    bcs swapYdirection
    jmp checkblockcollision_main
  
swapYdirection
    lda ydirection
    eor #$01
    sta ydirection
    jsr PlayBounceSound

;-------------------Block Collision
checkblockcollision_main
    ;if $d010==0 and x<=128 then go_on
    lda $d010
    cmp #$01
    beq checkblockcollision
    lda $d000
    cmp #129
    bcs checkblockcollision
    
    jmp music_irq

checkblockcollision
  ;CheckRightLeftFromMe
    ;Get x-pos in front of me when going right/left
    lda $d000
    sta $fb
    lda $d010
    sta $fc
    
    lda $fb
    sec
    sbc xdirection
    sta $fb
    lda $fc
    sbc #$00
    sta $fc
    
    lsr $fc
    ror $fb
    lsr $fb
    lsr $fb;(x-xdirection)/8
    dec $fb;(x-xdirection)/8 - 1
    
    lda xdirection
    sta $fc
    asl $fc
    lda $fb
    sec
    sbc $fc      
    and #%11111110;abrunden auf gerade Zahl
    tax
    
    ;Get current y-pos
    lda $d001
    sec
    sbc #$04
    lsr
    lsr
    lsr
    sec
    sbc #10
    and #%11111110;abrunden auf gerade Zahl
    tay
    
    jsr getFBFCfromXY;FBFC is scr-addr of upper left stone corner
    ldy #$00
    lda ($fb),y
    cmp #$20
    beq checkTopDownFromMe
    jsr deletestoneatFBFC
    jsr swapXdirection_sr
    
  checkTopDownFromMe
    ;Get y-pos in front of me when going up/down
    ;ERST NOCH CHECKEN, OB ZU HOCH ODER ZU TIEF!!!!!!!!!!
    ;
    ;
    lda ydirection
    asl
    clc
    adc ydirection
    sta $fb
    lda $d001
    clc
    adc #2
    sec
    sbc $fb
    lsr
    lsr
    lsr
    sec
    sbc #10
    sec
    sbc ydirection
    and #%11111110;abrunden auf gerade Zahl
    tay
    
    ;Get current x-pos
    lda $d000
    sta $fb
    lda $d010
    sta $fc
    
    lsr $fc
    ror $fb
    lsr $fb
    lsr $fb;x/8
    dec $fb
    dec $fb
    lda $fb
    and #%11111110;abrunden auf gerade Zahl
    tax
    
    jsr getFBFCfromXY;FBFC is scr-addr of upper left stone corner
    ldy #$00
    lda ($fb),y
    cmp #$20
    beq blockCollCheckEnd
    jsr deletestoneatFBFC
    jsr swapYdirection_sr

blockCollCheckEnd
    jmp music_irq
;-------------------End Block Collision

swapYdirection_sr
    lda ydirection
    eor #$01
    sta ydirection
rts    

swapXdirection_sr
    lda xdirection
    eor #$01
    sta xdirection
rts
;-------------------Auf Joystick-Movements reagieren
joystick
    lda $dc00
    cmp #107
    bne weiter
    lda #$02
    sta stop
  weiter    
    cmp #$7e ;hoch
    beq steered
    cmp #$7d ;runter
    beq steered
    eor #$ff
    and #$10
    sta halt_dummy
    rts
    steered   
        and #$01
        eor #$01
        tax ; steer type in x (x=0 ist runter)
        lda $d003
        cpx #$00
        beq checkrunter

        ;checkhoch
        cmp #$5c
        bcs update
        rts

        checkrunter
        cmp #$d3
        bcc update
        rts

        update
        ldy #joyspeed
        jsr myadd
        sta $d003
rts
;======================================================
; End Main Routine

!zone Graphics
recordLevel
    ldx Level
    jsr XtoDecimalInFBFC
    lda #$30
    sta $fd
    lda #$04
    sta $fe
    jsr printNumberFBFCatFDFE
rts


recordScore
    lda Score
    sta $fb
    lda Score+1
    sta $fc
    lda #$58
    sta $fd
    lda #$04
    sta $fe
    jsr printNumberFBFCatFDFE
rts


recordStonesNumber
    ldx stonesNumber
    jsr XtoDecimalInFBFC
    lda #$81
    sta $fd
    lda #$04
    sta $fe
    jsr printNumberFBFCatFDFE
rts


recordSpeed
    ldx #0
    lda speed
    clc
    adc #1
    sta $fb
  speedLoop  
    lda #99
    sta $07a0,x
    lda #100
    sta $07c8,x
    inx
    cpx $fb
    bne speedLoop
  speedLoop2
    lda #74
    sta $07a0,x
    lda #77
    sta $07c8,x
    inx
    cpx #7
    bcc speedLoop2
rts


printNumberFBFCatFDFE
    ldx #$02
    ldy #$00
 checkByte
    dex
  ;checkUpperNibble
    lda $fb,x
    jsr makeAupperNibble
    cmp #$00
    bne dooIt
    cpy #$00
    beq checkLowerNibble
  dooIt
    clc
    adc #48
    sta ($fd),y
    iny
  checkLowerNibble
    lda $fb,x
    jsr makeAlowerNibble
    cmp #$00
    bne dooIt2
    cpy #$00
    beq nextByte
  dooIt2
    clc
    adc #48
    sta ($fd),y
    iny
  nextByte
    cpx #$00
    bne checkByte
    
    cpy #0
    bne nonnull
    lda #48
    sta ($fd),y
    iny
    
  nonnull    
    ;add two whitespaces
    lda #$20
    sta ($fd),y
    iny
    sta ($fd),y
rts


IsFBFCLastStonePos;Carry clear: yes
    txa
    pha
    ldx #$ff
  checkNext
    inx
    cpx #9
    beq raus
    lda lastPosStoneslo,x
    cmp $fb
    bne checkNext
    lda lastPosStoneshi,x
    cmp $fc
    bne checkNext
    clc
  raus
    pla
    tax
rts


drawlives
    lda #$04
    sta $fc

    ldx Lives
    lda posFirstLife,x
    sta $fd
    
    lda posFirstLife+8
  NextLifeCheck
    sta $fb
    sta $fe
    cmp $fd
    bcs draw
    jsr deleteLifeAtFBFC   
    jmp prepNextLifeCheck
  draw
    jsr drawLifeAtFBFC
  prepNextLifeCheck
    lda $fe
    clc
    adc #3
    cmp #79
    bne NextLifeCheck   
rts


drawLifeAtFBFC
    lda #$00
    ldx #$40
    ldy #$00
    loop
        txa
        sta ($fb),y
        inx
        iny
        cpy #$03
        bne loop
        ldy #$00
        jsr addfourty
        cpx #$49
        bne loop    
rts


deleteLifeAtFBFC
    lda #$20
    ldx #$00
    ldy #$00
    luup
        sta ($fb),y
        inx
        iny
        cpy #$03
        bne luup
        ldy #$00
        jsr addfourty
        cpx #$09
        bne luup    
rts


getFBFCfromXY
    lda lineslo,y
    sta $fb
    lda lineshi,y
    sta $fc
    stx $fd
    jsr addFDtoFBFC
    lda #$c8
    sta $fd
    lda #$04
    sta $fe
    jsr addFDFEtoFBFC
rts


paintstones
    lda Level
    sec
    sbc #1
    tax

    lda stoneSetslo,x
    sta $fb
    lda stoneSetshi,x
    sta $fc
    ldy #0
    lda ($fb),y
    sta totalStonesNumber
    iny
    lda ($fb),y
    sta stonesNumber
    
    lda #2
    sta $fd
    jsr addFDtoFBFC
    lda $fb
    sta modifyX+1
    lda $fc
    sta modifyX+2
    
    lda totalStonesNumber
    sta $fd
    jsr addFDtoFBFC
    lda $fb
    sta modifyY+1
    lda $fc
    sta modifyY+2
    
    jsr addFDtoFBFC
    lda $fb
    sta modifyC+1
    lda $fc
    sta modifyC+2

    ldx #$00
paintemallstones
    lda #$04
    sta $fc
    lda #$c8
    sta $fb
  modifyX
    lda $1111,x
    sta $fd
    jsr addFDtoFBFC
  modifyY
    ldy $1111,x
    lda lineslo,y
    sta $fd
    lda lineshi,y
    sta $fe
    jsr addFDFEtoFBFC    
    jsr paintstoneatFBFC
  modifyC
    lda $1111,x
    sta $fd
    jsr colorpaintstoneatFBFCwithFD

    inx
    cpx totalStonesNumber
    bne paintemallstones
rts


paintstoneatFBFC
    jsr IsFBFCLastStonePos
    bcc paintLastPosStone
    lda #81
    ldy #$00
    sta ($fb),y
    lda #82
    iny
    sta ($fb),y
    jsr addfourty
    lda #83
    ldy #$00
    sta ($fb),y
    lda #84
    iny
    sta ($fb),y
    rts
  paintLastPosStone
    lda #81
    ldy #$00
    sta ($fb),y
    lda #89
    iny
    sta ($fb),y
    jsr addfourty
    lda #83
    ldy #$00
    sta ($fb),y
    lda #90
    iny
    sta ($fb),y
rts

    
deletestoneatFBFC
    jsr IsFBFCblue
    bcs delete
    jsr PlayBounceSound ;Blue Stone
    rts
  delete
    jsr PlayCrashSound
    jsr IsFBFCLastStonePos
    bcc deleteLastPosStone
    lda #$20
    ldy #$00
    sta ($fb),y
    iny
    sta ($fb),y
    jsr addfourty
    lda #$20
    ldy #$00
    sta ($fb),y
    iny
    sta ($fb),y
    jmp updateInfo
  deleteLastPosStone
    lda #$20
    ldy #$00
    sta ($fb),y
    iny
    lda #80
    sta ($fb),y
    jsr addfourty
    lda #$20
    ldy #$00
    sta ($fb),y
    iny
    lda #80
    sta ($fb),y
  updateInfo
    dec stonesNumber
    jsr recordStonesNumber
    ldx speed
    inx
    stx $fb
    lda Score
    sed
    clc
    adc $fb
    sta Score
    lda Score+1
    adc #0
    sta Score+1
    cld
    jsr recordScore
rts    


IsFBFCblue;Carry clear: yes
    lda #$00
    sta $fd
    lda #$d4
    sta $fe
    jsr addFBFCtoFDFE
    ldy #0
    lda ($fd),y
    and #%00001111
    cmp #14
    beq clearcarry
    sec
    rts
  clearcarry
    clc
rts

    
colorpaintstoneatFBFCwithFD;FBFC is screen position
    lda $fd
    pha
    lda #$d8
    sta $fd
    lda #$d3
    sta $fe  
    jsr addFDFEtoFBFC
    pla 
    ldy #$00
    sta ($fb),y
    iny
    sta ($fb),y
    jsr addfourty
    ldy #$00
    sta ($fb),y
    iny
    sta ($fb),y
    rts
  
   
!zone Math
makeAupperNibble
    and #%11110000
    lsr
    lsr
    lsr
    lsr
rts


makeAlowerNibble
    and #%00001111
rts


XtoDecimalInFBFC
    STX $fd
    SED             ; Decimal-Mode aktivieren
    LDA #0          ; Ergebnis auf 0 initialisieren
    STA $fb
    STA $fc
    LDY #8          ; Anzahl der Binärwert-Bits
CNVBIT
    ASL $fd         ; Ein Binär-Bit ins Carry
    LDA $fb       ; und in BCD-Wert übernehmen
    ADC $fb       ; ebenso durch Verdopplung
    STA $fb       ; mittels Addition mit
    LDA $fc       ; sich selbst in 4 BCD-Ziffern
    ADC $fc
    STA $fc
    DEY             ; nächstes Binärwert-Bit
    BNE CNVBIT
    CLD             ; wieder Binary-Mode
rts


isNegativeA
; Transforms a to 1 if negative and 0 otherwise
    asl
    php
    pla
    and #%00000001
rts


addFDFEToFBFC
;adds (-1)^x*(&FDFE) to &FBFC (format: lo fr)
    ldy #$01
    cpx #$01;also sets/clears the carry
    bcs subtractFromFloat
  addToFloat
    lda ($fb),y
    adc ($fd),y
    sta ($fb),y
    dey
    bpl addToFloat
    rts
  subtractFromFloat
    lda ($fb),y
    sbc ($fd),y
    sta ($fb),y
    dey
    bpl subtractFromFloat
    ror
    eor #$80
    rol
rts
    

myadd ;adds  (-1)^x * y to a (x in {0,1});
    sty add_dummy
    cpx #$01
    bcs subtract
    adc add_dummy
    rts
  subtract
    sbc add_dummy
    ror
    eor #$80
    rol
rts
   
   
addfourty
    pha
    lda $fd
    pha
    lda #$28
    sta $fd
    jsr addFDtoFBFC
    pla
    sta $fd
    pla
rts


addFDFEtoFBFC;and store in FBFC
pha
lda $fb
clc
adc $fd
sta $fb
php
pla 
and #$01
clc
adc $fc
clc
adc $fe
sta $fc
pla
rts


addFBFCtoFDFE;and store in FDFE
pha
lda $fb
clc
adc $fd
sta $fd
php
pla 
and #$01
clc
adc $fc
clc
adc $fe
sta $fe
pla
rts


addFDtoFBFC;and store in FBFC
    pha
    lda $fb
    clc
    adc $fd
    sta $fb
    lda $fc
    adc #0
    sta $fc
    pla
rts


abs; computes abs($fb)
    lda $fb
    bpl positive
    eor #$ff
    sta $fb
    inc $fb
  positive
rts


!zone Anfangsgrafik
fillcolorram
    lda #$00
    sta $fb
    lda #$d8
    sta $fc
    ldx #$00
  nextline
    lda #$08    ; MultiColor-Schwarz
    ldy #$00
  fillcol
    sta ($fb),y
    iny
    cpy #40
    bne fillcol
    jsr addfourty
    inx
    cpx #$19
    bne nextline
  
  ;Text Area.................  
    ldx #$00
    lda #$29
    sta $fb
    lda #$d8
    sta $fc
  nextlinie
    lda #$01   ; HiRes-Weiß  
    ldy #$00
  fillme
    sta ($fb),y
    iny
    cpy #14
    bne fillme
    jsr addfourty
    inx 
    cpx #$03
    bne nextlinie
    
  ;Speed Area................
    ldx #$00
    lda #$9a
    sta $fb
    lda #$db
    sta $fc
  nextlinie2
    lda #$01   ; HiRes-Weiß  
    ldy #$00
  fillme2
    sta ($fb),y
    iny
    cpy #5
    bne fillme2
    jsr addfourty
    inx 
    cpx #$02
    bne nextlinie2
    ;Speed Colors
    lda #9
    sta $dba0
    sta $dbc8
    ;lda #15
    sta $dba1
    sta $dbc9
    ;lda #11
    sta $dba2
    sta $dbca
    ;lda #13
    sta $dba3
    sta $dbcb
    ;lda #10
    sta $dba4
    sta $dbcc
    ;lda #12
    sta $dba5
    sta $dbcd
    
  ;Dark blue background in corners
    lda #14
    sta $d800
    sta $d827
    sta $dbc0
    sta $dbe7    
rts


drawInfo
    ldx #$00
  levelloop  
    lda levelText,x
    sta $042a,x
    inx
    cpx #$06
    bne levelloop
    
    ldx #$00
  scoreloop  
    lda scoreText,x
    sta $0452,x
    inx
    cpx #$06
    bne scoreloop

    ldx #$00
  stonesloop
    lda stonesText,x
    sta $047a,x
    inx
    cpx #7
    bne stonesloop
   
  ;Speed Area  
    lda #91
    sta $079a
    lda #92
    sta $079b
    lda #93
    sta $079c
    lda #93
    sta $079d
    lda #94
    sta $079e
    lda #95
    sta $07c2
    lda #96
    sta $07c3
    lda #97
    sta $07c4
    lda #97
    sta $07c5
    lda #98
    sta $07c6
rts


drawborders
    lda #77
    ldx #39
    upperline
    sta $04a0,x
    sta $07c0,x
    dex
    bpl upperline

    lda #74
    ldx #39
    lowerline
    sta $0798,x
    sta $0400,x
    dex
    bpl lowerline

    ;Ecken
    lda #76
    sta $04a0
    lda #87
    sta $07c0
    lda #73
    sta $0798
    lda #85
    sta $0400
    lda #75
    sta $07bf
    lda #86
    sta $0427
    lda #78
    sta $04c7
    lda #88
    sta $07e7

    ;Seiten
    lda #79
    sta $0428
    sta $0450
    sta $0478
    lda #80
    ldx #39
    sta $0428,x
    sta $0450,x
    sta $0478,x

    lda #$c8
    sta $fb
    lda #$04
    sta $fc
    ldy #$00
      leftline
      lda #79
      ldx #$00
      sta ($fb,x)
      jsr addfourty
      iny
      cpy #$12
      bne leftline
      
    lda #$ef
    sta $fb
    lda #$04
    sta $fc
    ldy #$00
      rightline
      lda #80
      ldx #$00
      sta ($fb,x)
      jsr addfourty
      iny
      cpy #$12
      bne rightline    
rts


setupgraphics
lda #$0e
sta $d021 ; Hintergrundfarbe
lda #$06
sta $d020 ; Rahmenfarbe
lda #$08
sta $0286 ; Textfarbe

;new char set
lda $d016                          ;Multicolor
ora #%00010000                     ;Textmodus
sta $d016                          ;setzen
 
lda $d018                          ;Zeichensatz
and #%11110001                 
ora #%00001000                     ;nach $2000
sta $d018                          ;legen
 
lda #$01
sta $d022                          ;erste MC-Farbe
 
lda #10
sta $d023                          ;zweite MC-Farbe

jsr fillcolorram
rts


createsprite1
    lda spritedata1,x
    sta $0340,x
    inx
    cpx #$40
    bne createsprite1
    lda #$0d
    sta $07f8
rts

createsprite2
lda spritedata2,x
sta $0380,x
inx
cpx #$40
bne createsprite2
lda #$0e
sta $07f9
rts


setSpriteColors
lda #$0f
sta $d027 ; Farbe 0
sta $d028 ; same for sprite 2
lda #$0b
sta $d025 ; MC-Farbe 1
lda #$01
sta $d026 ; MC-Farbe 2
lda #$03
sta $d01c ; MultiColor (MC) on for both sprites
lda #$03
sta $d015 ; Sprites anzeigen
rts



!zone Music
PlayCrashSound
    lda #0
    sta $d412; Waveformgenerator aus
    sta $d40e; Lo Frequ = 0
    lda #20
    sta $d40f; Hi Frequ = 20
    lda #$58
    sta $d413; Anschlag = 5, Abschwellen = 8
    lda #$00
    sta $d414; Halten = 0, Ausklingen = 0
    lda #129
    sta $d412; Waveform = Rauschen
rts

PlayBounceSound
    lda #0
    sta $d40b; Waveformgenerator aus
    lda #$00
    sta $d407; Lo Frequ = 0
    lda #11
    sta $d408; Hi Frequ = 11
    lda #$08
    sta $d40c; Anschlag = 0, Abschwellen = 8
    lda #$00
    sta $d40d; Halten = 0, Ausklingen = 0
    lda #$11
    sta $d40b; Waveform = Dreieck
rts

PrepareSong
    lda SongNumber
    sei
    jsr $712a
    cli
rts


!zone DATA
;===================================================
spritedata1
!byte  $01,$40,$00,$06,$90,$00,$1b,$a4
!byte  $00,$1f,$a4,$00,$6e,$a9,$00,$6e
!byte  $a9,$00,$6a,$a9,$00,$6a,$a9,$00
!byte  $1a,$a4,$00,$1a,$a4,$00,$06,$90
!byte  $00,$01,$40,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$8f

spritedata2
!byte  $15,$40,$00,$6e,$90,$00,$59,$50
!byte  $00,$59,$50,$00,$6e,$90,$00,$6e
!byte  $90,$00,$6e,$90,$00,$6e,$90,$00
!byte  $6e,$90,$00,$6e,$90,$00,$6e,$90
!byte  $00,$6e,$90,$00,$6e,$90,$00,$6e
!byte  $90,$00,$6e,$90,$00,$6e,$90,$00
!byte  $6e,$90,$00,$59,$50,$00,$59,$50
!byte  $00,$6e,$90,$00,$15,$40,$00,$8f

lineslo
!byte 00,40,80,120,160,200,240,24,64,104,144,184,224,8,48,88,128
lineshi
!byte 00,00,00,00,00,00,00,01,01,01,01,01,01,02,02,02,02

lastPosStoneslo
!byte $ee,$3e,$8e,$de,$2e,$7e,$ce,$1e,$6e
lastPosStoneshi
!byte $04,$05,$05,$05,$06,$06,$06,$07,$07

leftDetectionTable
!byte 7,24,7,24,9,28,9,28,10,30,10,30,11,32,11,32
!byte 12,34,12,34,12,34,12,34,12,34,12,34,12,34
!byte 12,34,12,34,12,34,12,34,12,34,12,34
!byte 11,32,11,32,10,30,10,30,9,28,9,28,7,24,7,24

Xrates !byte $00,$f8,$00,$e5,$00,$b5,$00,$72,$00,$3e  ;speed 0
       !byte $01,$f1,$01,$ca,$01,$6a,$00,$e5,$00,$7c  ;speed 1
       !byte $02,$e9,$02,$af,$02,$1f,$01,$57,$00,$ba  ;speed 2
       !byte $03,$e1,$03,$94,$02,$d4,$01,$ca,$00,$f8  ;speed 3
       !byte $04,$da,$04,$79,$03,$89,$02,$3c,$01,$36  ;speed 4
       !byte $05,$d2,$05,$5e,$04,$3e,$02,$af,$01,$75  ;speed 5

Yrates !byte $00,$3e,$00,$72,$00,$b5,$00,$e5,$00,$f8  ;speed 0
       !byte $00,$7c,$00,$e5,$01,$6a,$01,$ca,$01,$f1  ;speed 1
       !byte $00,$ba,$01,$57,$02,$1f,$02,$af,$02,$e9  ;speed 2
       !byte $00,$f8,$01,$ca,$02,$d4,$03,$94,$03,$e1  ;speed 3
       !byte $01,$36,$02,$3c,$03,$89,$04,$79,$04,$da  ;speed 4
       !byte $01,$75,$02,$af,$04,$3e,$05,$5e,$05,$d2  ;speed 5

speeds !byte 0,10,20,30,40,50

posFirstLife !byte 79,76,73,70,67,64,61,58,55,52

xdirection     !byte $00
ydirection     !byte $00
Xfloat         !byte $48,$00
Yfloat         !byte $9c,$00
rateType       !byte $00 ;value in 0..4
speed          !byte $00 ;value in 0..5
rateOffset     !byte $00 ;used locally in updateX/Y
leftCollOn     !byte $00
stonesNumber   !byte $00
totalStonesNumber !byte $00
Score          !byte 0,0 ;stored as decimal!!!
Lives          !byte $03
Level          !byte $01
IrqCounter     !byte $00
DirectDispatch !byte $00
SongNumber     !byte $02
halt_dummy     !byte $00
add_dummy      !byte $00
stop           !byte $00 ;1=partial_init, 2=back to BASIC, 3=level cleared

levelText      !byte 12,5,22,5,12,58
scoreText      !byte 19,3,15,18,5,58
stonesText     !byte 19,20,15,14,5,19,58

;---- Addresses of Stone Sets (Levels) ---------------
stoneSetshi  !byte $30,$30
stoneSetslo  !byte $00,$6e

;=====================================================
*=$2000
               !bin "chars.bin"
;=====================================================
*=$3000
; LEVEL 1
!byte 36,30;total no of stones,deletable no of stones

!byte 32,34,36,38,32,34,36,38,32,34,36,38
!byte 32,34,36,38,32,34,36,38,32,34,36,38
!byte 32,34,36,38,32,34,36,38,32,34,36,38

!byte 0,0,0,0,2,2,2,2,4,4,4,4
!byte 6,6,6,6,8,8,8,8,10,10,10,10
!byte 12,12,12,12,14,14,14,14,16,16,16,16

!byte 14,10,10,14,14,10,10,10,14,10,10,10
!byte 10,10,10,10,10,10,10,10,10,10,10,10
!byte 10,10,10,10,10,10,10,10,14,10,10,14

; LEVEL 2
!byte 45,45

!byte 30,32,34,36,38,30,32,34,36,38,30,32,34,36,38
!byte 30,32,34,36,38,30,32,34,36,38,30,32,34,36,38
!byte 30,32,34,36,38,30,32,34,36,38,30,32,34,36,38

!byte 0,0,0,0,0,2,2,2,2,2,4,4,4,4,4
!byte 6,6,6,6,6,8,8,8,8,8,10,10,10,10,10
!byte 12,12,12,12,12,14,14,14,14,14,16,16,16,16,16

!byte 11,10,10,10,10,10,10,10,10,10,10,10,10,10,10
!byte 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10
!byte 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10




;*=$70f0   ;-2 to remove the load adress
;      !binary "Great_Giana_Sisters.sid",,126