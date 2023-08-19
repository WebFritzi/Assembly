!to "WINGAMES.d64",d64

!zone Constants
NO_OF_GAMES = 3
SCRMEM = $0400
std_irq = $ea31
joy_delay = 13
VIC = $d000
sid = $d400
xPos0 = VIC
yPos0 = VIC+1
xPos1 = VIC+2
yPos1 = VIC+3
xPos2 = VIC+4
yPos2 = VIC+5
xPos3 = VIC+6
yPos3 = VIC+7
xPos4 = VIC+8
yPos4 = VIC+9
xPos5 = VIC+10
yPos5 = VIC+11
xPos6 = VIC+12
yPos6 = VIC+13
xPos7 = VIC+14
yPos7 = VIC+15
xposmsb  = VIC+16
FRAMECOLOR = VIC+32
BKGCOLOR = VIC+33
MULTICOLOR1 = VIC+34
MULTICOLOR2 = VIC+35
col0  = VIC+39
col1  = VIC+40
col2  = VIC+41
col3  = VIC+42
col4  = VIC+43
col5  = VIC+44
col6  = VIC+45
col7  = VIC+46
CHAR_BASE = $2000
CHAR_NUMBERS_BASE = CHAR_BASE + $180
CL_BLACK = 0
CL_WHITE = 1
CL_RED = 2
CL_CYAN = 3
CL_MAGENTA = 4
CL_DARKGREEN = 5
CL_DARKBLUE = 6
CL_YELLOW = 7
CL_LIGHTBROWN = 8
CL_DARKBROWN = 9
CL_ROSE = 10
CL_DARKGRAY = 11
CL_MIDGRAY = 12
CL_LIGHTGREEN = 13
CL_LIGHTBLUE = 14
CL_LIGHTGRAY = 15
SPRPTR_0 = 2040
SPRPTR_1 = 2041
SPRPTR_2 = 2042
SPRPTR_3 = 2043
SPRPTR_4 = 2044
SPRPTR_5 = 2045
SPRPTR_6 = 2046
SPRPTR_7 = 2047
DM_SHUTDOWN = 0
DM_LOAD = 1
GM_NORMAL = 0
GM_MENU = 1
GM_DIALOG = 255
; Exit Codes
EC_RBTNPRESS = 1
EC_RBTNRELEASE = 2
EC_LBTNPRESS = 3
EC_LBTNRELEASE = 4
EC_MOUSEMOVE = 5
EC_GAMEEXIT = 6
EC_WON = 7
EC_RESTART = 8
EC_DBLCLICK = 9
EC_LOAD = 10
;Sprite Blocks
SP_Mouse0 = Mousepointer0Addr/64
SP_Mouse1 = Mousepointer1Addr/64
SP_WinFlag = WinFlagAddr/64
SP_L = LAddr/64
SP_VertLine = VertLineAddr/64
SP_FlagPart = FlagPartAddr/64
SP_HorzLine = HorzLineAddr/64
SP_W = WAddr/64
SP_ind = indAddr/64
SP_do = doAddr/64
SP_ws = wsAddr/64
SP_s = sAddr/64
SP_tvGray = tvGrayAddr/64
SP_tvBlack = tvBlackAddr/64
SP_MineBlack = MineBlackAddr/64
SP_MineWhite = MineWhiteAddr/64
SP_KingBlack = KingBlackAddr/64
SP_KingWhite = KingWhiteAddr/64
SP_SolitaireBlack = SolitaireBlackAddr/64
SP_SolitaireRed = SolitaireRedAddr/64
SP_Balken = BalkenAddr/64
SP_DriveGray = DriveGrayAddr/64
SP_DriveBlack = DriveBlackAddr/64


!zone BASIC_stub
*=$0801
;** BASIC-Zeile: 0 SYS2061
 !word main-2, 0
 !byte $9e
 !text "2061"
 !byte $00,$00,$00

!zone Main
main            lda #0
                sta $9d; no kernal messages ("SEARCHING FOR ..." etc)
                jsr MouseJoyChoice
Restart         jsr SetGlobals
                jsr ClearScreen
                jsr DrawTaskbar
                jsr InstallIRQ

!zone MainLoop
MainLoop        lda exit_code
                beq MainLoop
                ldx #0
                stx exit_code
                cmp #EC_RBTNPRESS
                beq RBtnPress
                cmp #EC_RBTNRELEASE
                beq RBtnRelease
                cmp #EC_LBTNPRESS
                beq LBtnPress
                cmp #EC_LBTNRELEASE
                beq LBtnRelease
                cmp #EC_MOUSEMOVE
                beq Moved
                cmp #EC_LOAD
                beq GoLoadGame
                cmp #EC_GAMEEXIT
                beq Exit
                jmp MainLoop


RBtnPress       jsr OnRBtnPress
                jmp MainLoop

RBtnRelease     jsr OnRBtnRelease
                jmp MainLoop

LBtnPress       jsr OnLBtnPress
                jmp MainLoop

LBtnRelease     jsr OnLBtnRelease
                jmp MainLoop

Moved           jsr OnMouseMove
                jmp MainLoop

GoLoadGame      jsr RestoreIRQ
                ; Turn screen off
                lda $d011
                and #239
                sta $d011
                ; Start loading
                jsr LoadGame
                ; Turn screen back on
                lda $d011
                ora #16
                sta $d011
                ; Start game
                lda MouseOn
                sta $4c00
                jsr $4e00 ;<--- Start the game
                lda #0
                sta GameIndex
                jmp Restart

Exit            lda #0
                sta 198; number of chars in keyboard buffer
                sta VIC+21
                jsr RestoreIRQ
                jsr MultiColorOff
                jsr $e544; clear screen
                lda #21
                sta $d018
                lda #14
                sta FRAMECOLOR
                lda #6
                sta BKGCOLOR
                rts
;=============================================

!zone ____________Events______________
;============================================= OnDblClick
;OnDblClick      rts

;============================================= OnButtonPress
; Right Button------------------------------------------------
OnRBtnPress     ;lda $d012
;                cmp #122;254
;                bcc OnRBtnPress
                ;lda #EC_GAMEEXIT
                ;sta exit_code
                rts

; Left Button-------------------------------------------------
OnLBtnPress     jsr GetMouseInfo
                lda GameMode
                bpl ClickInNormalMenuMode
                jmp ClickInDlgMode                
                
ClickInNormalMenuMode
                lda StartBtnPushed
                bne MayCloseSMenu
                jsr IsInStartBtn
                lda res
                bne GotoMenuMode
                rts
GotoMenuMode    lda $d012
                cmp #122;254
                bcc GotoMenuMode
                lda #1
                sta StartBtnPushed
                ;inc $d020
                jsr DrawButtonDown
                ;dec $d020
                rts
MayCloseSMenu   jsr IsInStartMenu
                lda res
                beq CloseStartMenu
                rts
CloseStartMenu  lda $d012
                cmp #254
                bcc CloseStartMenu
                lda #0
                sta StartBtnPushed
                jsr DrawSpritesUp
                jsr KillStartMenu
                jsr DrawTaskbar
                lda #GM_NORMAL
                sta GameMode
                rts

ClickInDlgMode  jsr IsInYesBtn
                lda res
                bne +
                jsr IsInNoBtn
                lda res
                bne ++
                rts
+               ; Clicked in Yes button
                jsr PressYesBtn
                rts
++              ; Clicked in No button
                jsr PressNoBtn
                rts

;============================================= OnButtonRelease
; Right Button------------------------------------------------
OnRBtnRelease   rts

; Left Button-------------------------------------------------
OnLBtnRelease   jsr GetMouseInfo
                lda GameMode
                beq RelInNormalMode
                bpl RelInMenuMode
                jmp RelInDlgMode
                
;----- Rel jump too far ----------------------
RelInMenuMode   jsr RelInMenuMode2
                rts
RelInNormalMode jsr RelInNormalMode2
                rts
RelInDlgMode    jsr RelInDlgMode2
                rts

;----- In dialog mode ------------------------
RelInDlgMode2   lda bYesBtnPressed
                bne +
                lda bNoBtnPressed
                bne ++
                rts
+               ; Yes button is pressed
                jsr ReleaseYesBtn
                jsr Pause
                jsr KillShutDownDlg
                lda #GM_NORMAL
                sta GameMode
                lda #EC_GAMEEXIT
                sta exit_code
                rts
++              ; No button is pressed
                jsr ReleaseNoBtn
                jsr Pause
                jsr KillShutDownDlg
                lda #GM_NORMAL
                sta GameMode
                rts

;----- In menu mode --------------------------
RelInMenuMode2  jsr IsInBarBox
                lda res
                beq +
                jsr CloseStartMenu
                jsr GetMenuItem
                lda res
                sta SMC_MenuItem+1
                cmp #3
                bcc RelInGame
                ; Clicked in Shutdown
                jsr ShowShutDownDlg
                rts
RelInGame       jsr ShowLoadDlg
                jsr LongPause
                lda #GM_NORMAL
                sta GameMode
SMC_MenuItem    lda #$00; is filled
                ;tax
                ;inx
                sta GameIndex
                lda #EC_LOAD
                sta exit_code
+               rts

;----- In normal mode ------------------------
RelInNormalMode2
                lda StartBtnPushed
                bne +
                rts
+               lda #GM_MENU
                sta GameMode
                rts

Pause           ldx #32
                lda #0
                sta dummy
-               dec dummy
                bne -
                dex
                bne -
-               lda $d012
                bne -
                rts

LongPause       lda #3
                sta dummy
---             ldy #0
--              ldx #0
-               dex
                bne -
                dey
                bne --
                dec dummy
                bne ---
                rts
                
;============================================= OnMouseMove
OnMouseMove     lda GameMode
                beq MovInNormalMode
                bpl MovInMenuMode
;----- In dialog mode ------------------------
                lda bFirePressed
                bne fireDLGpressed
                lda IsLBtnPressed
                bne fireDLGpressed
                rts
fireDLGpressed  jsr GetMouseInfo
                jsr IsInYesBtn
                lda res
                bne hover_yes
                jsr IsInNoBtn
                lda res
                bne hover_no
                ; Not in any button
                lda bYesBtnPressed
                beq +
                jsr ReleaseYesBtn
                rts
+               lda bNoBtnPressed
                beq +
                jsr ReleaseNoBtn
                rts
hover_yes       ; Hovers Yes button
                lda bYesBtnPressed
                bne +
                jsr PressYesBtn
                rts
hover_no        ; Hovers No button
                lda bNoBtnPressed
                bne +
                jsr PressNoBtn
+               rts

;----- In normal mode ------------------------
MovInNormalMode rts

;----- In menu mode --------------------------
MovInMenuMode   rts
;=============================================

!zone Joystick
bFirePressed    !byte 0
dx              !byte 0 ;joystick
dy              !byte 0 ;directions

Joystick        lda dc_counter
                beq +
                dec dc_counter
                bne +
                ; time's up
                lda bFirePressed
                bne +
                lda #EC_LBTNRELEASE
                sta exit_code
+               jsr ProcessJoystick
                rts


ProcessJoystick jsr JoyDecoder
                bcs NoFire
                ;Fire pressed
                lda bFirePressed
                bne OnlyMove;fire was also pressed before
                lda #1;fire not pressed before
                sta bFirePressed
                lda dc_counter
                bne joydblclk
                lda #dc_delay
                sta dc_counter
                ; event click
                lda #EC_LBTNPRESS
                sta exit_code
                jmp OnlyMove
joydblclk       ; event double click
                lda #EC_DBLCLICK
                sta exit_code
                
                
                
                
                sta block_release
                lda #0
                sta dc_counter
                rts
NoFire          ;Fire not pressed
                lda bFirePressed
                beq OnlyMove;fire was also not pressed before
                lda #0;fire was pressed before
                sta bFirePressed
                lda dc_counter
                bne OnlyMove
                lda block_release
                bne +
                lda #EC_LBTNRELEASE
                sta exit_code
+               lda #0
                sta block_release

OnlyMove        lda dx
                ora dy
                bne move
                rts
move            lda #EC_MOUSEMOVE
                sta exit_code
                lda dx
                asl
                asl
                clc
                adc VIC
                sta VIC
                sta VIC+2
                lda dx
                bpl right
                ;left
                bcc ovf_lr
                lda $d010
                and #%00000011
                bne change_y
                lda VIC
                cmp #24
                bcs change_y
                lda #24
                sta VIC
                sta VIC+2
                jmp change_y
right           bcs ovf_lr

                lda $d010
                and #%00000011
                beq change_y
                lda VIC
                cmp #88
                bcc change_y
                lda #87
                sta VIC
                sta VIC+2
                jmp change_y
                
                jmp change_y
ovf_lr          lda $d010
                eor #%00000011
                sta $d010
change_y        lda dy
                asl
                asl
                clc
                adc VIC+1
                sta VIC+1
                sta VIC+3
                cmp #50
                bcc correct_yup
                cmp #250
                bcs correct_ydown
                rts
correct_yup     lda #50
                sta VIC+1
                sta VIC+3
                rts
correct_ydown   lda #249
                sta VIC+1
                sta VIC+3
                rts


JoyDecoder      lda $dc00     ; get input from port 2 only
                ldy #0        ; this routine reads and decodes the
                ldx #0        ; joystick/firebutton input data in
                lsr           ; the accumulator. this least significant
                bcs djr0      ; 5 bits contain the switch closure
                dey           ; information. if a switch is closed then it
djr0            lsr           ; produces a zero bit. if a switch is open then
                bcs djr1      ; it produces a one bit. The joystick dir-
                iny           ; ections are right, left, forward, backward
djr1            lsr           ; bit3=right, bit2=left, bit1=backward,
                bcs djr2      ; bit0=forward and bit4=fire button.
                dex           ; at rts time dx and dy contain 2's compliment
djr2            lsr           ; direction numbers i.e. $ff=-1, $00=0, $01=1.
                bcs djr3      ; dx=1 (move right), dx=-1 (move left),
                inx           ; dx=0 (no x change). dy=-1 (move up screen),
djr3            lsr           ; dy=1 (move down screen), dy=0 (no y change).
                stx dx        ; the forward joystick position corresponds
                sty dy        ; to move up the screen and the backward
                rts           ; position to move down screen.
                              ;
                              ; at rts time the carry flag contains the fire
                              ; button state. if c=1 then button not pressed.
                              ; if c=0 then pressed.
!zone Mouse
dc_delay = 15;13 ; double click delay
acc      = 1     ; accelaration (fast: 1, slow: 128)
ProcessDblClk   !byte 0; If set, recognizes double clicks
;---------------------------------
potx     = sid+$19
poty     = sid+$1a
maxx     = 319 ;Screen Width
maxy     = 199 ;Screen Height
offsetx  = 24  ;Sprite left border edge
offsety  = 50  ;Sprite top  border edge
musposx         !word 160
musposy         !word 100
old_yPos        !byte 0
old_xPos        !byte 0
dc_counter      !byte 0
block_release   !byte 0
IsLBtnPressed   !byte 0; documents left press at any time
IsRBtnPressed   !byte 0; documents right press at any time

Mouse           lda ProcessDblClk
                beq +
                lda dc_counter
                beq +
                dec dc_counter
                ;lda dc_counter
                bne +
                ; time's up
                lda IsLBtnPressed
                bne +
                lda #EC_LBTNRELEASE
                sta exit_code
+               jsr GetClicks
                jsr scanmovs
                jsr boundmus
                rts

;---------------------------------------
GetClicks       lda $dc01
                cmp #239
                bne noLbtn
                ; left btn pressed
                lda IsLBtnPressed
                bne out_here;btn was also pressed before
                ; btn was not pressed before
                lda #1
                sta IsLBtnPressed
                lda ProcessDblClk
                beq +
                lda dc_counter
                bne dblclk
                lda #dc_delay
                sta dc_counter
                ; event click
+               lda #EC_LBTNPRESS
                sta exit_code
                rts
dblclk          ; event double click
                lda #EC_DBLCLICK
                sta exit_code
                sta block_release
                lda #0
                sta dc_counter
                rts
noLbtn          ; left btn not pressed
                lda IsLBtnPressed
                beq checkRight;button was also not pressed before
                ; left btn was pressed before
                lda #0
                sta IsLBtnPressed
                lda ProcessDblClk
                beq +
                lda dc_counter
                bne out_here
                lda block_release
                bne ++
+               lda #EC_LBTNRELEASE
                sta exit_code
++              lda #0
                sta block_release
out_here        rts
checkRight      lda $dc01
                cmp #254
                bne noRbtn
                ; right btn pressed
                lda IsRBtnPressed
                bne out_here; right btn was also pressed before
                ; right btn was not pressed before
                lda #1
                sta IsRBtnPressed
                lda #EC_RBTNPRESS
                sta exit_code
                rts
noRbtn          ; right btn not pressed
                lda IsRBtnPressed
                beq out_here; right btn was also not pressed before
                ; right btn was pressed before
                lda #0
                sta IsRBtnPressed
                lda #EC_RBTNRELEASE
                sta exit_code
                rts
;---------------------------------------

scanmovs        ;--- X Axis ---
                lda potx
oldpotx         ldy #0
                jsr movechk
                beq noxmove

                sty oldpotx+1

                clc
                adc musposx
                sta musposx
                txa            ;upper 8-bits
                adc musposx+1
                sta musposx+1
noxmove         ;--- Y Axis ---
                lda poty
oldpoty         ldy #0
                jsr movechk
                beq noymov

                sty oldpoty+1

                clc
                eor #$ff       ;Reverse Sign
                adc #1

                clc
                adc musposy
                sta musposy
                txa            ;Upper 8-bits
                eor #$ff       ;Reverse Sign
                adc musposy+1
                sta musposy+1
noymov          rts
;---------------------------------------
movechk         ;Y -> Old Pot Value
                ;A -> New Pot Value
                sty oldvalue+1
                tay
                sec

oldvalue        sbc #$ff
                and #%01111111
                cmp #%01000000
                bcs neg

                lsr ;remove noise bit
                beq nomove

                cmp #acc ;Acceleration Speed
                bcc *+3
                asl ;X2

                ldx #0
                cmp #0

                ;A > 0
                ;X = 0 (sign extension)
                ;Y = newvalue
                ;Z = 0

                rts

neg             ora #%10000000
                cmp #$ff
                beq nomove

                sec    ;Keep hi negative bit
                ror ;remove noise bit

                cmp #256-acc ;Acceleration Speed
                bcs *+3
                asl ;X2

                ldx #$ff

                ;A < 0
                ;X = $ff (sign extension)
                ;Y = newvalue
                ;Z = 0

                ;fallthrough

nomove          ;A = -
                ;X = -
                ;Y = -
                ;Z = 1
                rts

;---------------------------------------
boundmus        ldx musposx+1
                bmi zerox
                beq chky

                ldx #maxx-256
                cpx musposx
                bcs chky

                stx musposx
                bcc chky

zerox           ldx #0
                stx musposx
                stx musposx+1

chky            ldy musposy+1
                bmi zeroy
                beq loychk

                dec musposy+1
                ldy #maxy
                sty musposy
                bne movemus

loychk          ldy #maxy
                cpy musposy
                bcs movemus

                sty musposy
                bcc movemus

zeroy           ldy #0
                sty musposy
                sty musposy+1

movemus         lda xPos0
                sta old_xPos
                
                clc
                lda musposx
                adc #offsetx
                sta xPos0
                sta xPos1
                
                lda musposx+1
                adc #0
                beq clearxhi

                ;set x sprite pos high
                lda xposmsb
                ora #%00000011         
                bne *+7
         
clearxhi        ;set x sprite pos low
                lda xposmsb
                and #%11111100
                sta xposmsb
                
                lda xPos0
                cmp old_xPos
                beq make_ymove
                ; EC_MOUSEMOVE can't override other exit_codes
                lda exit_code
                bne make_ymove
                lda #EC_MOUSEMOVE
                sta exit_code

make_ymove      lda yPos0
                sta old_yPos
                
                clc
                lda musposy
                adc #offsety
                sta yPos0
                sta yPos1

                cmp old_yPos
                beq no_ymove
                ; EC_MOUSEMOVE can't override other exit_codes
                lda exit_code
                bne no_ymove
                lda #EC_MOUSEMOVE
                sta exit_code
no_ymove        rts

!zone Preparations
;Once called, never changes
SetGlobals      ; Char set at $2000
                lda $d018
                and #%11110001
                ora #%00001000
                sta $d018

                ; Install mouse pointer sprites
                lda #SP_Mouse0
                sta SPRPTR_0
                lda #SP_Mouse1
                sta SPRPTR_1
                lda #CL_BLACK
                sta col0
                lda #CL_WHITE
                sta col1
                lda #100
                sta VIC
                sta VIC+2
                sta VIC+1
                sta VIC+3
                lda #%11111111
                sta VIC+21
                lda VIC+16
                and #%00000011
                sta VIC+16
                
                ; Colors: bkg, frame, and multicolor
                lda #CL_BLACK
                sta FRAMECOLOR
                lda #CL_LIGHTGRAY
                sta BKGCOLOR
                ; Turn MC on
                jsr MultiColorOn
                lda #CL_DARKBLUE
                sta MULTICOLOR1
                lda #CL_MIDGRAY
                sta MULTICOLOR2
                
                ; Fill GameNamesLo/Hi
                lda #<GameNames
                sta $fb
                lda #>GameNames
                sta $fc
                ldx #0
game_names_loop lda $fb
                sta GameNamesLo,x
                lda $fc
                sta GameNamesHi,x
                ldy StrLen,x
                jsr AddYtoFBFC
                inx
                cpx #NO_OF_GAMES
                bcc game_names_loop
                rts
                rts

!zone IRQ
RasterIRQ       lda $d019
                sta $d019
                
                lda $d012
                cmp #228
                bcc +++
                cmp #236
                bcc L_228
;L_236          ; Y = 236
                lda #CL_YELLOW
                sta MULTICOLOR1
                lda #0
                sta $d012
                lda $d011
                and #%01111111
                sta $d011
                jmp $ea81
L_228           ; Y = 228
                lda #CL_BLACK
                sta MULTICOLOR1
                lda #236
                sta $d012
                lda StartBtnPushed
                beq +
                jsr DrawSpritesDown
                jmp $ea81
+               jsr DrawSpritesUp
                jmp $ea81

+++             ; Y < 228
                cmp #106
                bcc L_0
                cmp #115
                bcc L_106
                cmp #151
                bcc L_115
                cmp #175
                bcc L_152
                cmp #200
                bcc L_175
                cmp #220
                bcc L_200
                
;L_220          ; Y = 220
                ; Only triggers in Dialog Mode
                ;inc $d020
                jsr MultiColorOn
                lda #228
                sta $d012
                ;dec $d020
                jmp $ea81
L_200           ; Y = 200
                ;lda $d012
;-               cmp $d012
;                beq -
;                inc $d020
                jsr Draw_W
                jsr Draw_TV
                ;dec $d020
                lda #228
                sta $d012
                jmp $ea81
L_175           ; Y = 175
                ;lda $d012
;-               cmp $d012
;                beq -
;                inc $d020
                jsr Draw_ind
                jsr DrawMine
                ;dec $d020
                lda #200
                sta $d012
                jmp $ea81
L_152           ; Y = 152
                ;lda $d012
;-               cmp $d012
;                beq -
;                inc $d020
                jsr Draw_do
                jsr DrawKing
                ;dec $d020
                lda #175
                sta $d012
                jmp $ea81
L_115           ; Y = 115
                ;inc $d020
                jsr Draw_s
                jsr Draw_ws
                jsr DrawSolitaire
                lda #152
                sta $d012
                ;dec $d020
                jmp $ea81
L_106           ; Y = 106
                ; Only triggers in Dialog Mode
                lda #CL_LIGHTGRAY
                sta BKGCOLOR
                lda #220
                sta $d012
                jmp $ea81
L_0             ; Y = 0
                lda #CL_DARKBLUE
                sta MULTICOLOR1
                lda StartBtnPushed
                bne ++
                lda GameMode
                cmp #GM_DIALOG
                bne +
                ; No menu, but a dialog
                jsr MultiColorOff
                lda #CL_DARKBLUE
                sta BKGCOLOR
                jsr DlgSpriteHandler
                lda #106
                sta $d012
                jmp MouseJoy
+               ; no menu, no dialog
                lda #228
                sta $d012
                jmp MouseJoy
++              ; StartMenu open
                jsr MultiColorOn
                lda #31
                sta xPos2
                sta xPos3
                lda #CL_LIGHTGRAY
                sta col2
                sta col3
                lda #0
                sta VIC+29
                jsr Highlight
                lda #115
                sta $d012
                ; Mouse and Joystick
MouseJoy        lda MouseOn
                bne mouse
                jsr Joystick
                jmp std_irq
mouse           jsr Mouse
                jmp std_irq

InstallIRQ      sei
                lda #<RasterIRQ
                sta $0314
                lda #>RasterIRQ
                sta $0315
                ; Raster IRQ at y=0
                lda #0
                sta $d012
                lda $d011
                and #%01111111
                sta $d011
                ; Enable raster IRQ
                lda $d01a
                ora #%00000001
                sta $d01a
                ; Disable CIA IRQ: Bit 7 sets the value, Bit 0...4 selects the bits to be set
                lda #%01111111
                ; Acknowledge any pending CIA irq
                sta $dc0d
                lda $dc0d
                cli
                rts

RestoreIRQ      sei
                lda #$31
                sta $0314
                lda #$ea
                sta $0315
                ; Disable raster IRQ
                lda $d01a
                and #%11111110
                sta $d01a
                ; Enable CIA IRQ
                lda #%11111111
                sta $dc0d
                lda $dc0d
                cli
                rts

!zone Graphics
;ExtBkgModeOn    lda $d011
;                ora #%01000000
;                sta $d011
;                ; Char set at $2800
;                lda $d018
;                and #%11110001
;                ora #%00001010
;                sta $d018
;                rts
;
;ExtBkgModeOff   lda $d011
;                and #%10111111
;                sta $d011
;                ; Char set at $2000
;                lda $d018
;                and #%11110001
;                ora #%00001000
;                sta $d018
;                rts

MultiColorOn    lda $d016
                ora #%00010000
                sta $d016
                rts

MultiColorOff   lda $d016
                and #%11101111
                sta $d016
                rts

GetMouseInfo    jsr MouseToScr
                lda VIC
                sta MouseInfo+2
                lda VIC+1
                sta MouseInfo+3
                lda VIC+16
                sta MouseInfo+4
                rts

;Returns Mouse pos in scr coords in MouseInfo, MouseInfo+1
MouseToScr      lda $d000
                sec
                sbc #24
                sta MouseInfo
                lda $d010
                sbc #0
                lsr
                lda MouseInfo
                ror
                lsr
                lsr
                sta MouseInfo

                lda $d001
                sec
                sbc #50
                lsr
                lsr
                lsr
                sta MouseInfo+1
                rts

;Expects scr pos in Y,X
;Output: scr mem adr in FDFE
PosToScrMem     sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $fd
                lda ScrTabHi,x
                adc #0
                sta $fe
                rts

ClearScreen     ldx #0
                ldy #CL_DARKBLUE
-               lda #47
                sta SCRMEM,x
                sta SCRMEM+$100,x
                sta SCRMEM+$200,x
                sta SCRMEM+$2e8,x ; nur bis 1000er-grenze
                tya         ; farbe
                sta $d800,x
                sta $d900,x
                sta $da00,x
                sta $dae8,x ; nur bis 1000er-grenze
                inx
                bne -
                rts

; Constants:
SMC_MapMemPos = draw_map+1
SMC_ScrMemPos = draw_map+4
SMC_ColMemPos = draw_map+11
SMC_AttribsPos = draw_map+8
; Expects:
;  map width in MapWidth
;  map height in MapHeight
;  map mem pos in SMC_MapMemPos
;  scr mem pos in SMC_ScrMemPos
DrawMap         ; Put color mem addr to SMC_col_row_mem
                lda SMC_ScrMemPos
                sta SMC_ColMemPos
                lda SMC_ScrMemPos+1; add #$d4 to SMC_ScrMemPos
                clc
                adc #$d4
                sta SMC_ColMemPos+1
                ; Start loop
                ldx MapHeight
                dex
                stx counter
outer_draw      ldy MapWidth
                dey
draw_map        lda $FFFF,y; fill with map row mem pos
                sta $FFFF,y; fill with scr row mem pos
                ; Adjust color---------
                tax
                lda Colors,x
                sta $FFFF,y; fill with col row mem pos
                ;----------------------
                dey
                bpl draw_map

                ; Update SMC_map_row_mem+1/2
                lda SMC_MapMemPos
                clc
                adc MapWidth
                sta SMC_MapMemPos
                lda SMC_MapMemPos+1
                adc #0
                sta SMC_MapMemPos+1
                ; Update SMC_scr/col_row_mem+1/2
                lda SMC_ScrMemPos
                clc
                adc #40
                sta SMC_ScrMemPos
                sta SMC_ColMemPos
                bcc +
                inc SMC_ScrMemPos+1
                inc SMC_ColMemPos+1

+               dec counter
                bpl outer_draw
                
                rts


;Expects:
; map mem pos in $fb
; map width in MapWidth
; map height in MapHeight
; fixed color in MapColor
; upper left screen mem location in $fd
DrawMapFixedClr lda $fd
                pha
                sta $04
                lda $fe; add ($d800 - SCRMEM) to $fd/$fe
                pha
                clc
                adc #($d8 - >SCRMEM)
                sta $05
                ldx MapHeight
                dex
                stx counter
outer_draw1     lda MapWidth
                tay
                dey
draw_map1       lda ($fb),y
                sta ($fd),y
                ; Adjust color---------
                lda MapColor
                sta ($04),y 
                ;----------------------
                dey
                bpl draw_map1
                
                ; Update map mem pos
                lda MapWidth
                clc
                adc $fb
                sta $fb
                lda $fc
                adc #0
                sta $fc
                ; Update scr/col mem pos
                lda $fd
                clc
                adc #40
                sta $fd
                sta $04
                bcc +
                inc $fe
                inc $05
                
+               dec counter
                bpl outer_draw1
                pla
                sta $fe
                pla
                sta $fd
                rts
MapColor        !byte 0





!zone GameGraphics
PressYesBtn     lda bYesBtnPressed
                bne +
                ldx #6
                lda #0
-               sta $d99e,x
                dex
                bpl -

                sta $d9c6
                sta $d9ee

                ldx #7
                lda #1
                sta bYesBtnPressed
-               sta $da16,x
                dex
                bpl -

                sta $d9a5
                sta $d9cd
                sta $d9f5
                
                lda #32
                sta $05c8
                inc $05c9
                inc $05ca
                inc $05cb
                inc $05f1
                inc $05f2
                inc $05f3
+               rts

ReleaseYesBtn   lda bYesBtnPressed
                beq +
                ldx #6
                lda #1
-               sta $d99e,x
                dex
                bpl -

                sta $d9c6
                sta $d9ee

                ldx #7
                lda #0
                sta bYesBtnPressed
-               sta $da16,x
                dex
                bpl -

                sta $d9a5
                sta $d9cd
                sta $d9f5

                lda #169
                sta $05c8
                dec $05c9
                dec $05ca
                dec $05cb
                dec $05f1
                dec $05f2
                dec $05f3
+               rts

PressNoBtn      lda bNoBtnPressed
                bne +
                ldx #6
                lda #0
-               sta $d9a6,x
                dex
                bpl -

                sta $d9ce
                sta $d9f6

                ldx #7
                lda #1
                sta bNoBtnPressed
-               sta $da1e,x
                dex
                bpl -

                sta $d9ad
                sta $d9d5
                sta $d9fd

                inc $05d1
                inc $05d2
                inc $05f9
                inc $05fa
+               rts

ReleaseNoBtn    lda bNoBtnPressed
                beq +
                ldx #6
                lda #1
-               sta $d9a6,x
                dex
                bpl -

                sta $d9ce
                sta $d9f6

                ldx #7
                lda #0
                sta bNoBtnPressed
-               sta $da1e,x
                dex
                bpl -

                sta $d9ad
                sta $d9d5
                sta $d9fd

                dec $05d1
                dec $05d2
                dec $05f9
                dec $05fa
+               rts

IsInYesBtn      lda #0
                sta res
                lda MouseInfo
                cmp #15
                bcc +
                cmp #21
                bcs +
-               lda MouseInfo+1
                cmp #11
                bcc +
                cmp #13
                bcs +
                lda #1
                sta res
+               rts

IsInNoBtn       lda #0
                sta res
                lda MouseInfo
                cmp #23
                bcc +
                cmp #29
                bcs +
                jmp -
+               rts

GetMenuItem     lda #3
                sta modulo
                ldx MouseInfo+1
                dex
                txa
                jsr Mod
                dex
                dex
                dex
                stx res
                rts

DlgSpriteHandler
                lda GameMode
                cmp #GM_DIALOG
                beq +
                rts
+               lda DialogMode
                cmp #DM_SHUTDOWN
                bne +
                ; In shutdown dlg
                lda #SP_tvGray
                sta SPRPTR_2
                lda #CL_MIDGRAY
                sta col2
                lda #SP_tvBlack
                sta SPRPTR_3
                lda #CL_BLACK
                sta col3
                lda #113
                sta yPos2
                sta yPos3
                lda #104
                sta xPos2
                sta xPos3
                lda #SP_VertLine
                sta SPRPTR_4
                sta SPRPTR_5
                lda #CL_WHITE
                sta col4
                lda #CL_BLACK
                sta col5
                lda #65
                sta xPos4
                lda #151
                sta yPos4
                lda #248
                sta xPos5
                lda #90
                sta yPos5
                rts
+               ; Load dialog
                lda #SP_DriveGray
                sta SPRPTR_2
                lda #CL_DARKGRAY
                sta col2
                lda #SP_DriveBlack
                sta SPRPTR_3
                lda #CL_BLACK
                sta col3
                lda #105
                sta yPos2
                sta yPos3
                lda #119
                sta xPos2
                sta xPos3
                lda #SP_VertLine
                sta SPRPTR_4
                sta SPRPTR_5
                lda #CL_WHITE
                sta col4
                lda #CL_BLACK
                sta col5
                lda #81
                sta xPos4
                lda #119
                sta yPos4
                lda #232
                sta xPos5
                lda #90
                sta yPos5
                rts

ShowLoadDlg     lda #GM_DIALOG
                sta GameMode
                lda #DM_LOAD
                sta DialogMode
                lda #19
                sta MapWidth
                lda #6
                sta MapHeight
                lda #<LoadDlgMap
                sta SMC_MapMemPos
                lda #>LoadDlgMap
                sta SMC_MapMemPos+1
                lda #$d2
                sta SMC_ScrMemPos
                lda #$04
                sta SMC_ScrMemPos+1
                lda #<Colors
                sta SMC_AttribsPos
                lda #>Colors
                sta SMC_AttribsPos+1
                jsr DrawMap
                rts

ShowShutDownDlg lda #GM_DIALOG
                sta GameMode
                lda #DM_SHUTDOWN
                sta DialogMode
                lda #23
                sta MapWidth
                lda #10
                sta MapHeight
                lda #<ShutDownDlgMap
                sta SMC_MapMemPos
                lda #>ShutDownDlgMap
                sta SMC_MapMemPos+1
                lda #$d0
                sta SMC_ScrMemPos
                lda #$04
                sta SMC_ScrMemPos+1
                lda #<Colors
                sta SMC_AttribsPos
                lda #>Colors
                sta SMC_AttribsPos+1
                jsr DrawMap
                rts

KillStartMenu   lda #47
                ldx #18
-               sta $0568,x
                sta $0590,x
                sta $05b8,x
                sta $05e0,x
                sta $0608,x
                sta $0630,x
                sta $0658,x
                sta $0680,x
                sta $06a8,x
                sta $06d0,x
                sta $06f8,x
                sta $0720,x
                sta $0748,x
                dex
                bpl -
                lda #CL_DARKBLUE
                ldx #18
--              sta $d968,x
                sta $d990,x
                sta $d9b8,x
                sta $d9e0,x
                sta $da08,x
                sta $da30,x
                sta $da58,x
                sta $da80,x
                sta $daa8,x
                sta $dad0,x
                sta $daf8,x
                sta $db20,x
                sta $db48,x
                dex
                bpl --
                rts

KillShutDownDlg lda #47
                ldx #23
-               sta $04d0,x
                sta $04f8,x
                sta $0520,x
                sta $0548,x
                sta $0570,x
                sta $0598,x
                sta $05c0,x
                sta $05e8,x
                sta $0610,x
                sta $0638,x
                dex
                bpl -
                lda #CL_DARKBLUE
                ldx #23
--              sta $d8d0,x
                sta $d8f8,x
                sta $d920,x
                sta $d948,x
                sta $d970,x
                sta $d998,x
                sta $d9c0,x
                sta $d9e8,x
                sta $da10,x
                sta $da38,x
                dex
                bpl --
                rts

; Is called at line 0 50/60 times per second
; Checks whether cursor pointer is in StartMenu
; If yes, it 
MenuItems       !byte 112,136,160,185
HL_Offsets      !byte 3,16,17,43,56,57,83,96,97,123,136,137
Ycoords         !byte 10,13,16,19

Highlight       jsr IsInBarBox
                lda res
                bne +
                jsr CleanUp
                lda #$ff
                sta MenuItem
                rts
+               ; Get MenuItem number (0,1,2,3) in X
                lda #3
                sta modulo
                ldx MouseInfo+1
                dex
                txa
                jsr Mod
                dex
                dex
                dex

                ; Sprites
                lda #SP_Balken
                sta SPRPTR_6
                sta SPRPTR_7
                lda #CL_DARKBLUE
                sta col6
                sta col7
                lda MenuItems,x
                sta yPos6
                sta yPos7
                lda #56
                sta xPos6
                lda #104
                sta xPos7
                lda #%11000000
                sta VIC+27; behind chars
                sta VIC+23; stretch vert
                sta VIC+29; stretch horz

                ; Characters
                cpx MenuItem
                bne +
                rts
+               jsr CleanUp
                ldx MenuItem
                lda Ycoords,x
                tay
                lda ScrTabLo,y
                sta $fb
                lda ScrTabHi,y
                sta $fc
                
                ; Fill with blue and white
                lda #CL_WHITE
                jsr ChangeItemColor

                cpx #3
                bcs +
                ; For menu items 0,1,2
                ldx #8
-               lda HL_Offsets,x
                tay
                lda #47
                sta ($fb),y
                dex
                bpl -
                rts
+               ; For menu item 3
                ldx #2
-               lda HL_Offsets,x
                tay
                lda #111
                sta ($fb),y
                dex
                bpl -
                ldx #3
-               lda HL_Offsets,x
                tay
                lda #47
                sta ($fb),y
                inx
                cpx #9
                bcc -
                lda #112
                ldy #123
                sta ($fb),y
                lda #113
                ldy #136
                sta ($fb),y
                iny
                sta ($fb),y
                rts

CleanUp         ; Here, X is the new menu item
                txa
                ldx MenuItem; old one (that's supposed to be cleaned up for)
                sta MenuItem

                cpx #4
                bcc +
                rts
+               lda Ycoords,x
                tay
                lda ScrTabLo,y
                sta $fb
                lda ScrTabHi,y
                sta $fc

                lda #CL_BLACK
                jsr ChangeItemColor

                cpx #3
                bcs +
                ; For menu items 0,1,2
                ldx #8
-               lda HL_Offsets,x
                tay
                lda #32
                sta ($fb),y
                dex
                bpl -
                rts
+               ; For menu item 3
                ldx #2
-               lda HL_Offsets,x
                tay
                lda #45
                sta ($fb),y
                dex
                bpl -
                ldx #3
-               lda HL_Offsets,x
                tay
                lda #32
                sta ($fb),y
                inx
                cpx #9
                bcc -
                lda #31
                ldy #123
                sta ($fb),y
                lda #30
                ldy #136
                sta ($fb),y
                iny
                sta ($fb),y
                rts

; Changes the item's text color to A
; Needs:
; * A (color) and X (menu item) to be set
; * FBFC: item's scr mem location
color           !byte 0
ChangeItemColor sta color
                ; Get item's color mem location in FDFE
                lda $fb
                sta $fd
                lda $fc
                clc
                adc #$d4
                sta $fe
                ; At the right place (add 8 to it)
                lda $fd
                clc
                adc #8
                sta $fd
                lda $fe
                adc #0
                sta $fe

                cpx #3
                bcs +
                ; For menu items 0,1,2
                txa
                pha
                ldx #2
--              ldy #7
                lda color
-               sta ($fd),y
                dey
                bpl -
                ldy #40
                jsr AddYtoFDFE
                dex
                bpl --
                
                pla
                tax
                rts
+               ; For menu item 3
                ldy #40
                jsr AddYtoFDFE
                ldy #7
                lda color
-               sta ($fd),y
                dey
                bpl -
                ldy #40
                jsr AddYtoFDFE
                ldy #7
                lda color
-               sta ($fd),y
                dey
                bpl -
                rts

DrawSolitaire   lda #SP_SolitaireBlack
                sta SPRPTR_4
                lda #CL_BLACK
                sta col4
                lda #132
                sta yPos4
                sta yPos5
                lda #57
                sta xPos4
                sta xPos5
                lda #SP_SolitaireRed
                sta SPRPTR_5
                lda #CL_RED
                sta col5
                rts

DrawKing        lda #SP_KingBlack
                sta SPRPTR_4
                lda #CL_BLACK
                sta col4
                lda #156
                sta yPos4
                sta yPos5
                lda #55
                sta xPos4
                sta xPos5
                lda #SP_KingWhite
                sta SPRPTR_5
                lda #CL_ROSE
                sta col5
                rts

DrawMine        lda #SP_MineBlack
                sta SPRPTR_4
                lda #CL_BLACK
                sta col4
                lda #180
                sta yPos4
                sta yPos5
                lda #57
                sta xPos4
                sta xPos5
                lda #SP_MineWhite
                sta SPRPTR_5
                lda #CL_WHITE
                sta col5
                rts

Draw_TV         lda #SP_tvGray
                sta SPRPTR_4
                lda #CL_MIDGRAY
                sta col4
                lda #206
                sta yPos4
                sta yPos5
                lda #56
                sta xPos4
                sta xPos5
                lda #SP_tvBlack
                sta SPRPTR_5
                lda #CL_BLACK
                sta col5
                rts

Draw_W          lda #SP_W
                sta SPRPTR_2
                lda #204
                sta yPos2
                rts

Draw_ind        lda #SP_ind
                sta SPRPTR_3
                lda #182
                sta yPos3
                rts

Draw_do         lda #SP_do
                sta SPRPTR_2
                lda #161
                sta yPos2
                rts

Draw_ws         lda #SP_ws
                sta SPRPTR_3
                lda #140
                sta yPos3
                rts

Draw_s          lda #SP_s
                sta SPRPTR_2
                lda #119
                sta yPos2
                rts
                
IsInStartMenu   lda #0
                sta res
                jsr MouseToScr
                lda MouseInfo
                cmp #18
                bcs +
                lda MouseInfo+1
                cmp #10
                bcc +
                cmp #22
                bcs +
                lda #1
                sta res
+               rts

IsInBarBox      lda #0
                sta res
                jsr MouseToScr
                lda MouseInfo
                cmp #18
                bcs +
                cmp #3
                bcc +
                lda MouseInfo+1
                cmp #10
                bcc +
                cmp #22
                bcs +
                lda #1
                sta res
+               rts

IsInStartBtn    lda #0
                sta res
                lda MouseInfo+4
                bne +
                lda MouseInfo+2
                cmp #80
                bcs +
                cmp #26
                bcc +
                lda MouseInfo+3
                cmp #230
                bcc +
                cmp #249
                bcs +
                lda #1
                sta res
+               rts

DrawTaskbar     lda #40
                sta MapWidth
                lda #3
                sta MapHeight
                lda #<TaskbarMap
                sta SMC_MapMemPos
                lda #>TaskbarMap
                sta SMC_MapMemPos+1
                lda #$70
                sta SMC_ScrMemPos
                lda #$07
                sta SMC_ScrMemPos+1
                jsr DrawMap
                rts

DrawSpritesUp   lda #SP_L
                sta SPRPTR_2
                lda #CL_MIDGRAY
                sta col2
                lda #55
                sta xPos2
                lda #230
                sta yPos2
                lda #0
                sta VIC+29

                lda #SP_VertLine
                sta SPRPTR_3
                lda #CL_WHITE
                sta col3
                lda #3
                sta xPos3
                lda #230
                sta yPos3

                lda #SP_VertLine
                sta SPRPTR_4
                lda #CL_BLACK
                sta col4
                lda #56
                sta xPos4
                lda #230
                sta yPos4

                lda #SP_WinFlag
                sta SPRPTR_5
                lda #CL_BLACK
                sta col5
                lda #30
                sta xPos5
                lda #232
                sta yPos5

                lda #SP_FlagPart
                sta SPRPTR_6
                lda #CL_LIGHTBLUE
                sta col6
                lda #30
                sta xPos6
                lda #235
                sta yPos6

                lda #SP_FlagPart
                sta SPRPTR_7
                lda #CL_RED
                sta col7
                lda #30
                sta xPos7
                lda #232
                sta yPos7

                lda #%11111111
                sta VIC+21
                lda #0
                sta VIC+27; behind chars
                sta VIC+23; stretch vert
                rts

DrawButtonDown  lda #19
                sta MapWidth
                lda #3
                sta MapHeight
                lda #<StartBtnDownMap
                sta SMC_MapMemPos
                lda #>StartBtnDownMap
                sta SMC_MapMemPos+1
                lda #$70
                sta SMC_ScrMemPos
                lda #$07
                sta SMC_ScrMemPos+1
                jsr DrawMap
                jsr DrawSpritesDown
                ; Also the Start menu
                lda #19
                sta MapWidth
                lda #13
                sta MapHeight
                lda #<StartMenuMap
                sta SMC_MapMemPos
                lda #>StartMenuMap
                sta SMC_MapMemPos+1
                lda ScrTabLo+9
                sta SMC_ScrMemPos
                lda ScrTabHi+9
                sta SMC_ScrMemPos+1
                jsr DrawMap
                rts

DrawSpritesDown lda #SP_HorzLine
                sta SPRPTR_2
                lda #CL_WHITE
                sta col2
                lda #48
                sta xPos2
                lda #248
                sta yPos2
                lda #%00000100
                sta VIC+29

                lda #SP_VertLine
                sta SPRPTR_3
                lda #CL_BLACK
                sta col3
                lda #3
                sta xPos3
                lda #231
                sta yPos3

                lda #SP_VertLine
                sta SPRPTR_4
                lda #CL_WHITE
                sta col4
                lda #56
                sta xPos4
                lda #231
                sta yPos4

                lda #SP_WinFlag
                sta SPRPTR_5
                lda #CL_BLACK
                sta col5
                lda #31
                sta xPos5
                lda #233
                sta yPos5

                lda #SP_FlagPart
                sta SPRPTR_6
                lda #CL_LIGHTBLUE
                sta col6
                lda #31
                sta xPos6
                lda #236
                sta yPos6

                lda #SP_FlagPart
                sta SPRPTR_7
                lda #CL_RED
                sta col7
                lda #31
                sta xPos7
                lda #233
                sta yPos7

                lda #%11111111
                sta VIC+21
                lda #0
                sta VIC+27; behind chars
                sta VIC+23; stretch vert
                rts

!zone Math
AddYtoFBFC      tya
                clc
                adc $fb
                sta $fb
                lda $fc
                adc #0
                sta $fc
                rts

AddYtoFDFE      tya
                clc
                adc $fd
                sta $fd
                lda $fe
                adc #0
                sta $fe
                rts

modulo          !byte 0
;a <- a mod [modulo], x <- a div [modulo]
Mod             sec
                ldx #$ff
mod_loop        inx
                sbc modulo
                bcs mod_loop
                adc modulo
                rts

!zone MouseJoystickChoice
DrawChoiceMap   ldx #8
                ldy #15
                jsr PosToScrMem
                lda #10
                sta MapWidth
                lda #4
                sta MapHeight
                lda #1
                sta MapColor
                jsr DrawMapFixedClr
                rts
                
DrawJChoiceMap  lda #<JoyChoiceMap
                sta $fb
                lda #>JoyChoiceMap
                sta $fc
                jsr DrawChoiceMap
                rts

DrawMChoiceMap  lda #<MouseChoiceMap
                sta $fb
                lda #>MouseChoiceMap
                sta $fc
                jsr DrawChoiceMap
                rts

MouseJoyChoice  lda MouseOn
                bne +
                jsr DrawJChoiceMap
                jmp ++
+               jsr DrawMChoiceMap
++              lda #0
                sta 198
input_get       jsr $ffe4
                beq input_get
                cmp #13
                beq ok
                cmp #145
                beq up
                cmp #17
                beq down
                jmp input_get
up              jsr DrawJChoiceMap
                lda #0
                sta MouseOn
                jmp input_get
down            jsr DrawMChoiceMap
                lda #1
                sta MouseOn
                jmp input_get
ok              rts

!zone GameLoading
; Expects game no in A  ;
LoadGame        ldx GameIndex
                lda StrLen,x
                sta $fb
                ldy GameNamesHi,x
                lda GameNamesLo,x
                tax
                lda $fb
                ;LDA #fname_end-fname
                ;LDX #<fname
                ;LDY #>fname
                JSR $FFBD     ; call SETNAM
                LDA #$01
                LDX $BA       ; last used device number
                BNE skip
                LDX #$08      ; default to device 8
skip            LDY #$01      ; not $01 means: load to address stored in file
                JSR $FFBA     ; call SETLFS

                LDA #$00      ; $00 means: load to memory (not verify)
                JSR $FFD5     ; call LOAD
                BCS error     ; if carry set, a load error has happened
                RTS
error           ; Accumulator contains BASIC error code
                ; Most likely errors:
                ; A = $05 (DEVICE NOT PRESENT)
                ; A = $04 (FILE NOT FOUND)
                ; A = $1D (LOAD ERROR)
                ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
                RTS

!zone Data
MouseOn         !byte 1
GameNames       !text "SOLITAIRE","FREECELL","MINESWEEP"
StrLen          !byte 9,8,9
GameNamesLo     !byte 0,0,0
GameNamesHi     !byte 0,0,0
dummy           !byte 0
MenuItem        !byte $ff
StartBtnPushed  !byte 0
MouseInfo       !byte 0,0,0,0,0;MouseInfo: xScr,yScr,x,y,xHiByte
GameMode        !byte GM_NORMAL; 0: normal, 1: menu, 255: dialog
DialogMode      !byte 0; 0: shutdown, 1: wait
counter         !byte 0
exit_code       !byte 0
res             !byte 0 ;return value for various functions
MapWidth        !byte 0
MapHeight       !byte 0
bYesBtnPressed  !byte 0
bNoBtnPressed   !byte 0
GameIndex       !byte 0 ; Solitaire = 1, Freecell = 2, Minesweeper = 3
ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
                !byte $05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07
; Maps
TaskbarMap      !bin "Taskbar.bin"
StartBtnDownMap !bin "StartBtnDown.bin"
StartMenuMap    !bin "StartMenu.bin"
ShutDownDlgMap  !bin "ShutDownDlg.bin"
LoadDlgMap      !bin "LoadDlg.bin"
JoyChoiceMap    !bin "JoyChoiceMap.bin"
MouseChoiceMap  !bin "MouseChoiceMap.bin"
Colors          !bin "Attributes.bin"; per char, thus 256 bytes

*=$2000         
                !bin "chars.bin"

*=$2800
Mousepointer0Addr
!byte $c0,$00,$00,$a0,$00,$00,$90,$00
!byte $00,$88,$00,$00,$84,$00,$00,$82
!byte $00,$00,$8e,$00,$00,$a8,$00,$00
!byte $e4,$00,$00,$14,$00,$00,$12,$00
!byte $00,$12,$00,$00,$0c,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00

Mousepointer1Addr
!byte $c0,$00,$00,$e0,$00,$00,$f0,$00
!byte $00,$f8,$00,$00,$fc,$00,$00,$fe
!byte $00,$00,$fe,$00,$00,$f8,$00,$00
!byte $fc,$00,$00,$1c,$00,$00,$1e,$00
!byte $00,$1e,$00,$00,$0c,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

WinFlagAddr
!byte $00,$78,$00,$01,$fe,$00,$83,$b7
!byte $00,$af,$33,$00,$3f,$33,$00,$03
!byte $7b,$00,$03,$ff,$00,$03,$b7,$00
!byte $03,$33,$00,$03,$33,$00,$03,$7b
!byte $00,$83,$ff,$00,$af,$87,$00,$3e
!byte $01,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00

LAddr
!byte $00,$00,$00,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$7f,$ff,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00

VertLineAddr
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

FlagPartAddr
!byte $00,$00,$00,$00,$00,$00,$00,$40
!byte $00,$00,$c0,$00,$00,$c0,$00,$80
!byte $c0,$00,$ac,$c0,$00,$3c,$c0,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

HorzLineAddr
!byte $ff,$ff,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

WAddr
!byte $c0,$00,$00,$f8,$00,$00,$ff,$00
!byte $00,$ff,$e0,$00,$ff,$fc,$00,$0f
!byte $fc,$00,$01,$fc,$00,$07,$fc,$00
!byte $3f,$f0,$00,$ff,$80,$00,$fe,$00
!byte $00,$ff,$80,$00,$3f,$f0,$00,$07
!byte $fc,$00,$01,$fc,$00,$0f,$fc,$00
!byte $ff,$fc,$00,$ff,$e0,$00,$ff,$00
!byte $00,$f8,$00,$00,$c0,$00,$00,$01

indAddr
!byte $1f,$fc,$00,$1f,$fc,$00,$1f,$fc
!byte $00,$0f,$f8,$00,$03,$e0,$00,$00
!byte $00,$00,$0f,$fc,$00,$1f,$fc,$00
!byte $1f,$fc,$00,$1f,$fc,$00,$1c,$00
!byte $00,$0c,$00,$00,$1f,$fc,$00,$1f
!byte $fc,$00,$1f,$fc,$00,$1f,$fc,$00
!byte $00,$00,$00,$df,$fc,$00,$df,$fc
!byte $00,$df,$fc,$00,$df,$fc,$00,$01

doAddr
!byte $1f,$80,$00,$1c,$00,$00,$03,$e0
!byte $00,$0f,$f8,$00,$0f,$f8,$00,$1e
!byte $3c,$00,$1c,$1c,$00,$1c,$1c,$00
!byte $1c,$1c,$00,$1e,$3c,$00,$0f,$f8
!byte $00,$0f,$f8,$00,$03,$e0,$00,$00
!byte $00,$00,$ff,$fc,$00,$ff,$fc,$00
!byte $ff,$fc,$00,$ff,$fc,$00,$1c,$1c
!byte $00,$18,$0c,$00,$1c,$1c,$00,$01

wsAddr
!byte $19,$8c,$00,$1f,$9c,$00,$1f,$b8
!byte $00,$0f,$38,$00,$0f,$30,$00,$00
!byte $00,$00,$1c,$00,$00,$1f,$80,$00
!byte $1f,$f0,$00,$1f,$fc,$00,$07,$fc
!byte $00,$01,$fc,$00,$07,$fc,$00,$1f
!byte $e0,$00,$1f,$80,$00,$1f,$e0,$00
!byte $07,$fc,$00,$01,$fc,$00,$07,$fc
!byte $00,$1f,$fc,$00,$1f,$f0,$00,$01

sAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$70,$00
!byte $0c,$f8,$00,$1d,$f8,$00,$1d,$dc
!byte $00,$1d,$8c,$00,$19,$8c,$00,$01

tvGrayAddr
!byte $00,$00,$00,$07,$00,$00,$1f,$c0
!byte $00,$27,$f3,$80,$39,$fc,$c0,$2e
!byte $7e,$20,$23,$99,$60,$20,$e7,$60
!byte $20,$37,$60,$20,$17,$60,$20,$17
!byte $60,$20,$17,$60,$20,$17,$40,$38
!byte $17,$00,$3e,$17,$00,$0f,$97,$00
!byte $03,$f6,$00,$00,$f4,$00,$00,$30
!byte $00,$00,$00,$00,$00,$00,$00,$0c

tvBlackAddr
!byte $07,$00,$00,$18,$c0,$00,$20,$3f
!byte $80,$58,$0c,$60,$46,$03,$30,$51
!byte $81,$d0,$5c,$66,$90,$5f,$18,$90
!byte $5f,$c8,$90,$5f,$e8,$90,$5f,$e8
!byte $90,$5f,$e8,$90,$5f,$e8,$a0,$47
!byte $e8,$c0,$41,$e8,$80,$30,$68,$80
!byte $0c,$09,$00,$03,$0a,$00,$00,$cc
!byte $00,$00,$30,$00,$00,$00,$00,$00

MineBlackAddr
!byte $00,$00,$00,$00,$70,$00,$00,$f8
!byte $00,$1b,$06,$c0,$1c,$01,$c0,$08
!byte $00,$80,$10,$01,$c0,$10,$00,$c0
!byte $20,$61,$e0,$60,$92,$f0,$60,$95
!byte $70,$60,$62,$f0,$20,$05,$e0,$10
!byte $aa,$c0,$19,$55,$c0,$0f,$ab,$80
!byte $1f,$ff,$c0,$1b,$fe,$c0,$00,$f8
!byte $00,$00,$70,$00,$00,$00,$00,$00

MineWhiteAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$f0,$00,$03,$a0,$00,$05
!byte $40,$00,$0e,$00,$00,$0c,$00,$00
!byte $18,$00,$00,$10,$00,$00,$10,$00
!byte $00,$10,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

KingBlackAddr
!byte $1f,$ff,$fc,$10,$55,$58,$0d,$55
!byte $50,$05,$00,$20,$02,$3f,$e0,$7e
!byte $40,$30,$81,$80,$28,$81,$1d,$e8
!byte $81,$26,$70,$81,$02,$28,$e7,$02
!byte $28,$99,$02,$28,$81,$07,$28,$81
!byte $18,$a8,$81,$57,$36,$81,$00,$29
!byte $81,$08,$a9,$81,$ca,$a6,$81,$3a
!byte $d8,$ff,$05,$00,$00,$00,$00,$00

KingWhiteAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $3f,$c0,$00,$7f,$c0,$00,$e2,$00
!byte $00,$d1,$00,$00,$fd,$c0,$00,$fd
!byte $c0,$00,$fd,$c0,$00,$f8,$c0,$00
!byte $e7,$40,$00,$a8,$c0,$00,$ff,$c0
!byte $00,$f0,$40,$00,$30,$40,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

SolitaireBlackAddr
!byte $03,$ff,$80,$07,$ff,$c0,$0f,$ff
!byte $c0,$10,$01,$c0,$10,$01,$c0,$10
!byte $01,$c0,$10,$01,$e0,$10,$01,$90
!byte $3f,$3f,$08,$61,$21,$90,$ae,$dd
!byte $60,$a8,$05,$20,$68,$05,$20,$28
!byte $05,$20,$28,$05,$20,$28,$05,$20
!byte $28,$05,$20,$28,$05,$40,$28,$05
!byte $80,$2f,$fd,$00,$00,$00,$00,$00

SolitaireRedAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$02,$08,$00,$07
!byte $1c,$00,$07,$1c,$00,$02,$08,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

BalkenAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$06

DriveGrayAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $1f,$ff,$c0,$20,$00,$40,$40,$00
!byte $c0,$80,$01,$c0,$80,$01,$c0,$80
!byte $01,$c0,$88,$01,$c0,$88,$01,$c0
!byte $88,$01,$c0,$88,$01,$c0,$80,$01
!byte $80,$ff,$ff,$00,$00,$00,$00,$0c

DriveBlackAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$20,$00,$00
!byte $20,$00,$00,$20,$00,$00,$20,$00
!byte $00,$20,$00,$00,$20,$1f,$fc,$20
!byte $00,$00,$20,$00,$00,$20,$00,$00
!byte $40,$00,$00,$80,$7f,$ff,$00,$01
