!to "MINESWEEP.d64",d64

!zone Constants
VIC_BANK = $4000
SCRMEM = $4800
CHARSET = $4000
std_irq = $ea31
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
col0  = VIC+39
col1  = VIC+40
col2  = VIC+41
col3  = VIC+42
col4  = VIC+43
col5  = VIC+44
col6  = VIC+45
col7  = VIC+46
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
CL_DARKGREY = 11
CL_MIDGREY = 12
CL_LIGHTGREEN = 13
CL_LIGHTBLUE = 14
CL_LIGHTGREY = 15
SPRPTR_0 = SCRMEM+1016
SPRPTR_1 = SCRMEM+1017
SPRPTR_2 = SCRMEM+1018
SPRPTR_3 = SCRMEM+1019
SPRPTR_4 = SCRMEM+1020
SPRPTR_5 = SCRMEM+1021
SPRPTR_6 = SCRMEM+1022
SPRPTR_7 = SCRMEM+1023
DM_ABOUT = 0
DM_INSTRUCTS = 1
DM_STATS = 2
GM_NORMAL = 0
GM_MENU = 1
GM_DIALOG = $ff
MM_GAME = 0
MM_HELP = 1
GM_MI_NEW = 0
GM_MI_STATS = 1
GM_MI_LARGE = 2
GM_MI_EXIT = 3
HM_MI_HELP = 0
HM_MI_ABOUT = 1
SM_SMALL = 0
SM_LARGE = 1
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
; Sprite Blocks
SP_Mouse0 = (Mousepointer0Addr - VIC_BANK)/64
SP_Mouse1 = (Mousepointer1Addr - VIC_BANK)/64
SP_HorizontalLine = (HorizontalLineAddr - VIC_BANK)/64
SP_VerticalLine = (VerticalLineAddr - VIC_BANK)/64
SP_HelpMenuRight = (HelpMenuRightAddr - VIC_BANK)/64


!zone BASIC_stub
;*=$0801
;;** BASIC-Zeile: 0 SYS2061
; !word main-2, 0
; !byte $9e
; !text "2061"
; !byte $00,$00,$00

!zone VIC
; Character set
*=$4000
CharacterSet    !bin "Chars.bin"

; Screen memory at $4800

; Some data
*=$4c00
MouseOn         !byte 1

; 5 Sprites (free space for 4 more)
*=$4c40
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

HorizontalLineAddr
!byte $ff,$ff,$ff,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

VerticalLineAddr
!byte $80,$00,$00,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$80,$00,$00
!byte $80,$00,$00,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$80,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

HelpMenuRightAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$ff,$ff,$ff,$01

!zone Main
*=$4e00
                jsr SetGlobals
                jsr InstallIRQ
                jsr LoadStats
StartGame       jsr Initialize
                jsr ClearScreen
                jsr ShowMainWindow
                jsr DistributeMines
                jsr AddAroundMines

!zone MainLoop
MainLoop        lda exit_code
                beq MainLoop
                pha
                lda #0
                sta exit_code
                pla
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
                cmp #EC_GAMEEXIT
                beq Exit
                cmp #EC_WON
                beq Won
                cmp #EC_RESTART
                beq Restart
                cmp #EC_DBLCLICK
                beq DblClick
                jmp MainLoop

DblClick        jsr OnDblClick
                jmp MainLoop

RBtnRelease     jsr OnRBtnRelease
                jmp MainLoop

RBtnPress       jsr OnRBtnPress
                jmp MainLoop

Exit            ; Shut off ext bkg mode
                lda $d011
                and #%10111111
                sta $d011
                
                jsr DefaultKernal
                lda #0
                sta 198
                sta VIC+21
                lda #21
                sta $d018
                
                ; VIC bank 0
                lda $dd02
                ora #%00000011
                sta $dd02
                lda $dd00
                and #%11111100
                ora #%00000011
                sta $dd00

                ldy #CL_LIGHTBLUE
                jsr ClearScreen
                jsr RestoreIRQ
                rts

LBtnRelease     jsr OnLBtnRelease
                jmp MainLoop

Moved           jsr OnMouseMove
                jmp MainLoop

LBtnPress       jsr OnLBtnPress
                jmp MainLoop
                
Restart         jmp StartGame

Won             lda #0
                sta TimerOn
                ; Smile face
                ldx #7
-               lda CHARSET+$2e0,x
                sta CHARSET,x
                dex
                bpl -
                ; Times
                jsr UpdateTimes
                lda SizeMode
                beq +
                jsr ShowStatsDlgLar
                jmp ++
+               jsr ShowStatsDlgSma
++              lda InBesties
                beq ++
                jsr SaveStats
                lda #0
                sta InBesties
++              jmp MainLoop
;=============================================
!zone ____________Events______________
;============================================= OnDblClick
OnDblClick      lda GameMode
                bne +
                jsr IsInBoard
                lda res
                beq +
                jsr BoardClick
+               rts

;============================================= OnButtonPress
; Right Button------------------------------------------------
OnRBtnPress     jsr GetMouseInfo
                rts

; Left Button-------------------------------------------------
OnLBtnPress     jsr GetMouseInfo
                
                lda GameMode
                cmp #GM_MENU
                beq +
                cmp #GM_DIALOG
                beq ++
                rts
                
+               lda SelMenuIndex
                sta ClickedMenuIndex
                rts

++              jsr IsInDlgBtn
                lda res
                beq +
                jsr PressDlgBtn
+               rts

;============================================= OnButtonRelease
; Right Button------------------------------------------------
OnRBtnRelease   lda GameMode
                bne +
                ; Only in normal mode
                jsr IsInBoard
                lda res
                beq +
                ; Right clicked in board
                jsr BoardRightClick
+               rts

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
RelInDlgMode2   lda IsDlgBtnPressed
                beq +
                lda #0
                sta IsDlgBtnPressed
                jsr ReleaseDlgBtn
                jsr IsInDlgBtn
                lda res
                beq +
                jsr Pause
                jsr GotoNormalMode
+               rts

;----- In menu mode --------------------------
RelInMenuMode2  lda MenuMode
                bne MM_HelpMenu
                ; Possibly (!) Clicked in Game Menu
                jsr IsInGameMenu
                lda res
                beq GotoNormalMode
            ;GM_Action
                jsr GotoNormalMode
                jsr GM_Actions
                rts
MM_HelpMenu     ; Possibly (!) Clicked in Help Menu
                jsr IsInHelpMenu
                lda res
                beq GotoNormalMode
            ;HM_Action
                jsr GotoNormalMode
                jsr HM_Actions
                rts
GotoNormalMode  lda #%00000011
                sta VIC+21
                jsr RestoreScreen
                ;jsr display_time
                lda #GM_NORMAL
                sta GameMode
                rts

;----- In normal mode ------------------------
RelInNormalMode2
                jsr MouseToScr
                jsr IsInWindow
                lda res
                beq leave
                jsr IsInGame; (Menu Game)
                lda res
                beq +
                jsr ShowGameMenu
                rts
+               jsr IsInHelp; (Help menu)
                lda res
                beq +
                jsr ShowHelpMenu
                rts
+               jsr IsInSmiley
                lda res
                beq +
                lda #EC_RESTART
                sta exit_code
+               jsr IsInBoard
                lda res
                beq leave
                ;Pressed in board
                lda MouseOn
                bne +
                jsr BoardRightClick
                rts
+               jsr BoardClick
leave           rts

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
                jsr IsInDlgBtn
                lda res
                bne +
                lda IsDlgBtnPressed
                beq ++
                jsr ReleaseDlgBtn
                rts
+               lda IsDlgBtnPressed
                bne ++
                jsr PressDlgBtn
++              rts

;----- In normal mode ------------------------
MovInNormalMode lda bFirePressed
                bne fire_is_pressed
                ;
                rts
fire_is_pressed rts

;----- In menu mode --------------------------
MovInMenuMode   lda MenuMode
                bne moved_HM_shown
;moved_GM_shown
                jsr IsInGameMenu
                lda res
                bne moved_in_GM
                rts
moved_in_GM     jsr GM_SelMenuItem
                rts
moved_HM_shown  jsr IsInHelpMenu
                lda res
                bne moved_in_HM
                rts
moved_in_HM     jsr HM_SelMenuItem
                rts
;=============================================

!zone MenuActions
GM_Actions      lda ClickedMenuIndex
                cmp #GM_MI_NEW
                beq ACTION_NEW
                cmp #GM_MI_STATS
                beq ACTION_STATS
                cmp #GM_MI_LARGE
                beq ACTION_LARGE
                cmp #GM_MI_EXIT
                beq ACTION_EXIT
ACTION_NEW      lda #EC_RESTART
                sta exit_code
                rts
ACTION_STATS    lda SizeMode
                beq +
                jsr ShowStatsDlgLar
                rts
+               jsr ShowStatsDlgSma
                rts
ACTION_LARGE    jsr ChangeSize
                rts
ACTION_EXIT     lda #EC_GAMEEXIT
                sta exit_code
                rts
HM_Actions      lda ClickedMenuIndex
                cmp #HM_MI_HELP
                beq ACTION_HELP
                cmp #HM_MI_ABOUT
                beq ACTION_ABOUT
ACTION_HELP     ;lda #0
                ;sta TimerOn
                lda SizeMode
                beq +
                jsr ShowInstrDlgLar
                rts
+               jsr ShowInstrDlgSma
                rts
ACTION_ABOUT    ;lda #0
                ;sta TimerOn
                lda SizeMode
                beq +
                jsr ShowAboutDlgLar
                rts
+               jsr ShowAboutDlgSma
                rts

!zone MenuRoutines
ShowGameMenu    lda #GM_MENU
                sta GameMode
                lda #MM_GAME
                sta MenuMode
                lda #255
                sta SelMenuIndex
                jsr SaveScreen
                ;  Invert "Game"
                ldx #1
                ldy #0
                jsr WindowToScrClrMem
                ldy #3
invert_game     lda ($fb),y
                clc
                adc #128
                sta ($fb),y
                lda #1
                sta ($fd),y
                dey
                bpl invert_game
                ; Manipulate character set
                ldx #127
-               lda CharacterSet+$200,x
                sta CharacterSet+88,x
                dex
                bpl -
                ; Boundary sprites
                lda #SP_HorizontalLine
                sta SPRPTR_2
                sta SPRPTR_3
                sta SPRPTR_4
                sta SPRPTR_5
                lda #SP_VerticalLine
                sta SPRPTR_6
                sta SPRPTR_7
                lda #12
                sta col2
                sta col3
                lda #0
                sta col4
                sta col5
                sta col6
                lda #1
                sta col7
                ; stretch
                lda #%00010100
                sta VIC+29; stretch X
                lda #%01000000
                sta VIC+23; stretch Y
                ; Positions
                jsr GetWndSprPos
                ; Distribute positions
                ldx #0
-               lda GM_SprPositions,x
                clc
                adc WndSprPosX
                sta $d004,x
                bcc +
                lda $d010
                ora HiBytes,x
                sta $d010
                jmp ++
+               lda HiBytes,x
                eor #%11111111
                and $d010
                sta $d010
++              inx
                lda GM_SprPositions,x
                clc
                adc WndSprPosY
                sta $d004,x
                inx
                cpx #12
                bcc -
                ; Turn 'em all on
                lda #$ff
                sta VIC+21
DrawGameMenuMap lda $fb; FBFC = scrmem of 'G' in "Game"
                clc
                adc #40
                sta $fd
                lda $fc
                adc #0
                sta $fe
                lda #<GameMenuMap
                sta $fb
                lda #>GameMenuMap
                sta $fc
                lda #<GameMenuAttribs
                sta $02
                lda #>GameMenuAttribs
                sta $03
                lda #9
                sta MapWidth
                lda #5
                sta MapHeight
                jsr DrawMap
                rts


ShowHelpMenu    lda #GM_MENU
                sta GameMode
                lda #MM_HELP
                sta MenuMode
                lda #255
                sta SelMenuIndex
                jsr SaveScreen
                ;  Invert "Help"
                ldy #5
                ldx #1
                jsr WindowToScrClrMem
                ldy #0
                lda ($fb),y
                clc
                adc #128
                sta ($fb),y
                lda #1
                sta ($fd),y
                ; Manipulate character set
                ldx #127
-               lda CharacterSet+$200,x
                sta CharacterSet+88,x
                dex
                bpl -
                ; Boundary sprites
                lda #SP_HorizontalLine
                sta SPRPTR_2
                sta SPRPTR_3
                sta SPRPTR_4
                sta SPRPTR_5
                sta SPRPTR_6
                lda #SP_HelpMenuRight
                sta SPRPTR_7
                lda #12
                sta col2
                sta col3
                sta col4
                sta col5
                lda #0
                sta col6
                sta col7
                ; stretch
                lda #%01010100
                sta VIC+29; stretch X
                lda #%00000000
                sta VIC+23; stretch Y
                ; Positions
                jsr GetWndSprPos
                ; Distribute positions
                ldx #0
-               lda HM_SprPositions,x
                clc
                adc WndSprPosX
                sta $d004,x
                bcc +
                lda $d010
                ora HiBytes,x
                sta $d010
                jmp ++
+               lda HiBytes,x
                eor #%11111111
                and $d010
                sta $d010
++              inx
                lda HM_SprPositions,x
                clc
                adc WndSprPosY
                sta $d004,x
                inx
                cpx #12
                bcc -
                ; Turn 'em all on
                lda #$ff
                sta VIC+21
DrawHelpMenuMap lda $fb; scrmem of "?"
                clc
                adc #40
                sta $fd
                lda $fc
                adc #0
                sta $fe
                lda #<HelpMenuMap
                sta $fb
                lda #>HelpMenuMap
                sta $fc
                lda #<HelpMenuAttribs
                sta $02
                lda #>HelpMenuAttribs
                sta $03
                lda #7
                sta MapWidth
                lda #2
                sta MapHeight
                jsr DrawMap
                rts


;Expects menu item index in SelMenuItem
HM_SelMenuItem  ; obtain selected menu index
                jsr MouseToScr
                lda MouseInfo+1
                sec
                sbc WindowPos+1
                sec
                sbc #3
                cmp SelMenuIndex
                bne highlight1
                rts
highlight1      sta SelMenuIndex
                ldy #5
                ldx #1
                jsr WindowToScrClrMem
                jsr DrawHelpMenuMap
                ldy #5
                ldx SelMenuIndex
                inx
                inx
                jsr WindowToScrClrMem
                ldy #6
-               lda ($fb),y
                sec
                sbc #64
                sta ($fb),y
                dey
                bpl -
                ldy #5
-               lda #1
                sta ($fd),y
                dey
                bne -
                rts

;Expects menu item index in SelMenuIndex
GM_SelMenuItem  ; obtain selected menu index
                jsr MouseToScr
                lda MouseInfo+1
                sec
                sbc WindowPos+1
                sec
                sbc #3
                cmp SelMenuIndex
                bne highlight
                rts
highlight       sta SelMenuIndex
                ldy #0
                ldx #1
                jsr WindowToScrClrMem
                jsr DrawGameMenuMap
                ldy #0
                ldx SelMenuIndex
                inx
                inx
                jsr WindowToScrClrMem
                ldy #8
-               lda ($fb),y
                sec
                sbc #64
                sta ($fb),y
                dey
                bpl -
                ldy #7
                lda #1
-               sta ($fd),y
                dey
                bne -
                rts

IsInGameMenu    lda #0
                sta res
                jsr MouseToScr
                lda WindowPos
                cmp MouseInfo
                bcs +
                clc
                adc #9
                cmp MouseInfo
                bcc +
                lda WindowPos+1
                clc
                adc #2
                cmp MouseInfo+1
                bcs +
                clc
                adc #4
                cmp MouseInfo+1
                bcc +
                lda #1
                sta res
+               rts
                
IsInHelpMenu    lda #0
                sta res
                jsr MouseToScr
                lda WindowPos
                clc
                adc #5
                cmp MouseInfo
                bcs +
                clc
                adc #7
                cmp MouseInfo
                bcc +
                lda WindowPos+1
                clc
                adc #2
                cmp MouseInfo+1
                bcs +
                clc
                adc #2
                cmp MouseInfo+1
                bcc +
                lda #1
                sta res
+               rts

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
                jsr IsInWindow
                lda dx
                ldx GameMode
                bmi +
                ldx res
                beq +
                jmp ++
+               asl
                ;asl
++              clc
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
change_y        jsr IsInWindow
                lda dy
                ldx GameMode
                bmi +
                ldx res
                beq +
                jmp ++
+               asl
                ;asl
++              clc
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
acc      = 128     ; accelaration (fastest: 1)
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
;---------------------------------------

!zone Preparations
;Once called, never changes
SetGlobals      jsr ConfigKernal

                ; MultiColor off
                lda $d016
                and #%11101111
                sta $d016

                jsr DisableSTOP

                ; VIC bank 1
                lda $dd02
                ora #%00000011
                sta $dd02
                lda $dd00
                and #%11111100
                ora #%00000010
                sta $dd00
                
                ; Character set at $4000 and screen memory at $4800
                lda #%00100000
                sta $d018

                ; Install ext bkg mode
                lda $d011
                ora #%01000000
                sta $d011
                lda #0
                sta $d020
                lda #6; dark blue as 1st bkg color
                sta $d021
                lda #1; white as 2nd bkg color
                sta $d022
                lda #15; light gray as 3rd bkg color
                sta $d023
                lda #0; black as 4th bkg color
                sta $d024
                
                ;Install mouse pointer sprites
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

                ; Prepare ScrTabLo/Hi
                ldx #0
                lda #<SCRMEM
                sta ScrTabLo,x
                lda #>SCRMEM
                sta ScrTabHi,x
fill_scrtab     lda ScrTabLo,x
                clc
                adc #40
                inx
                sta ScrTabLo,x
                dex
                lda ScrTabHi,x
                adc #0
                inx
                sta ScrTabHi,x
                cpx #24
                bcc fill_scrtab
                
                ; Get Mem pos of "OK" in character set
                lda #<(CHARSET+496)
                sta OkMemPos
                lda #>(CHARSET+496)
                sta OkMemPos+1
                rts

Initialize      lda #0
                sta TimerOn
                lda #240
                sta CurTime
                sta CurTime+1
                sta CurTime+2
                lda #255
                sta SelMenuIndex
                lda #1
                sta CanPlay
                ; Normal game mode
                lda #GM_NORMAL
                sta GameMode
                ; Show sprites
                lda #%00000011
                sta VIC+21
                ; Zero out board and mines
                ldx #208
                lda #0
-               sta Board,x
                dex
                bne -
                sta Board
                ldx #22
-               sta Mines,x
                dex
                bpl -
                ; Bring neutral smiley into char set
                ldx #7
-               lda CHARSET+$2d8,x
                sta CHARSET,x
                dex
                bpl -
                ; Size-related settings
                lda SizeMode
                beq +
                ; Large window
                ldx #4
-               lda SmallText,x
                sta GameMenuMap+19,x
                dex
                bpl -
                lda #8
                sta WindowPos
                lda #2
                sta WindowPos+1
                lda #11
                sta BoardHeight
                lda #19
                sta BoardWidth
                lda #209
                sta BoardSize
                lda #23
                sta MinesTotal
                lda #<Mult19
                sta MultBoardWidth
                lda #>Mult19
                sta MultBoardWidth+1
                lda #23
                sta FlagsToSet
                lda #186
                sta CellsToOpen
                rts
+               ; Small window
                ldx #4
-               lda LargeText,x
                sta GameMenuMap+19,x
                dex
                bpl -
                lda #13
                sta WindowPos
                lda #3
                sta WindowPos+1
                lda #9
                sta BoardHeight
                sta BoardWidth
                lda #81
                sta BoardSize
                lda #10
                sta MinesTotal
                lda #<Mult9
                sta MultBoardWidth
                lda #>Mult9
                sta MultBoardWidth+1
                lda #10
                sta FlagsToSet
                lda #71
                sta CellsToOpen
                rts
SmallText       !byte 17,10,9,20,20
LargeText       !byte 13,9,14,15,4

!zone IRQ
CIA_IRQ         jsr RequestTimer
                ; Mouse and Joystick
                lda MouseOn
                bne mouse
                jsr Joystick
                jmp std_irq
mouse           jsr Mouse
                jmp std_irq


RequestTimer    lda TimerOn; Every 1/60 second
                bne DoTimer
                rts
DoTimer         dec Timer
                beq TimerAction
                rts
TimerAction     lda #60; Every second
                sta Timer
                
                ldy #2
                lda CurTime,y
                tax
                inx
                cpx #250
                beq +
                txa
                sta CurTime,y
                jmp UpdTimeDisplay
+               lda #240
                sta CurTime,y
                
                ldy #1
                lda CurTime,y
                tax
                inx
                cpx #250
                beq +
                txa
                sta CurTime,y
                jmp UpdTimeDisplay
+               lda #240
                sta CurTime,y
                
                ldy #0
                lda CurTime,y
                tax
                inx
                cpx #250
                beq +
                txa
                sta CurTime,y
                jmp UpdTimeDisplay
+               lda #0
                sta TimerOn
                rts

UpdTimeDisplay  lda GameMode
                cmp #GM_MENU
                bne +
                lda SizeMode
                bne +
                rts
+               ldy #7
                lda SizeMode
                beq +
                ldy #17
+               ldx #3
                jsr WindowToScrClrMem
                ldy #2
-               lda CurTime,y
                sta ($fb),y
                dey
                bpl -
                rts

InstallIRQ      sei
                lda #<CIA_IRQ
                sta $0314
                lda #>CIA_IRQ
                sta $0315
                cli
                rts

RestoreIRQ      sei
                lda #$31
                sta $0314
                lda #$ea
                sta $0315
                cli
                rts

!zone ConnectingGameAndGraphics
DisplayFlagsNo  ldy #1
                ldx #3
                jsr WindowToScrClrMem
                lda FlagsToSet
                bmi +
                jsr ByteToDeciChar
                lda #240
                ldy #0
                sta ($fb),y
                jmp ++
+               ; Flags no is negative
                eor #%11111111
                tax
                inx
                txa
                jsr ByteToDeciChar
                lda #252
                ldy #0
                sta ($fb),y
++              lda DigitHi
                clc
                adc #192
                iny
                sta ($fb),y
                lda DigitLo
                clc
                adc #192
                iny
                sta ($fb),y
                rts

BoardRightClick lda CanPlay
                bne +
                rts
+               lda MultBoardWidth
                sta $02
                lda MultBoardWidth+1
                sta $03
                jsr GetMouseInfo
                jsr MouseToCellInfo
                ldx CellIndex
                lda Board,x
                and #%01000000
                beq +
                rts
+               lda Board,x
                eor #%00100000
                sta Board,x
                and #%00100000
                bne +
                ; Remove flag
                inc FlagsToSet
                lda #161
                jmp ++
+               ; Set flag
                dec FlagsToSet
                jsr CheckWon
                lda #170
++              ldy #0
                sta ($fb),y
                jsr DisplayFlagsNo
                rts

BoardClick      lda CanPlay
                bne +
                rts
+               lda #1
                sta TimerOn
                lda MultBoardWidth
                sta $02
                lda MultBoardWidth+1
                sta $03
                jsr MouseToCellInfo
                ldx CellIndex
                lda Board,x
                and #%01100000
                beq +
                rts
+               ; Neither open nor flagged
                lda Board,x
                bpl +
                ; It's a mine ------------> PLAYER LOSES
                jsr ShowMines
                lda #0
                sta TimerOn
                sta CanPlay
                ldx #7
-               lda CHARSET+$2e8,x
                sta CHARSET,x
                dex
                bpl -
                rts
+               ; It's not a mine
                jsr OpenCell
                ldx CellIndex
                lda Board,x
                cmp #%01000000
                bne +; just a number in cell
                jsr Tirade
+               rts

MouseToCellInfo lda MouseInfo+1
                sec
                sbc #6
                sec
                sbc WindowPos+1
                sta Row
                tay
                lda ($02),y; multiply Y with 9 or 19
                pha
                
                lda MouseInfo
                sec
                sbc #2
                sec
                sbc WindowPos
                sta Col
                
                pla
                clc
                adc Col
                sta CellIndex
                jsr CellToScrColMem
                rts

GoUp            dec Row
                lda CellIndex
                sec
                sbc BoardWidth
                sta CellIndex
                lda $fb
                sec
                sbc #40
                sta $fb
                lda $fc
                sbc #0
                sta $fc
                rts

GoDown          inc Row
                lda CellIndex
                clc
                adc BoardWidth
                sta CellIndex
                lda $fb
                clc
                adc #40
                sta $fb
                lda $fc
                adc #0
                sta $fc
                rts

GoRight         inc Col
                inc CellIndex
                inc $fb
                bne +
                inc $fc
+               rts

GoLeft          dec Col
                dec CellIndex
                lda $fb
                bne +
                dec $fc
+               dec $fb
                rts

; Opens three cells horizontally
; starts and ends in center
OpenThreeCells  jsr OpenCell
                ; W
                lda Col
                beq +
                jsr GoLeft
                jsr OpenCell
                jsr GoRight
                ; E
                ldx Col
                inx
                cpx BoardWidth
                beq ++
+               jsr GoRight
                jsr OpenCell
                jsr GoLeft
++              rts

OpenAdjCells    ; N
                lda Row
                beq +
                jsr GoUp
                jsr OpenThreeCells
                jsr GoDown
                ; S
                ldx Row
                inx
                cpx BoardHeight
                beq ++
+               jsr GoDown
                jsr OpenThreeCells
                jsr GoUp
++              ; W
                lda Col
                beq +
                jsr GoLeft
                jsr OpenCell
                jsr GoRight
                ; E
                ldx Col
                inx
                cpx BoardWidth
                beq ++
+               jsr GoRight
                jsr OpenCell
                jsr GoLeft
++              rts

; Expexts Col and Row filled
; Output: SrcMem in FBFC
CellToScrColMem ldy Col
                iny
                ldx Row
                inx
                inx
                inx
                inx
                inx
                jsr WindowToScrClrMem
                rts

; Expects CellIndex filled
GetCellInfo     lda BoardWidth
                sta modulo
                lda CellIndex
                jsr Mod
                ; CellIndex = X * BoardWidth + A
                stx Row
                sta Col
                jsr CellToScrColMem
                rts

; Expects CellIndex and FBFC filled
OpenCell        ldx CellIndex
                lda Board,x
                bmi ++; if mine
                and #%01000000
                bne ++; if already open
                lda Board,x
                and #%00100000
                bne ++; if flagged
                lda Board,x
                clc
                adc #33
                ldy #0
                sta ($fb),y
                lda Board,x
                ora #%01000000
                sta Board,x
                cmp #%01000000
                bne +
                ; if empty (not a number or flag)
                ; append to list
                lda CellIndex
                ldx ListIndex
                sta CellsList,x
                inc ListIndex
+               dec CellsToOpen
                jsr CheckWon
++              rts

Tirade          ldx ListIndex
-               dex
                lda CellsList,x
                sta CellIndex
                jsr GetCellInfo
                dec ListIndex
                jsr OpenAdjCells
                ldx ListIndex
                bne -
                rts

ShowMines       lda BoardWidth
                sta modulo
                lda MinesTotal
                sta counter
                dec counter
                
-               ldy counter
                lda Mines,y
                jsr Mod
                ;Mines,counter = X * BoardWidth + A
                ;Y = A+1
                ;X = X+5
                tay
                iny
                inx
                inx
                inx
                inx
                inx
                jsr WindowToScrClrMem
                lda #43
                ldy #0
                sta ($fb),y
                lda #2
                sta ($fd),y
                
                dec counter
                bpl -
                rts

!zone Graphics
ClearScreen     ldx #0
                ldy #CL_DARKBLUE
-               lda #32
                sta SCRMEM,x
                sta SCRMEM+$100,x
                sta SCRMEM+$200,x
                sta SCRMEM+$2e8,x ; nur bis 1000er-grenze
                inx
                bne -
                rts

; Fills WndSprPosX/Y
GetWndSprPos    lda WindowPos
                clc
                adc #4
                asl
                asl
                rol
                sta WndSprPosX
                lda #0
                adc #0
                sta WndSprPosX+1
                lda WindowPos+1
                tax
                inx
                txa
                asl
                asl
                asl
                clc
                adc #50
                sta WndSprPosY
                rts


; Expects (y,x) as local window coords
; Output: scrmem pointer in FBFC,
;         colormem pointer in FDFE
WindowToScrClrMem
                tya
                clc
                adc WindowPos
                tay
                iny
                ;---
                txa
                clc
                adc WindowPos+1
                tax
                inx
                lda ScrTabLo,x
                sta $fb
                lda ScrTabHi,x
                sta $fc
                jsr AddYtoFBFC
                ; Now the color
                lda $fb
                sec
                sbc #<SCRMEM
                sta $fd
                lda $fc
                sbc #>SCRMEM
                sta $fe
                lda $fe
                clc
                adc #$d8
                sta $fe
                rts

ShowMainWindow  lda SizeMode
                beq +
                jsr ShowMainWindowLarge
                rts
+               jsr ShowMainWindowSmall
                rts

ShowMainWindowSmall
                lda #<MainWindowMap
                sta $fb
                lda #>MainWindowMap
                sta $fc
                lda #<MainWndAttribs
                sta $02
                lda #>MainWndAttribs
                sta $03
                lda #13
                sta MapWidth
                lda #17
                sta MapHeight
                ldy WindowPos
                ldx WindowPos+1
                jsr PosToScrMem
                jsr DrawMap
                rts

ShowMainWindowLarge
                lda #<MainLWndMap
                sta $fb
                lda #>MainLWndMap
                sta $fc
                lda #<MainLWndAttribs
                sta $02
                lda #>MainLWndAttribs
                sta $03
                lda #23
                sta MapWidth
                lda #19
                sta MapHeight
                ldy WindowPos
                ldx WindowPos+1
                jsr PosToScrMem
                jsr DrawMap
                rts

IsInWindow      lda #0
                sta res
                jsr GetMouseInfo
                lda WindowPos
                cmp MouseInfo
                bcs +
                clc
                adc BoardWidth
                clc
                adc #2
                cmp MouseInfo
                bcc +
                lda WindowPos+1
                cmp MouseInfo+1
                bcs +
                clc
                adc BoardHeight
                clc
                adc #6
                cmp MouseInfo+1
                bcc +
                lda #1
                sta res
+               rts

IsInGame        lda #0
                sta res
                lda WindowPos
                cmp MouseInfo
                bcs +
                clc
                adc #4
                cmp MouseInfo
                bcc +
                lda WindowPos+1
                clc
                adc #2
                cmp MouseInfo+1
                bne +
                lda #1
                sta res
+               rts

IsInHelp        lda #0
                sta res
                lda WindowPos
                clc
                adc #6
                cmp MouseInfo
                bne +
                lda WindowPos+1
                clc
                adc #2
                cmp MouseInfo+1
                bne +
                lda #1
                sta res
+               rts

IsInSmiley      lda #0
                sta res
                lda WindowPos
                clc
                adc #6
                ldx SizeMode
                beq +
                clc
                adc #5
+               cmp MouseInfo
                bne +
                lda WindowPos+1
                clc
                adc #4
                cmp MouseInfo+1
                bne +
                lda #1
                sta res
+               rts

IsInBoard       lda #0
                sta res
                lda WindowPos
                clc
                adc #1
                cmp MouseInfo
                bcs +
                clc
                adc BoardWidth
                cmp MouseInfo
                bcc +
                lda WindowPos+1
                clc
                adc #5
                cmp MouseInfo+1
                bcs +
                clc
                adc BoardHeight
                cmp MouseInfo+1
                bcc +
                lda #1
                sta res
+               rts

!zone GeneralGraphics
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

SaveScreen      ldx #0
-               lda SCRMEM,x
                sta $c400,x
                lda SCRMEM+$100,x
                sta $c500,x
                lda SCRMEM+$200,x
                sta $c600,x
                lda SCRMEM+$2e8,x ; nur bis 1000er-grenze
                sta $c6e8,x
                ; colors
                lda $d800,x
                sta $c800,x
                lda $d900,x
                sta $c900,x
                lda $da00,x
                sta $ca00,x
                lda $dae8,x ; nur bis 1000er-grenze
                sta $cae8,x
                inx
                bne -
                rts
                
RestoreScreen   ldx #0
-               lda $c400,x
                sta SCRMEM,x
                lda $c500,x
                sta SCRMEM+$100,x
                lda $c600,x
                sta SCRMEM+$200,x
                lda $c6e8,x
                sta SCRMEM+$2e8,x ; nur bis 1000er-grenze
                lda $c800,x
                sta $d800,x
                lda $c900,x
                sta $d900,x
                lda $ca00,x
                sta $da00,x
                lda $cae8,x
                sta $dae8,x ; nur bis 1000er-grenze
                inx
                bne -
                jsr UpdTimeDisplay
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

; Draws map in extended bkg mode
; Expects:
;  map mem pos in $fb
;  map attribs pos in $02 (One attrib: [bkg code(0-3)|col] nybble-wise)
;  map width in MapWidth
;  original map width in MapWidthOrig
;  map height in MapHeight
;  upper left screen mem location in $fd
DrawMapPart     ; Color pos in 04/05
                lda $fd
                sta $04
                lda $fe
                clc
                adc #($d8 - >SCRMEM)
                sta $05
                ; Start loop
                ldx MapHeight
                dex
                stx counter
outer_draw      ldy MapWidth
                dey
draw_map        lda ($02),y
                and #%11110000
                asl
                asl
                clc
                adc ($fb),y
                sta ($fd),y
                ; Adjust color---------
                lda ($02),y
                and #%00001111
                sta ($04),y
                ;----------------------
                dey
                bpl draw_map
                
                ldy MapWidthOrig
                jsr AddYtoFBFC
                jsr AddYto0203
                ldy #40
                jsr AddYtoFDFE
                jsr AddYto0405
                
                dec counter
                bpl outer_draw
                rts

; Expects:
;  map mem pos in $fb
;  map attribs pos in $02
;  map width in MapWidth
;  map height in MapHeight
;  upper left screen mem location in $fd
DrawMap         lda MapWidth
                sta MapWidthOrig
                jsr DrawMapPart
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
                
                ldy MapWidth
                jsr AddYtoFBFC
                ldy #40
                jsr AddYtoFDFE
                jsr AddYto0405
                
                dec counter
                bpl outer_draw1
                pla
                sta $fe
                pla
                sta $fd
                rts
MapColor        !byte 0

!zone DialogRoutines
PressDlgBtn     lda #1
                sta IsDlgBtnPressed
                ; Manipulate "OK" in character set
                lda OkMemPos
                sta $fb
                lda OkMemPos+1
                sta $fc
                ldy #15
-               lda ($fb),y
                lsr
                sta ($fb),y
                dey
                bpl -
                ldy #15
-               dey
                lda ($fb),y
                iny
                sta ($fb),y
                dey
                bne -
                ; Draw map
                ldx DialogMode
                lda OkBtnCoordsX,x
                ldy SizeMode
                beq +
                clc
                adc #5
+               tay
                dey
                lda OkBtnCoordsY,x
                tax
                dex
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda #<OkButtonMap
                sta $fb
                lda #>OkButtonMap
                sta $fc
                lda #<OkBtnPressAttr
                sta $02
                lda #>OkBtnPressAttr
                sta $03
                lda #6
                sta MapWidth
                lda #3
                sta MapHeight
                jsr DrawMap
                rts

ReleaseDlgBtn   lda #0
                sta IsDlgBtnPressed
                ; Manipulate "OK" in character set
                lda OkMemPos
                sta $fb
                lda OkMemPos+1
                sta $fc
                ldy #15
-               lda ($fb),y
                asl
                sta ($fb),y
                dey
                bpl -
                ldy #1
-               iny
                lda ($fb),y
                dey
                sta ($fb),y
                iny
                cpy #15
                bne -
                ldy #0
                lda ($fb),y
                ldy #15
                sta ($fb),y
                ; Draw map
                ldx DialogMode
                lda OkBtnCoordsX,x
                ldy SizeMode
                beq +
                clc
                adc #5
+               tay
                dey
                lda OkBtnCoordsY,x
                tax
                dex
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda #<OkButtonMap
                sta $fb
                lda #>OkButtonMap
                sta $fc
                lda #<OkBtnRelAttribs
                sta $02
                lda #>OkBtnRelAttribs
                sta $03
                lda #6
                sta MapWidth
                lda #3
                sta MapHeight
                jsr DrawMap
                rts

IsInDlgBtn      lda #0
                sta res
                ldx DialogMode
                lda WindowPos
                clc
                adc OkBtnCoordsX,x
                ldy SizeMode
                beq +
                clc
                adc #5
+               cmp MouseInfo
                bcs +
                clc
                adc #4
                cmp MouseInfo
                bcc +
                lda WindowPos+1
                clc
                adc OkBtnCoordsY,x
                tax
                inx
                txa
                cmp MouseInfo+1
                bne +
                lda #1
                sta res
+               rts

Pause           ldx #32
                lda #0
                sta dummy
-               dec dummy
                bne -
                dex
                bne -
                rts

ShowStatsDlgLar jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_STATS
                sta DialogMode
                ; Manipulate character set
                ldx #127
-               lda CharacterSet+$200,x
                sta CharacterSet+88,x
                dex
                bpl -
                ldx #31
                ldy #136
                jsr ShowLargeDlgSpr
                ; Draw map
                ldy #4
                ldx #5
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda #<(BestsDlgMap+16)
                sta $fb
                lda #>(BestsDlgMap+16)
                sta $fc
                lda #<(BestsDlgAttribs+16)
                sta $02
                lda #>(BestsDlgAttribs+16)
                sta $03
                lda #15
                sta MapWidthOrig
                lda #13
                sta MapWidth
                lda #6
                sta MapHeight
                jsr DrawMapPart
                ; Fill in times
                ldy #8
                ldx #7
                jsr WindowToScrClrMem
                ldx #0
-               lda Stats_L_Time0,x
                clc
                adc #176
                ldy #0
                sta ($fb),y
                inx
                lda Stats_L_Time0,x
                lsr
                lsr
                lsr
                lsr
                clc
                adc #176
                iny
                sta ($fb),y
                lda Stats_L_Time0,x
                and #%00001111
                clc
                adc #176
                iny
                sta ($fb),y
                ldy #40
                jsr AddYtoFBFC
                inx
                cpx #6
                bne -
                rts

ShowStatsDlgSma jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_STATS
                sta DialogMode
                ; Manipulate character set
                ldx #127
-               lda CharacterSet+$200,x
                sta CharacterSet+88,x
                dex
                bpl -
                lda #72
                jsr ShowSmallDlgSpr
                ; Draw map
                ldy #254 ; = -2
                ldx #4
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda #<BestsDlgMap
                sta $fb
                lda #>BestsDlgMap
                sta $fc
                lda #<BestsDlgAttribs
                sta $02
                lda #>BestsDlgAttribs
                sta $03
                lda #15
                sta MapWidth
                lda #7
                sta MapHeight
                jsr DrawMap
                ; Fill in times
                ldy #3
                ldx #7
                jsr WindowToScrClrMem
                ldx #0
-               lda Stats_S_Time0,x
                clc
                adc #176
                ldy #0
                sta ($fb),y
                inx
                lda Stats_S_Time0,x
                lsr
                lsr
                lsr
                lsr
                clc
                adc #176
                iny
                sta ($fb),y
                lda Stats_S_Time0,x
                and #%00001111
                clc
                adc #176
                iny
                sta ($fb),y
                ldy #40
                jsr AddYtoFBFC
                inx
                cpx #6
                bne -
                rts

ShowAboutDlgLar jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_ABOUT
                sta DialogMode
                ; Manipulate character set
                ldx #127
-               lda CharacterSet+$280,x
                sta CharacterSet+88,x
                dex
                bpl -
                ldx #15
                ldy #152
                jsr ShowLargeDlgSpr
                ; Draw map
                ldy #2
                ldx #5
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda #<(AboutDlgMap+20)
                sta $fb
                lda #>(AboutDlgMap+20)
                sta $fc
                lda #<(AboutDlgAttribs+20)
                sta $02
                lda #>(AboutDlgAttribs+20)
                sta $03
                lda #19
                sta MapWidthOrig
                lda #17
                sta MapWidth
                lda #6
                sta MapHeight
                jsr DrawMapPart
                rts
                
ShowAboutDlgSma jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_ABOUT
                sta DialogMode
                ; Manipulate character set
                ldx #127
-               lda CharacterSet+$280,x
                sta CharacterSet+88,x
                dex
                bpl -
                lda #72
                jsr ShowSmallDlgSpr
                ; Draw map
                ldy #252 ; = -4
                ldx #4
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda #<AboutDlgMap
                sta $fb
                lda #>AboutDlgMap
                sta $fc
                lda #<AboutDlgAttribs
                sta $02
                lda #>AboutDlgAttribs
                sta $03
                lda #19
                sta MapWidth
                lda #7
                sta MapHeight
                jsr DrawMap
                rts

ShowInstrDlgSma jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_INSTRUCTS
                sta DialogMode
                ; Manipulate character set
                ldx #127
                lda MouseOn
                beq next
-               lda CharacterSet+$300,x
                sta CharacterSet+88,x
                dex
                bpl -
                jmp ++
next            lda CharacterSet+$380,x
                sta CharacterSet+88,x
                dex
                bpl next
++              lda #72
                jsr ShowSmallDlgSpr
                ; Draw map
                ldy #250 ; = -6
                ldx #4
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda MouseOn
                beq +
                lda #<InstrDlgMap_M
                sta $fb
                lda #>InstrDlgMap_M
                sta $fc
                jmp ++
+               lda #<InstrDlgMap_J
                sta $fb
                lda #>InstrDlgMap_J
                sta $fc
++              lda #<InstrDlgAttribs
                sta $02
                lda #>InstrDlgAttribs
                sta $03
                lda #23
                sta MapWidth
                lda #8
                sta MapHeight
                jsr DrawMap
                rts

ShowInstrDlgLar jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_INSTRUCTS
                sta DialogMode
                ; Manipulate character set
                ldx #127
                lda MouseOn
                beq next2
-               lda CharacterSet+$300,x
                sta CharacterSet+88,x
                dex
                bpl -
                jmp ++
next2           lda CharacterSet+$380,x
                sta CharacterSet+88,x
                dex
                bpl next2
++              ; Draw map
                ldy #0
                ldx #5
                jsr WindowToScrClrMem
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda MouseOn
                beq +
                lda #<(InstrDlgMap_M+24)
                sta $fb
                lda #>(InstrDlgMap_M+24)
                sta $fc
                jmp ++
+               lda #<(InstrDlgMap_J+24)
                sta $fb
                lda #>(InstrDlgMap_J+24)
                sta $fc
++              lda #<(InstrDlgAttribs+24)
                sta $02
                lda #>(InstrDlgAttribs+24)
                sta $03
                lda #23
                sta MapWidthOrig
                lda #21
                sta MapWidth
                lda #7
                sta MapHeight
                jsr DrawMapPart
                ; Missing char
                ldy #0
                ldx #4
                jsr WindowToScrClrMem
                ldy #0
                lda #155
                sta ($fb),y
                lda #145
                jsr ShowSmallDlgSpr
                rts

ShowLargeDlgSpr tya
                pha
                txa
                pha
                lda #SP_VerticalLine
                sta SPRPTR_2
                sta SPRPTR_3
                sta SPRPTR_4
                sta SPRPTR_5
                lda #1
                sta col2
                sta col3
                lda #12
                sta col4
                sta col5
                ; stretch
                lda #0
                sta VIC+29; stretch X
                lda #%00010100
                sta VIC+23; stretch Y
                ; Positions
                jsr GetWndSprPos
                pla
                clc
                adc WndSprPosX
                sta $d004
                sta $d006
                bcc +
                lda $d010
                ora #%00010100
                sta $d010
+               pla
                clc
                adc WndSprPosX
                sta $d008
                sta $d00a
                bcc +
                lda $d010
                ora #%00101000
                sta $d010
+               lda #39
                clc
                adc WndSprPosY
                sta $d005
                sta $d009
                adc #32
                sta $d007
                sta $d00b
                ; Turn it on
                lda #%00111111
                sta VIC+21
                rts

; Expects relative spr pos in A
ShowSmallDlgSpr pha
                lda #SP_HorizontalLine
                sta SPRPTR_2
                lda #1
                sta col2
                ; stretch
                lda #0
                sta VIC+29; stretch X
                sta VIC+23; stretch Y
                ; Position
                jsr GetWndSprPos
                pla
                clc
                adc WndSprPosX
                sta $d004
                bcc +
                lda $d010
                ora #%00000100
                sta $d010
+               lda #39
                clc
                adc WndSprPosY
                sta $d005
                ; Turn it on
                lda #%00000111
                sta VIC+21
                rts

!zone Math
; Expects byte in A
; Converts a byte to a displayable number
; in terms of two digits, DigitLo and DigitHi
; Ex.: A = $13
; DigitLo = "9", DigitHi = "1" (both as chars)
ByteToDeciChar  ldx #10
                stx modulo
                jsr Mod
                clc
                adc #48
                sta DigitLo
                txa
                clc
                adc #48
                sta DigitHi
                rts

                
;AddYXtoFBFC     tya
;                clc
;                adc $fb
;                sta $fb
;                txa
;                adc $fc
;                sta $fc
;                rts
                
AddYto0203      tya
                clc
                adc $02
                sta $02
                lda $03
                adc #0
                sta $03
                rts
                
AddYto0405      tya
                clc
                adc $04
                sta $04
                lda $05
                adc #0
                sta $05
                rts
                
AddYtoFBFC      tya
                clc
                adc $fb
                sta $fb
                lda $fc
                adc #0
                sta $fc
                rts
;
;AddXtoFBFC      txa
;                clc
;                adc $fb
;                sta $fb
;                lda $fc
;                adc #0
;                sta $fc
;                rts
                
AddYtoFDFE      tya
                clc
                adc $fd
                sta $fd
                lda $fe
                adc #0
                sta $fe
                rts

;SubtractYFromFDFE  
;                sty factor
;                lda $fd
;                sec
;                sbc factor
;                sta $fd
;                lda $fe
;                sbc #0
;                sta $fe
;                rts

modulo          !byte 0
;a <- a mod [modulo], x <- a div [modulo]
Mod             sec
                ldx #$ff
mod_loop        inx
                sbc modulo
                bcs mod_loop
                adc modulo
                rts

Random          lda $dc04  ;Low-Byte  von Timer A aus dem CIA-1
                eor $dc05  ;High-Byte von Timer A aus dem CIA-1
                eor $dd04  ;Low-Byte  von Timer A aus dem CIA-2
                adc $dd05  ;High-Byte von Timer A aus dem CIA-2
                eor $dd06  ;Low-Byte  von Timer B aus dem CIA-2
                eor $dd07  ;High-Byte von Timer B aus dem CIA-2
                rts

!zone Kernal
ConfigKernal    lda #0
                sta $9d; no kernal messages
DisableSTOP     LDA #234
                STA $0328
                rts
                
                
DefaultKernal   lda #128
                sta $9d; kernal messages on
EnableSTOP      LDA #237
                STA $0328
                rts
                
                
lname           !text "STATS,P,R"
lname_end
sname           !text "@0:STATS"
sname_end

LoadStats       ; Turn off screen
                lda $d011
                and #%11101111
                sta $d011

                LDA #lname_end-lname
                LDX #<lname
                LDY #>lname
                JSR $FFBD     ; call SETNAM
                LDA #$01
                LDX $BA       ; last used device number
                BNE skip1
                LDX #$08      ; default to device 8
skip1           LDY #$01      ; not $01 means: load to address stored in file
                JSR $FFBA     ; call SETLFS

                LDA #$00      ; $00 means: load to memory (not verify)
                JSR $FFD5     ; call LOAD
                BCS eror      ; if carry set, a load error has happened
                 
tos             ; Turn on screen
                lda $d011
                ora #%00010000
                sta $d011
                RTS
eror            ; Accumulator contains BASIC error code
                ; most likely errors:
                ; A = $05 (DEVICE NOT PRESENT)
                ; A = $04 (FILE NOT FOUND)
                ; A = $1D (LOAD ERROR)
                ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
                jmp tos
                RTS


SaveStats       jsr EnableSTOP

                LDA #sname_end-sname
                LDX #<sname
                LDY #>sname
                JSR $FFBD     ; call SETNAM
                LDX $BA       ; last used device number
                BNE skip
                LDX #$08      ; default to device 8
skip            LDY #$01
                LDA #$00
                JSR $FFBA     ; call SETLFS

                LDA #<Stats_S_Time0
                STA $C1
                LDA #>Stats_S_Time0
                STA $C2

                LDX #<Time
                LDY #>Time
                LDA #$C1      ; start address located in $C1/$C2
                JSR $FFD8     ; call SAVE
                BCS error     ; if carry set, a save error has happened
                jsr DisableSTOP
                RTS
error           ; Akkumulator contains BASIC error code
                jsr DisableSTOP
                RTS


UpdateTimes     ; Fill Time
                ldy #7
                lda SizeMode
                beq +
                ldy #17
+               ldx #3
                ;jsr WindowToScrClrMem
                ldy #0
                ;lda ($fb),y
                lda CurTime,y
                sec
                sbc #240
                sta Time
                iny
                ;lda ($fb),y
                lda CurTime,y
                sec
                sbc #240
                asl
                asl
                asl
                asl
                sta dummy
                iny
                ;lda ($fb),y
                lda CurTime,y
                sec
                sbc #240
                ora dummy
                sta Time+1
                ;if time < time0 then time2 = time1 : time1 = time0 : time0 = time
                ;if time < time1 then time2 = time1 : time 1 = time
                ;if time < time2 then time2 = time
                lda SizeMode
                beq +
                lda #<Stats_L_Time0
                sta $fb
                lda #>Stats_L_Time0
                sta $fc      
                jmp ++
+               lda #<Stats_S_Time0
                sta $fb
                lda #>Stats_S_Time0
                sta $fc

++              ldy #0
                jsr IsTimeSmallerThanFBFCandY
                lda res
                bne best
                
                ldy #2
                jsr IsTimeSmallerThanFBFCandY
                lda res
                bne second_best
                
                ldy #4
                jsr IsTimeSmallerThanFBFCandY
                lda res
                bne third_best
                
                rts
                
best            ; time2 = time1
                ldy #2
                lda ($fb),y
                ldy #4
                sta ($fb),y
                ldy #3
                lda ($fb),y
                ldy #5
                sta ($fb),y
                ; time1 = time0
                ldy #0
                lda ($fb),y
                ldy #2
                sta ($fb),y
                ldy #1
                lda ($fb),y
                ldy #3
                sta ($fb),y
                ; time0 = time
                lda Time
                ldy #0
                sta ($fb),y
                lda Time+1
                ldy #1
                sta ($fb),y
                sty InBesties
                rts

second_best     ; time2 = time1
                ldy #2
                lda ($fb),y
                ldy #4
                sta ($fb),y
                ldy #3
                lda ($fb),y
                ldy #5
                sta ($fb),y
                ; time1 = time
                lda Time
                ldy #2
                sta ($fb),y
                lda Time+1
                ldy #3
                sta ($fb),y
                sty InBesties
                rts

third_best      ; time2 = time
                lda Time
                ldy #4
                sta ($fb),y
                lda Time+1
                ldy #5
                sta ($fb),y
                sty InBesties
                rts

; Expects 16 bit value in ($FBFC) (hi/lo !!!!)
; and an even value for Y (0,2,4,...)
IsTimeSmallerThanFBFCandY
                lda #0
                sta res
                lda Time
                cmp ($fb),y
                bcc is_smaller
                beq hi_equal
                rts
hi_equal        lda Time+1
                iny
                cmp ($fb),y
                bcc is_smaller1
                dey
                rts
is_smaller1     dey
is_smaller      lda #1
                sta res
                rts

!zone Game
CheckWon        lda FlagsToSet
                bne +
                lda CellsToOpen
                bne +
                lda #EC_WON
                sta exit_code
+               rts

ChangeSize      lda SizeMode
                eor #%00000001
                sta SizeMode
                bne +
                jsr $e544; clear screen
+               lda #EC_RESTART
                sta exit_code
                rts

DistributeMines ; Generate mines
                lda BoardSize
                sta modulo
                ldy #0
--              jsr Random
                jsr Mod
                sty dummy
                beq +
                ; check if number is valid
                pha
                tya
                tax
                pla
                dex
-               cmp Mines,x
                beq --
                dex
                bpl -
                ; -------------------
+               sta Mines,y
                iny
                cpy MinesTotal
                bcc --
                ; Set mines in board
                ldx MinesTotal
                dex
-               lda Mines,x
                tay
                lda #%10000000
                sta Board,y
                dex
                bpl -
                rts

AddAroundMine   lda BoardWidth
                sta modulo
                lda mine
                jsr Mod
                ; mine = X * BoardWidth + A
                stx div
                sta mod
                ; N
                cpx #0
                beq ++
                lda mine
                sec
                sbc BoardWidth
                tax
                inc Board,x
                ; NW
                dex
                lda mod
                beq +
                inc Board,x
+               ; NE
                ldy mod
                iny
                cpy BoardWidth
                beq ++
                inx
                inx
                inc Board,x
++              ; S
                ldy div
                iny
                cpy BoardHeight
                bcs ++
                lda mine
                clc
                adc BoardWidth
                tax
                inc Board,x
                ; SW
                dex
                lda mod
                beq +
                inc Board,x
+               ; SE
                ldy mod
                iny
                cpy BoardWidth
                beq ++
                inx
                inx
                inc Board,x
++              ; W
                lda mod
                beq +
                ldx mine
                dex
                inc Board,x
+               ; E
                ldy mod
                iny
                cpy BoardWidth
                beq +
                ldx mine
                inx
                inc Board,x
+               rts
mine            !byte 0
div             !byte 0
mod             !byte 0

AddAroundMines  ldx MinesTotal
                dex
-               lda Mines,x
                sta mine
                txa
                pha
                jsr AddAroundMine
                pla
                tax
                dex
                bpl -
                rts

!zone Data
; Game related -----------
; Rules for board:
; 0-8: number of adjacent mines
; %1000xxxx: mine
; 6th bit on/off: cell open/closed
; 5th bit on/off: flagged yes/no
Board           !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Mines           !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
BoardWidth      !byte 9
BoardHeight     !byte 9
BoardSize       !byte 81
MinesTotal      !byte 10
Mult9           !byte 0,9,18,27,36,45,54,63,72
Mult19          !byte 0,19,38,57,76,95,114,133,152,171,190
MultBoardWidth  !byte 0,0
Col             !byte 0
Row             !byte 0
CellIndex       !byte 0
FlagsToSet      !byte 10
CellsToOpen     !byte 71
CanPlay         !byte 1
InBesties       !byte 0
CurTime         !byte 240,240,240
; To save on disk --(HI/LO)----
Stats_S_Time0   !byte $09,$99 ;best small
                !byte $09,$99 ;2nd best small
                !byte $09,$99 ;3rd best small
Stats_L_Time0   !byte $09,$99 ;best large
                !byte $09,$99 ;2nd best large
                !byte $09,$99 ;3rd best large
;----------------------------
Time            !byte 0,0
SizeMode        !byte 0
IsDlgBtnPressed !byte 0
WindowPos       !byte 13,3
DigitLo         !byte 0; For conversion
DigitHi         !byte 0; byte to decimal
SelMenuIndex    !byte 255
ClickedMenuIndex!byte 255
dummy           !byte 0,0
counter         !byte 0
TimerOn         !byte 1
Timer           !byte 60; counts down from 60 to 0
WndSprPosX      !byte 0,0
WndSprPosY      !byte 0
exit_code       !byte 0
;-------------------------------
res             !byte 0 ;return value for various functions
MouseInfo       !byte 0,0,0,0,0;MouseInfo: xScr,yScr,x,y,xHiByte
GameMode        !byte GM_NORMAL; 0: normal, 1: menu, 255: dialog
MenuMode        !byte 0; 0: MM_GAME, 1: MM_HELP
DialogMode      !byte 0;0: about dlg, 1: intructs dlg, 2: stats dlg
MapWidth        !byte 0
MapWidthOrig    !byte 0
MapHeight       !byte 0
ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
                !byte $05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07
;ColorTabHi      !byte $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9
;                !byte $d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db
GM_SprPositions !byte 0,15,48,15,3,49,49,49,72,18,7,50
HM_SprPositions !byte 40,15,72,15,40,32,72,32,42,33,73,13;+40,+15
HiBytes         !byte 4,0,8,0,16,0,32,0,64,0,128,0
OkBtnCoordsX    !byte 9,11,7
OkBtnCoordsY    !byte 7,7,7
OkMemPos        !byte 0,0



MainWindowMap   !bin "MainWindowMap.bin"
MainWndAttribs  !bin "MainWndAttribs.bin"
GameMenuMap     !bin "GameMenuMap.bin"
GameMenuAttribs !bin "GameMenuAttribs.bin"
HelpMenuMap     !bin "HelpMenuMap.bin"
HelpMenuAttribs !bin "HelpMenuAttribs.bin"
AboutDlgMap     !bin "AboutDlgMap.bin"
AboutDlgAttribs !bin "AboutDlgAttribs.bin"
OkButtonMap     !bin "OkButtonMap.bin"
OkBtnRelAttribs !bin "OkButtonRelAttribs.bin"
OkBtnPressAttr  !bin "OkButtonPressAttribs.bin"
InstrDlgMap_M   !bin "InstructsDlgMap_M.bin"
InstrDlgMap_J   !bin "InstructsDlgMap_J.bin"
InstrDlgAttribs !bin "InstructsDlgAttribs.bin"
BestsDlgMap     !bin "BestiesDlgMap.bin"
BestsDlgAttribs !bin "BestiesDlgAttribs.bin"
MainLWndMap     !bin "MainWndLargeMap.bin"
MainLWndAttribs !bin "MainWndLargeAttribs.bin"

; List of cells to process
ListIndex       !byte 0
CellsList       !byte 0
