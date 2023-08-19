!to "FREECELL.d64",d64

!zone Constants
VIC_BANK = $4000
SCRMEM = $4800
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
DM_EXIT = 0
DM_WON = 1
DM_STATS = 2
DM_TOOMANYCARDS = 3
DM_INSTRUCTS = 4
DM_ABOUT = 5
DM_NEW = 6
DM_THANKS = 7
GM_NORMAL = 0
GM_MENU = 1
GM_DIALOG = $ff
MM_GAME = 0
MM_HELP = 1
GM_MI_NEW = 0
GM_MI_UNDO = 1
GM_MI_STATS = 2
GM_MI_EXIT = 3
HM_MI_HELP = 0
HM_MI_ABOUT = 1
;Sprite Blocks
SP_Mouse0 = (Mousepointer0Addr - VIC_BANK)/64
SP_Mouse1 = (Mousepointer1Addr - VIC_BANK)/64
SP_MenuItemHighliter = (MenuItemHighliterAddr - VIC_BANK)/64
SP_Separator = (SeparatorAddr - VIC_BANK)/64


!zone BASIC_stub
;*=$0801
;;** BASIC-Zeile: 0 SYS19968
; !word main-2, 0
; !byte $9e ; SYS
; !text "19968"
; !byte $00,$00,$00

!zone VIC
; Character set
*=$4000         
!bin "Atari.bin"

; Screen memory at $4800

; Some data
*=$4c00
MouseOn         !byte 1

; 4 Sprites (free space for 4 more)
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

MenuItemHighliterAddr
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00

SeparatorAddr
!byte $a0,$00,$00,$a0,$00,$00,$a0,$00
!byte $00,$a0,$00,$00,$a0,$00,$00,$a0
!byte $00,$00,$a0,$00,$00,$a0,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

!zone Main
*=$4e00
                jsr SetGlobals
                jsr InstallIRQ
                jsr LoadStats
StartGame       jsr Initialize
                jsr ClearScreen
                jsr DrawMenu
                jsr DrawRepAces
                jsr ShuffleDeck
                jsr DeckToPiles
                jsr StartRattle
                jsr DrawPiles

!zone MainLoop
;Exit codes:
; 00000001 dbl click
; 00000010 btn press
; 00000100 btn release
; 00001000 mouse moved
; 00010000 exit program to BASIC
; 00100000 player has won the game
; 01000000 restart game 
MainLoop        lda bRestart
                asl
                ora bWon
                asl
                ora bExit
                asl
                ora bMoved
                asl
                ora bLBtnRelease
                asl
                ora bLBtnPress
                asl
                ora bDblClk
                beq MainLoop

                sta exit_code
                and #%00000001
                bne DblClk        ;<--
                lda exit_code
                and #%00000010
                bne LBtnPress     ;<--
                lda exit_code
                and #%00000100
                bne LBtnRelease   ;<--
                lda exit_code
                and #%00001000
                bne Moved         ;<--
                lda exit_code
                and #%00010000
                bne Exit          ;<--
                lda exit_code
                and #%00100000
                bne Won           ;<--
                lda exit_code
                and #%01000000
                bne Restart       ;<--
                
DblClk          lda #0
                sta bDblClk
                jsr OnDoubleClick
                jmp MainLoop

Exit            jsr DefaultKernal
                lda #0
                sta bExit
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
                
                ;jsr $e544; clear screen
                ldy #CL_LIGHTBLUE
                jsr ClearScreen
                jsr RestoreIRQ
                rts

LBtnRelease     lda #0
                sta bLBtnRelease
                jsr OnLBtnRelease
                jmp MainLoop

Moved           lda #0
                sta bMoved
                jsr OnMouseMove
                jmp MainLoop

LBtnPress       lda #0
                sta bLBtnPress
                jsr OnLBtnPress
                jmp MainLoop

Restart         lda #0
                sta bRestart
                jmp StartGame

Won             lda #0
                sta bWon
                sta TimerOn
                ; Increment Stats_Wins
                sed ; set decimal mode
                lda Stats_Wins
                clc
                adc #1
                sta Stats_Wins
                lda Stats_Wins+1
                adc #0
                sta Stats_Wins+1
                cld ; clear decimal mode
                ;
                jsr UpdateTimes
                jsr ShowWonDlg
                jsr SaveStats
                lda #1
                sta WonFlag
                jmp MainLoop

;=============================================
!zone ____________Events______________
;============================================= OnButtonPress
OnLBtnPress     ; Fill MouseInfo
                jsr MouseToScr
                lda VIC
                sta MouseInfo+2
                lda VIC+1
                sta MouseInfo+3
                lda VIC+16
                sta MouseInfo+4
                
                lda GameMode
                cmp #GM_MENU
                bne +
                ; In Menu Mode
                lda MouseInfo+3
                sec
                sbc #62
                lsr
                lsr
                lsr
                lsr
                sta ClickedMenuIndex
                rts
+               cmp #GM_DIALOG
                bne +
                ; In DLG Mode
                lda DialogMode
                cmp #DM_INSTRUCTS
                beq pr_instructs
                cmp #DM_ABOUT
                beq pr_tmc_abot_thx
                cmp #DM_STATS
                beq pr_stats
                cmp #DM_NEW
                beq pr_newexit
                cmp #DM_EXIT
                beq pr_newexit
                cmp #DM_WON
                beq pr_won
                cmp #DM_TOOMANYCARDS
                beq pr_tmc_abot_thx
                cmp #DM_THANKS
                beq pr_tmc_abot_thx
                rts

pr_tmc_abot_thx jsr IsInTMCDlgBtn
-               lda res
                beq pr_not_in_btn
                jsr PressOk
                rts
pr_instructs    jsr IsInInstDlgBtn
                jmp -
pr_stats        jsr IsInStatsDlgBtn
                jmp -
pr_newexit      jsr IsInYesNoBtns
-               lda res
                beq pr_not_in_btn
                lda VIC
                cmp #208
                bcc yes
                cmp #216
                bcc pr_not_in_btn
                jsr PressNo
                rts
yes             jsr PressYes
                rts
pr_won          jsr IsInWonDlgBtns
                jmp -
pr_not_in_btn   rts
+               rts

;============================================= OnDoubleClick
OnDoubleClick   lda GameMode
                cmp #GM_NORMAL
                bne +
                jsr DoubleClick
+               rts

;============================================= OnButtonRelease
OnLBtnRelease   lda GameMode
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
RelInDlgMode2   lda DialogMode
                cmp #DM_ABOUT
                beq about
                cmp #DM_TOOMANYCARDS
                beq too_many_cards
                cmp #DM_THANKS
                beq thanks
                cmp #DM_INSTRUCTS
                beq instructs
                cmp #DM_NEW
                beq asknew
                cmp #DM_EXIT
                beq askexit
                cmp #DM_STATS
                beq stats
                cmp #DM_WON
                beq won
                rts
about           jsr IsInTMCDlgBtn
                lda res
                beq not_in_dlg_btn
same2           jsr ReleaseOk
                jsr Pause2
                jsr GotoNormalMode
                rts
too_many_cards  jsr IsInTMCDlgBtn
                lda res
                beq not_in_dlg_btn
                jsr ReleaseOk
                jsr Pause2
                jsr RestoreScreen
                lda #GM_NORMAL
                sta GameMode
                rts
instructs       jsr IsInInstDlgBtn
                lda res
                beq not_in_dlg_btn
                jsr same2
                lda #1
                sta TimerOn
                rts
asknew          lda #<bRestart
                sta here+1
                lda #>bRestart
                sta here+2
                jsr asknewexit
                rts
askexit         lda #<bExit
                sta here+1
                lda #>bExit
                sta here+2
                jsr asknewexit
                rts
stats           jsr Stats; see below
                rts
thanks          jsr IsInTMCDlgBtn
                lda res
                beq not_in_dlg_btn
                jsr same2
                lda #1
                sta bExit
not_in_dlg_btn  rts
won             jsr IsInWonDlgBtns
                lda res
                beq not_in_dlg_btn
                lda VIC
                cmp #208
                bcc +
                cmp #216
                bcc not_in_dlg_btn
                lda bIsYes
                bne not_in_dlg_btn
                jsr ReleaseNo
                jsr Pause
                lda #1
                sta bExit
                  
                     
                rts
+               lda bIsYes
                beq not_in_dlg_btn
                jsr ReleaseYes
                jsr Pause2
                jsr RestoreScreen
                jsr ShowStatsDlg
                lda #1
                sta PlayAgainFlag
                rts
Stats           jsr IsInStatsDlgBtn
                lda res
                beq not_in_dlg_btn
                jsr ReleaseOk
                jsr Pause2
                lda WonFlag
                bne +
                jsr GotoNormalMode
                rts
+               lda #0
                sta WonFlag
                lda PlayAgainFlag
                pha
                lda #0
                sta PlayAgainFlag
                pla
                bne start_over
                jsr RestoreScreen
                jsr ShowThanksDlg
                rts
start_over      jsr GotoNormalMode
                lda #1
                sta bRestart
foff            rts
asknewexit      jsr IsInYesNoBtns
                lda res
                beq foff
                lda VIC
                cmp #208
                bcc newexit
                cmp #216
                bcc foff
                lda bIsYes
                bne foff
                jsr ReleaseNo
                jsr Pause2
                jmp GotoNormalMode
newexit         lda bIsYes
                bne +
                rts
+               lda #1
here            sta $ffff; put your addr here before calling (bExit or bRestart)
                jsr ReleaseYes
                jsr Pause2
                ; Increment Stats_Lost
                sed ; set decimal mode
                lda Stats_Lost
                clc
                adc #1
                sta Stats_Lost
                lda Stats_Lost+1
                adc #0
                sta Stats_Lost+1
                cld ; clear decimal mode
                ;
                jsr SaveStats
                jsr ShowStatsDlg
                rts
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
GotoNormalMode  lda #%00001111
                sta VIC+21
                jsr RestoreScreen
                jsr display_time
                lda #GM_NORMAL
                sta GameMode
                rts
;----- In normal mode ------------------------
RelInNormalMode2
                lda MouseInfo+1
                cmp #4
                bcs rel_in_area
                cmp #1
                bcs rel_in_repaces
                ;released in menu
                lda MouseInfo
                cmp #6
                bcc in_game_menu
                cmp #12
                bcc in_help_menu
                rts
in_help_menu    jsr ShowHelpMenu
                rts
in_game_menu    jsr ShowGameMenu
                rts
rel_in_repaces  lda MouseInfo
                cmp #20
                bcc clicked_in_rep
                ;released in aces
                lda bSelected
                bne sel_aces
                rts
sel_aces        jsr ToAcesTransfer
                lda result
                beq +
                jsr Tirade
+               rts
                ;released in repository
clicked_in_rep  lda bSelected
                bne sel_rep
                ;not selected
                jsr MarkRepCard
                lda res; if 0 then no selection
                sta bSelected
                rts
sel_rep         lda #5
                sta modulo
                lda MouseInfo
                jsr Mod
                txa
                clc
                adc #8
                sta PileTo
                sta Action+1
                jsr ToReposTransfer
                lda result
                beq +
                jsr Tirade
+               rts
                ;released in pile area
rel_in_area     lda bSelected
                bne selected_area
                jsr MarkPile
                lda res; if 0 then no selection
                sta bSelected
                rts
selected_area   jsr ToPileTransfer
                lda result
                beq +
                jsr Tirade
+               rts
;============================================= OnMouseMove

OnMouseMove     lda GameMode
                beq MovInNormalMode
                bpl MovInMenuMode
                jmp MovInDlgMode
;----- In normal mode ------------------------
MovInNormalMode lda #1
                sta ProcessDblClk
                lda bFirePressed
                bne fire_is_pressed
                ;
                rts
fire_is_pressed rts
;----- In menu mode --------------------------
MovInMenuMode   jsr MouseToScr
                lda MenuMode
                bne moved_HM_shown
                ;moved while game menu shown
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
;----- In dialog mode ------------------------
MovInDlgMode    lda #0
                sta ProcessDblClk
                lda bFirePressed
                bne +
                lda IsLBtnPressed
                bne +
                rts
                ; Mouse or joystick button pressed
+               lda DialogMode
                cmp #DM_INSTRUCTS
                beq prrel_instructs
                cmp #DM_ABOUT
                beq prrel_about
                cmp #DM_STATS
                beq prrel_stats
                cmp #DM_NEW
                beq prrel_newexit
                cmp #DM_EXIT
                beq prrel_newexit
                cmp #DM_WON
                beq prrel_won
                cmp #DM_TOOMANYCARDS
                beq prrel_about
                rts
prrel_about     jsr IsInTMCDlgBtn
                lda res
same            beq +
                lda bButtonPressing
                bne ++
                jsr PressOk
                rts
+               lda bButtonPressing
                beq ++
                jsr ReleaseOk
++              rts
prrel_instructs jsr IsInInstDlgBtn
                lda res
                jmp same
prrel_stats     jsr IsInStatsDlgBtn
                lda res
                jmp same
prrel_newexit   jsr IsInYesNoBtns
-               lda res
                beq not_in_btns
                lda VIC
                cmp #208
                bcc in_yes
                cmp #216
                bcc not_in_btns
                ; In No button
                lda bIsYes
                bne not_in_btns
                lda bButtonPressing
                bne ++
                jsr PressNo
                rts
in_yes          lda bIsYes
                beq not_in_btns
                lda bButtonPressing
                bne ++
                jsr PressYes
                rts
not_in_btns     ;Not in buttons
                lda bButtonPressing
                beq ++
                lda bIsYes
                bne +
                jsr ReleaseNo
                rts
+               jsr ReleaseYes
                rts
prrel_won       jsr IsInWonDlgBtns
                jmp -
++              rts
;=============================================

!zone MenuActions
GM_Actions      lda ClickedMenuIndex
                cmp #GM_MI_NEW
                beq ACTION_NEW
                cmp #GM_MI_UNDO
                beq ACTION_UNDO
                cmp #GM_MI_STATS
                beq ACTION_STATS
                cmp #GM_MI_EXIT
                beq ACTION_EXIT
ACTION_NEW      jsr ShowRUSureDlg
                lda #DM_NEW
                sta DialogMode
                rts
ACTION_UNDO     jsr DoUndo
                rts
ACTION_STATS    jsr ShowStatsDlg
                rts
ACTION_EXIT     jsr ShowRUSureDlg
                lda #DM_EXIT
                sta DialogMode
                rts
HM_Actions      lda ClickedMenuIndex
                cmp #HM_MI_HELP
                beq ACTION_HELP
                cmp #HM_MI_ABOUT
                beq ACTION_ABOUT
ACTION_HELP     lda #0
                sta TimerOn
                jsr ShowInstrDlg
                rts
ACTION_ABOUT    jsr ShowAboutDlg
                rts

;=============================================
!zone Joystick
bFirePressed    !byte 0

Joystick        lda dc_counter
                beq +
                dec dc_counter
                bne +
                ; time's up
                lda bFirePressed
                bne +
                lda #1
                sta bLBtnRelease
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
                lda #1
                sta bLBtnPress
                jmp OnlyMove
joydblclk       ; event double click
                lda #1
                sta bDblClk
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
                lda #1
                sta bLBtnRelease
+               lda #0
                sta block_release

OnlyMove        lda dx
                ora dy
                bne move
                rts
move            lda #1
                sta bMoved
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
acc      = 1   ; accelaration (best: 1)
ProcessDblClk   !byte 1; If set, recognizes double clicks
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
IsLBtnPressed   !byte 0; documents fire press at any time

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
                lda #1
                sta bLBtnRelease
+               jsr GetClicks
                jsr scanmovs
                jsr boundmus
                rts

;---------------------------------------
GetClicks       lda $dc01
                cmp #$ef
                bne noFire
                ;Fire pressed
                lda IsLBtnPressed
                bne out_here;fire was also pressed before
                ;fire was not pressed before
                lda #1
                sta IsLBtnPressed
                lda ProcessDblClk
                beq +
                lda dc_counter
                bne dblclk
                lda #dc_delay
                sta dc_counter
                ; event click
+               lda #1
                sta bLBtnPress
                rts
dblclk          ; event double click
                lda #1
                sta bDblClk
                sta block_release
                lda #0
                sta dc_counter
                rts
noFire          ;Fire not pressed
                lda IsLBtnPressed
                beq out_here;fire was also not pressed before
                ;fire was pressed before
                lda #0
                sta IsLBtnPressed
                lda ProcessDblClk
                beq +
                lda dc_counter
                bne out_here
                lda block_release
                bne ++
+               lda #1
                sta bLBtnRelease
++              lda #0
                sta block_release
out_here        rts
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

                lsr    ;remove noise bit
                beq nomove

                cmp #acc ;Acceleration Speed
                bcc *+3
                asl    ;X2

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
                ror   ;remove noise bit

                cmp #256-acc ;Acceleration Speed
                bcs *+3
                asl        ;X2

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
                lda #1
                sta bMoved

make_ymove      lda yPos0
                sta old_yPos
                
                clc
                lda musposy
                adc #offsety
                sta yPos0
                sta yPos1

                cmp old_yPos
                beq no_ymove
                lda #1
                sta bMoved
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
                lda #%00000011
                sta VIC+21
                
                ; Install separator sprite
                lda #SP_Separator
                sta SPRPTR_2
                sta SPRPTR_3
                lda #1
                sta col2
                lda #0
                sta col3
                ldx #182
                stx VIC+4
                inx
                stx VIC+6
                ldx #66
                stx VIC+5
                inx
                stx VIC+7
                
                ; Install menu item highliter sprite
                lda #SP_MenuItemHighliter
                sta SPRPTR_4
                sta SPRPTR_5
                lda #2
                sta col4
                sta col5
                ;stretch
                lda #%00110000
                sta VIC+29; stretch X
                ;select box behind text
                lda #%00110000
                sta VIC+27
                lda VIC+16
                and #%00000011
                sta VIC+16
                
                lda #15
                sta sid+24
                lda #CL_BLACK
                sta $d020
                lda #CL_WHITE
                sta $d021
                
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
                
                ;Prepare DBTables (DB = Double Buffer)
                ldy #5
                ldx #0
                lda #<DBScrMap
                sta $fb
                lda #>DBScrMap
                sta $fc
                
loopscrtab      lda $fb
                sta DBScrTabLo,x
                lda $fc
                sta DBScrTabHi,x
                jsr AddYtoFBFC; in this case, it's 5
                inx
                cpx #25
                bcc loopscrtab
                
                ldx #0
                lda #<DBColMap
                sta $fb
                lda #>DBColMap
                sta $fc
                
loopcoltab      lda $fb
                sta DBColTabLo,x
                lda $fc
                sta DBColTabHi,x
                jsr AddYtoFBFC; in this case, it's 5
                inx
                cpx #25
                bcc loopcoltab
                rts


Initialize      lda #0
                sta bSelected
                sta WonFlag
                lda #1
                sta TimerOn
                sta MayUndo
                lda #60
                sta Timer
                lda #255
                sta SelMenuIndex
                ; init time, all pile lenghts
                ldx #8
                lda #0
                sta Seconds
                sta Minutes
init_rep        sta PileLengths,x
                inx
                cpx #16
                bcc init_rep
                ; init aces
                lda #$ff
                ldx #3
init_aces       sta Aces,x
                dex
                bpl init_aces
                ; Normal game mode
                lda #GM_NORMAL
                sta GameMode
                ; Set ActionStackPtr
                lda #<ActionStack
                sta ActionStackPtr
                lda #>ActionStack
                sta ActionStackPtr+1
                rts

!zone IRQ
CIA_IRQ         jsr RequestTimer
                ; Rattle Sound
                lda RattleSoundOn
                beq +
                dec RattleCounter1
                lda RattleCounter1
                bne +
                ldx RattleCounter2
                lda Freqs,x
                sec
                sbc #20
                sta Freq
                jsr SimpleCardSound
                lda #RattleDelay
                sta RattleCounter1
                dec RattleCounter2
                lda RattleCounter2
                bne +
                lda #0
                sta RattleSoundOn
+               ; Mouse and Joystick
                lda MouseOn
                bne mouse
                jsr Joystick
                jmp std_irq
mouse           jsr Mouse
                jmp std_irq


RequestTimer    lda TimerOn
                bne DoTimer
                rts
DoTimer         dec Timer
                beq TimerAction
                rts
TimerAction     lda #60
                sta Timer
                inc Seconds
                lda Seconds
                cmp #60
                bne display_time
                inc Minutes
                lda #0
                sta Seconds
                lda Minutes
                cmp #99
                bne display_time
                lda #0
                sta TimerOn
display_time    lda Seconds
                jsr ByteToDeciChar
                lda DigitHi
                sta SCRMEM+$26
                lda DigitLo
                sta SCRMEM+$27
                lda Minutes
                jsr ByteToDeciChar
                lda DigitHi
                sta SCRMEM+$23
                lda DigitLo
                sta SCRMEM+$24
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
Pause           ldy #35
--              ldx #0
-               dex
                bne -
                dey
                bne --
                rts


Tirade          jsr SmallTirade
                lda retval
                bne Tirade
                rts


retval          !byte 0
SmallTirade     lda #0
                sta retval
                ldx #11
-               stx PileFrom
                stx Action
                txa
                pha
                jsr CheckPileForTirade
                lda result
                beq +
                jsr ToAcesTransfer
                lda result
                beq nopause
                pha
                jsr Pause
                pla
nopause         ora retval
                sta retval
+               pla
                tax
                dex
                bpl -
                rts


CheckPileForTirade
                ; Get card and store in Cmp
                lda #0
                sta result
                jsr GetPileInfo
                ldy PileLength
                bne +
                rts
+               dey
                lda ($02),y
                jsr GetSuitAndValue
                lda Suit
                sta SuitCmp
                lda Value
                sta ValueCmp
                ; At least a 3 (Value >= 2)
                cmp #2
                bcc success
                
                lda SuitCmp
                eor #%00000010
                tax
                lda Aces,x
                cmp #$ff
                bne +
                rts
+               jsr GetSuitAndValue
                ldx Value
                inx
                cpx ValueCmp
                bcs +
                rts
+               lda SuitCmp
                eor #%00000010
                eor #%00000001
                tax
                lda Aces,x
                cmp #$ff
                bne +
                rts
+               jsr GetSuitAndValue
                ldx Value
                inx
                cpx ValueCmp
                bcs success
                rts
success         lda #1
                sta result
                rts


; Expects pile in X
; Redraws pile
UpdatePile      cpx #8
                bcc pile_area
                txa
                sec
                sbc #8
                sta RepositoryPos
                lda #CL_DARKGREEN
                sta MapColor
                jsr DrawRepCard
                rts
pile_area       jsr PileToDB; fills xPos/yPos
                jsr DrawDB
                rts


DoUndo          ; Check if at beginning (no undo possible)
                lda ActionStackPtr
                cmp #<ActionStack
                bne +
                lda ActionStackPtr+1
                cmp #>ActionStack
                bne +
                rts
+               ; Go back with ActionStack pointer
                lda ActionStackPtr
                sec
                sbc #3
                sta ActionStackPtr
                sta $fb
                lda ActionStackPtr+1
                sbc #0
                sta ActionStackPtr+1
                sta $fc
                ; Fill Action and perform it
                ldy #0
                lda ($fb),y
                sta Action+1
                iny
                lda ($fb),y
                sta Action
                iny
                lda ($fb),y
                sta Action+2
                jsr PerformAction
                ; Redraw piles
                ldx Action
                jsr UpdatePile
                ldx Action+1
                jsr UpdatePile
                rts


DoubleClick     lda bSelected
                beq +
                rts
+               ; Find PileFrom
                lda #5
                sta modulo
                lda MouseInfo
                jsr Mod                
                lda MouseInfo+1
                cmp #4
                bcc +
                stx PileFrom
                jmp ++
+               txa
                clc
                adc #8
                sta PileFrom
                tax
                cmp #12
                bcc ++
                rts
++              ; Try Aces
                lda PileLengths,x
                bne +
                rts
+               jsr ToAcesTransfer
                lda result
                beq try_rep
                jsr Tirade
                rts
try_rep         ; If aces failed: try repository
                lda PileFrom
                cmp #8
                bcc +
                rts
+               ldx #8
rep_loop        stx PileTo
                txa
                pha
                jsr ToReposTransfer
                pla
                tax
                lda result
                beq +
                jsr Tirade
                rts
+               inx
                cpx #12
                bcc rep_loop
                rts


; Tries to transfer card to repository
; Expects: PileFrom (0-11) and PileTo filled (8-11)
ToReposTransfer lda #0
                sta result
                ; Check if transfer ok
                ldx PileTo
                stx Action+1
                lda PileLengths,x
                bne deselect; not ok
                ; Ok
                lda #1
                sta Action+2
                ldx PileFrom
                stx Action
                lda PileLengths,x
                tax
                dex
                stx PileIndexFrom;only last card in pile
                jsr RecordAction
                jsr PerformAction; sets result to 1
                lda #CL_DARKGREEN
                sta MapColor
                lda PileTo
                sec
                sbc #8
                sta RepositoryPos
                jsr DrawRepCard
                ; Deselect and/or update PileFrom
deselect        jsr DeselUpdate
                rts


DeselUpdate     lda result
                bne +
                lda bSelected
                bne +
                rts
+               lda #0
                sta bSelected
                ldx PileFrom
                jsr UpdatePile
                rts


; Tries to transfer card to aces
; Expects: PileFrom filled
ToAcesTransfer  lda #0
                sta result
                ; Save last card of pile in LastCard
                ldx PileFrom
                stx Action
                jsr GetPileInfo
                ldy PileLength
                bne +
                rts
+               dey
                lda ($02),y
                sta LastCard
                ; Find PileTo and RepositoryPos
                jsr GetSuitAndValue
                lda Value
                sta ValueCmp
                lda Suit
                sta SuitCmp
                tax
                clc
                adc #4
                sta RepositoryPos
                clc
                adc #8
                sta PileTo
                sta Action+1
                ; Check if transfer is ok
                lda Aces,x
                cmp #$ff
                bne check_normal
                ;no ace yet on pile
                lda Value
                bne desel; not ok
                jmp aces_go
check_normal    jsr GetSuitAndValue; of Aces,x
                ldx Value
                inx
                cpx ValueCmp
                bne desel; not ok
aces_go         ; Ok
                lda #1
                sta Action+2
                jsr RecordAction
                jsr PerformAction; sets result to 1
                lda #CL_DARKGREEN
                sta MapColor
                jsr DrawRepCard
                ; Deselect and/or update PileFrom
desel           jsr DeselUpdate
                rts


; Counts no of cards that user may transfer at once
GetTransferable ; Count free repository cards
                ldy #0
                ldx #8
countloop       lda PileLengths,x
                bne prepareloop
                iny
prepareloop     inx
                cpx #12
                bne countloop
                iny
                sty Transferable; (free repcards + 1)
                ; Count empty piles (that aren't target)
                ldx PileTo
                ldy #0
                lda PileLengths,x
                bne pileto_nonvoid
                ldy #255
pileto_nonvoid  ldx #7
countloop1      lda PileLengths,x
                bne prepareloop1
                iny
prepareloop1    dex
                bpl countloop1
                ; now (empty piles) is in Y
                tya
                bne has_empty_piles
                rts; returns (free rep cards + 1) in Transferable
has_empty_piles cpy #8
                bne finalize
                lda #255
                sta Transferable
                rts; returns 255 in Transferable
finalize        ; Return (free rep cards + 1) * 2^(empty piles) in Transferable
                lda Transferable
powers          asl
                dey
                bne powers
                sta Transferable
                rts


ToPileTransfer  lda #0
                sta result
                ; Find PileTo
                lda #5
                sta modulo
                lda MouseInfo
                jsr Mod
                stx PileTo
                stx Action+1
                ; Check if transfer is ok
                jsr GetPileInfo
                ldy PileLength
                beq length_check
                ; Do suit and value match
                dey
                lda ($02),y
                jsr GetSuitAndValue
                lda Suit
                sta SuitCmp
                lda Value
                sta ValueCmp
                lda UpperCard
                jsr GetSuitAndValue
                ldx Value
                inx
                cpx ValueCmp
                bne DeSelect; not ok
                lda Suit
                eor SuitCmp
                and #%00000010
                beq DeSelect; not ok
length_check    ; Get max no of cards for transfer
                jsr GetTransferable
                ; Count selected cards
                ldx PileFrom
                stx Action
                lda PileLengths,x
                sec
                sbc PileIndexFrom
                sta Action+2; no of cards
                ; Check
                lda Transferable
                cmp Action+2; no of cards
                bcs go_transfer
                ; too many cards message
                jsr DeSelect
                lda Transferable
                jsr ShowTMCDlg
                rts
go_transfer     jsr RecordAction
                jsr PerformAction; sets result to 1
                ; Deselect and/or update piles
DeSelect        lda result
                bne +
                lda bSelected
                bne +
                rts
+               ldx PileTo
                jsr PileToDB
                jsr DrawDB
                lda #0
                sta bSelected
                ldx PileFrom
                jsr UpdatePile
                rts


MarkRepCard     lda #0
                sta res
                ;Find PileFrom
                lda #5
                sta modulo
                ;jsr MouseToScr
                lda MouseInfo
                jsr Mod
                stx RepositoryPos
                txa
                clc
                adc #8
                sta PileFrom
                tax
                lda PileLengths,x
                bne can_mark
                rts
can_mark        inc res
                lda #0
                sta PileIndexFrom
                ldx RepositoryPos
                lda Repository,x
                sta UpperCard
                lda #CL_DARKBLUE
                sta MapColor
                jsr DrawRepCard
                rts


MarkPile        lda #0
                sta res
                ; Find marked pile
                lda #5
                sta modulo
                lda MouseInfo
                jsr Mod
                stx PileFrom
                ; Find selected pos in pile -> PileIndexFrom
                jsr GetPilePosAbstr; in SelPilePos
                ldx PileFrom
                jsr GetBestPilePos; in BestPilePos
                lda SelPilePos
                sta PileIndexFrom
                cmp BestPilePos
                bcs further
                lda BestPilePos
                sta PileIndexFrom
                ; Exit if PileIndex >= PileLength
further         ldx PileFrom
                jsr GetPileInfo
                lda PileLength
                bne +
                rts
+               lda PileIndexFrom
                cmp PileLength
                bcs stop_draw2; zero length
                ; Fill xPos/yPos
                ldx PileFrom
                jsr PileToDB; Draw pile to buffer first
                lda PileIndexFrom
                sta PileIndex
                jsr GetYFromPileInd
                lda res
                sta yPosDB
                ; Draw first card
                lda #CL_DARKBLUE
                sta MapColor
                ldy PileIndexFrom
                lda ($02),y
                sta UpperCard
                jsr DrawTopCardDB
                
                inc res
                
                ; Exit if PileIndex >= PileLength - 1
                ldx PileLength
                dex
                cpx PileIndexFrom
                beq stop_draw2
                ; Distinguish between short and long piles
                cpx #8
                bcc normal_pail
                ;it's a long pile
                ldy PileIndexFrom
                iny
draw_down2      lda ($02),y
                inc yPosDB
                inc yPosDB
                jsr DrawCoverCardDB
                iny
                cpy #8
                bne draw_down2
                
                inc yPosDB
                ldy PileLength
                dey
                lda ($02),y
                jsr DrawCoverCardDB
                jsr DrawDB
                rts
                ;it's a short pile
normal_pail     ldy PileIndexFrom
                iny
draw_down3      lda ($02),y
                inc yPosDB
                inc yPosDB
                jsr DrawCoverCardDB
                iny
                cpy PileLength
                bne draw_down3
                
stop_draw2      jsr DrawDB
                rts

;Expects pile no in X
;Writes position in pile to SelPilePos
GetPilePosAbstr jsr GetPileInfo
                lda PileLength
                cmp #9
                bcs pile_long
                lda MouseInfo+3; contains $d001
pile_normal     sec
                sbc #86                
                lsr
                lsr
                lsr
                lsr
                cmp #11
                bcc go_on
                lda #0
go_on           cmp PileLength
                bcs last
                sta SelPilePos
                rts
last            ldx PileLength
                dex
                stx SelPilePos
                rts
pile_long       lda MouseInfo+3
                cmp #197;89
                bcc pile_normal
                ;cmp #197
                ;bcs last
                ;lda #$ff
                ;sta SelPilePos
                jmp last
                rts

;Expects PileIndex to be filled                
GetYFromPileInd lda PileIndex
                cmp #8
                bcc oben
                lda #15
                sta res
                rts
oben            asl
                sta res
                rts

!zone Graphics
ClearDB         ldx #124
loopclrdb       lda #160
                sta DBScrMap,x
                lda #CL_DARKGREEN
                sta DBColMap,x
                dex
                bpl loopclrdb
                rts

;Returns Mouse pos in scr coords in MouseInfo
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

SaveScreen      ldy #3
ss_outer        ldx #0
ss_inner        lda SCRMEM,x ; bb ll hh
                sta $c400,x ; bb ll hh
                lda $d800,x ; bb ll hh
                sta $c800,x ; bb ll hh
                dex
                bne ss_inner
                inc ss_inner+2
                inc ss_inner+5
                inc ss_inner+8
                inc ss_inner+11
                dey
                bpl ss_outer
                ; wiederherstellen
                lda #>SCRMEM
                sta ss_inner+2
                lda #$c4
                sta ss_inner+5
                lda #$d8
                sta ss_inner+8
                lda #$c8
                sta ss_inner+11
                rts
                
RestoreScreen   ldy #3
rs_outer        ldx #0
rs_inner        lda $c400,x ; bb ll hh
                sta SCRMEM,x ; bb ll hh
                lda $c800,x ; bb ll hh
                sta $d800,x ; bb ll hh
                dex
                bne rs_inner
                inc rs_inner+2
                inc rs_inner+5
                inc rs_inner+8
                inc rs_inner+11
                dey
                bpl rs_outer
                lda #$c4
                sta rs_inner+2
                lda #>SCRMEM
                sta rs_inner+5
                lda #$c8
                sta rs_inner+8
                lda #$d8
                sta rs_inner+11
                rts
                
ClearScreen     ldx #0
                ldy #CL_DARKGREEN
clear_loop      lda #160
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
                bne clear_loop
                rts

DrawMenu        lda #<MenuMap
                sta $fb
                lda #>MenuMap
                sta $fc
                lda #40
                sta MapWidth
                lda #1
                sta MapHeight
                lda #<SCRMEM
                sta $fd
                lda #>SCRMEM
                sta $fe
                jsr DrawMap
                rts

DrawRepAces     lda #<RepAcesMap
                sta $fb
                lda #>RepAcesMap
                sta $fc
                lda #40
                sta MapWidth
                lda #3
                sta MapHeight
                lda #<(SCRMEM+$28)
                sta $fd
                lda #>(SCRMEM+$28)
                sta $fe
                jsr DrawMap
                
                lda #%00001111
                sta VIC+21
                rts


clr             !byte 0
;Expects
;char in A
;color in clr
;Scr Pos in xPos,yPos
DrawCharColor   pha
                ldy xPos
                ldx yPos
                lda ScrTabLo,x
                sta $fb
                lda ScrTabHi,x
                sta $fc
                pla
                sta ($fb),y
                
                lda ColorTabHi,x
                sta $fc
                lda clr
                sta ($fb),y
                rts


;Expects
;char in A
;color in clr
;DB Pos in xPosDB,yPosDB
DrawCharColorDB pha
                ldy xPosDB
                ldx yPosDB
                lda DBScrTabLo,x
                sta $fb
                lda DBScrTabHi,x
                sta $fc
                pla
                sta ($fb),y

                lda DBColTabLo,x
                sta $fb                
                lda DBColTabHi,x
                sta $fc
                lda clr
                sta ($fb),y
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


; Uses: $0203 (for color mem ptr)
; Expects:
;  map mem pos in $fb
;  map width in MapWidth
;  map height in MapHeight
;  upper left screen mem location in $fd
DrawMap         lda $fd
                sta $04
                lda $fe; add ($d800 - SCRMEM) to $fd/$fe
                clc
                adc #($d8 - >SCRMEM)
                sta $05
                ; Start loop
                ldx MapHeight
                dex
                stx counter
outer_draw      lda MapWidth
                tay
                dey
draw_map        lda ($fb),y
                sta ($fd),y
                ; Adjust color---------
                tax
                lda Colors,x
                sta ($04),y
                ;----------------------
                dey
                bpl draw_map
                
                ldy MapWidth
                jsr AddYtoFBFC
                ldy #40
                jsr AddYtoFDFE
                jsr AddYto0405
                
                dec counter
                bpl outer_draw
                
                rts
                
;                
;DrawMap         ; Put color mem addr to $02
;                lda $fd
;                sta $04
;                lda $fe; add $d4 to $fe
;                clc
;                adc #$d4
;                sta $05
;                ; Start loop
;                ldx MapHeight
;                dex
;                stx counter
;outer_draw      lda MapWidth
;                tay
;                dey
;draw_map        lda ($fb),y
;                sta ($fd),y
;                ; Adjust color---------
;                tax
;                lda Colors,x
;                sta ($04),y 
;                ;----------------------
;                dey
;                bpl draw_map
;                
;                ldy MapWidth
;                jsr AddYtoFBFC
;                ldy #40
;                jsr AddYtoFDFE
;                jsr AddYto0405
;                
;                dec counter
;                bpl outer_draw
;                
;                rts


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


;Expects:
; map mem pos in $fb
; map width in MapWidth
; map height in MapHeight
; upper left screen mem location in $fd
; color map in $02
DrawScrClrMap   lda $fd
                sta $04
                lda $fe; add ($d800 - SCRMEM) to $fd/$fe
                clc
                adc #($d8 - >SCRMEM)
                sta $05
                ldx MapHeight
                dex
                stx counter
outer_draw3     lda MapWidth
                tay
                dey
draw_map3       lda ($fb),y
                sta ($fd),y
                ; Adjust color---------
                lda ($02),y
                sta ($04),y
                ;----------------------
                dey
                bpl draw_map3
                
                ldy MapWidth
                jsr AddYtoFBFC
                jsr AddYto0203
                ldy #40
                jsr AddYto0405
                jsr AddYtoFDFE
                
                dec counter
                bpl outer_draw3
                
                rts

!zone CardDrawing
DrawPiles       ldx #0
draw_pile_loop  jsr PileToDB
                jsr DrawDB
                inx
                cpx #8
                bne draw_pile_loop
                rts

;Draws double buffer to xPos/yPos
DrawDB          tax
                pha
                lda #<DBScrMap
                sta $fb
                lda #>DBScrMap
                sta $fc
                lda #<DBColMap
                sta $02
                lda #>DBColMap
                sta $03
                ldx yPos
                ldy xPos
                jsr PosToScrMem
                lda #5
                sta MapWidth
                lda #21
                sta MapHeight
                jsr DrawScrClrMap
                pla
                tax
                rts

;Expects pile no in X
PileToDB        lda #5
                sta MapColor
                txa
                pha
                lda #0
                sta yPosDB
                sta xPosDB
                lda #4
                sta yPos
                stx dummy
                txa
                asl
                asl
                clc
                adc dummy
                sta xPos
                
                jsr GetPileInfo
                jsr ClearDB
                lda PileLength
                beq stop_draw
                
                ldy #0
                lda ($02),y
                jsr DrawTopCardDB
                
                lda PileLength
                cmp #1
                beq stop_draw
                cmp #9
                bcc normal_pile
                
                ;it's a long pile
                ldy #1
draw_down1      lda ($02),y
                inc yPosDB
                inc yPosDB
                jsr DrawCoverCardDB
                iny
                cpy #8
                bne draw_down1
                
                inc yPosDB
                ldy PileLength
                dey
                lda ($02),y
                jsr DrawCoverCardDB
                ;jsr DrawDB
                pla
                tax
                rts
                
normal_pile     ldy #1
draw_down       lda ($02),y
                inc yPosDB
                inc yPosDB
                jsr DrawCoverCardDB
                iny
                cpy PileLength
                bne draw_down
                ;jsr DrawDB
                
stop_draw       pla
                tax
                rts

;Expects pos in yPosDB, card no in A
DrawTopCardDB   jsr GetSuitAndValue
                lda #<FullCardMap
                sta $fb
                lda #>FullCardMap
                sta $fc
                lda yPosDB
                sta dummy
                asl
                asl
                clc
                adc dummy
                adc #29
                tax
                ldy #29
drawtoploop     lda ($fb),y
                sta DBScrMap,x
                lda MapColor
                sta DBColMap,x
                dex
                dey
                bpl drawtoploop
                jsr DrawSuitValDB
                rts

;Expects pos in yPosDB, card no in A
DrawCoverCardDB tax
                tya
                pha
                txa
                jsr GetSuitAndValue
                lda #<CoverCardMap
                sta $fb
                lda #>CoverCardMap
                sta $fc
                
                lda yPosDB
                sta dummy
                asl
                asl
                clc
                adc dummy
                adc #29
                tax
                ldy #29
drawcovloop     lda ($fb),y
                sta DBScrMap,x
                lda MapColor
                sta DBColMap,x
                dex
                dey
                bpl drawcovloop
                
                jsr DrawSuitValDB
                pla
                tay
                rts

;Draws card content to double buffer with Value and Suit to yPosDB
DrawSuitValDB   lda yPosDB
                pha
               
                lda Suit
                and #%00000010
                sta clr

                inc yPosDB
                lda #1
                sta xPosDB
                
                ldx Value
                cpx #9
                beq its_a_ten
                
                lda ValueToChar,x
                jsr DrawCharColorDB
                inc xPosDB
                inc xPosDB
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColorDB
                inc yPosDB
                inc yPosDB
                inc yPosDB
                ldx Value
                lda ValueToChar,x
                jsr DrawCharColorDB
                dec xPosDB
                dec xPosDB
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColorDB
                
                pla
                sta yPosDB               
                rts
                
its_a_ten       lda #49
                jsr DrawCharColorDB
                inc xPosDB
                lda #58
                jsr DrawCharColorDB
                inc xPosDB
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColorDB
                inc yPosDB
                inc yPosDB
                inc yPosDB
                lda #58
                jsr DrawCharColorDB
                dec xPosDB
                lda #49
                jsr DrawCharColorDB
                dec xPosDB
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColorDB
                
                pla
                sta yPosDB
                rts
                
; Draws a repository or aces card
; Expects RepositoryPos (0-7) and MapColor
DrawRepCard     ldx #1
                stx yPos
                lda RepositoryPos
                asl
                asl
                clc
                adc RepositoryPos
                tax
                stx xPos
                ;Draw empty rep card
                lda #<RepCardMap
                sta $fb
                lda #>RepCardMap
                sta $fc
                lda #5
                sta MapWidth
                lda #3
                sta MapHeight
                ldx yPos
                ldy xPos
                jsr PosToScrMem
                jsr DrawMapFixedClr
                
                inc xPos
                inc yPos
                lda RepositoryPos
                clc
                adc #8
                tax
                jsr GetPileInfo
                lda PileLength
                bne is_card
                cpx #12
                bcs is_aces_pile
                ; draw empty rep pile
                lda #CL_DARKBLUE
                sta clr
                lda #0
                jsr DrawCharColor
                lda #0
                inc xPos
                jsr DrawCharColor
                lda #0
                inc xPos
                jsr DrawCharColor
                rts
is_aces_pile    txa
                sec
                sbc #12
                tax
                and #%00000010
                sta clr
                lda Aces,x
                cmp #$ff
                bne is_card
                inc xPos
                lda SuitToChar,x
                jsr DrawCharColor
                rts
is_card         ldy #0
                lda ($02),y
                jsr GetSuitAndValue
                lda Suit
                and #%00000010
                sta clr
                ldx Value
                cpx #9
                beq its_a_rep_ten
                
                lda ValueToChar,x
                jsr DrawCharColor
                inc xPos
                lda #32
                jsr DrawCharColor
                inc xPos
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColor
                rts
its_a_rep_ten   lda #49
                jsr DrawCharColor
                inc xPos
                lda #58
                jsr DrawCharColor
                inc xPos
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColor
                rts

!zone GeneralDialogRoutines
ShowStatsDlg    jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_STATS
                sta DialogMode
                lda #<Stats_DlgMap
                sta $fb
                lda #>Stats_DlgMap
                sta $fc
                ; Fill in the data
                ; Wins
                lda Stats_Wins
                and #$0f; low (decimal) digit
                clc
                adc #48
                ldy #81
                sta ($fb),y
                dey
                lda Stats_Wins
                lsr
                lsr
                lsr
                lsr; high decimal digit
                clc
                adc #48
                sta ($fb),y
                dey
                lda Stats_Wins+1
                and #$0f; low decimal digit
                beq +
                clc
                adc #48
                sta ($fb),y                
+               ; Lost
                lda Stats_Lost
                and #$0f; low (decimal) digit
                clc
                adc #48
                ldy #99
                sta ($fb),y
                dey
                lda Stats_Lost
                lsr
                lsr
                lsr
                lsr; high decimal digit
                clc
                adc #48
                sta ($fb),y
                dey
                lda Stats_Lost+1
                and #$0f; low decimal digit
                beq +
                clc
                adc #48
                sta ($fb),y
+               ; Best time
                lda Stats_Time0+1
                jsr ByteToDeciChar
                lda DigitHi
                ldy #150
                sta ($fb),y
                lda DigitLo
                iny
                sta ($fb),y
                lda Stats_Time0
                jsr ByteToDeciChar
                lda DigitHi
                ldy #153
                sta ($fb),y
                lda DigitLo
                iny
                sta ($fb),y
                ; Second best time
                lda Stats_Time1+1
                jsr ByteToDeciChar
                lda DigitHi
                ldy #168
                sta ($fb),y
                lda DigitLo
                iny
                sta ($fb),y
                lda Stats_Time1
                jsr ByteToDeciChar
                lda DigitHi
                ldy #171
                sta ($fb),y
                lda DigitLo
                iny
                sta ($fb),y
                ; Third best time
                lda Stats_Time2+1
                jsr ByteToDeciChar
                lda DigitHi
                ldy #186
                sta ($fb),y
                lda DigitLo
                iny
                sta ($fb),y
                lda Stats_Time2
                jsr ByteToDeciChar
                lda DigitHi
                ldy #189
                sta ($fb),y
                lda DigitLo
                iny
                sta ($fb),y
                ;----------------
                lda #18
                sta MapWidth
                lda #13
                sta MapHeight
                ldy #11
                ldx #6
                jsr PosToScrMem
                jsr DrawMap
                rts

ShowThanksDlg   jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_THANKS
                sta DialogMode
                lda #<Thanks_DlgMap
                sta $fb
                lda #>Thanks_DlgMap
                sta $fc
                lda #20
                sta MapWidth
                lda #10
                sta MapHeight
                ldy #10
                ldx #7
                jsr PosToScrMem
                jsr DrawMap
                rts

ShowRUSureDlg   jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #<RUSure_DlgMap
                sta $fb
                lda #>RUSure_DlgMap
                sta $fc
                lda #18
                sta MapWidth
                lda #10
                sta MapHeight
                ldy #11
                ldx #7
                jsr PosToScrMem
                jsr DrawMap
                rts

ShowAboutDlg    jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_ABOUT
                sta DialogMode
                lda #<About_DlgMap
                sta $fb
                lda #>About_DlgMap
                sta $fc
                lda #20
                sta MapWidth
                lda #11
                sta MapHeight
                ldy #10
                ldx #6
                jsr PosToScrMem
                jsr DrawMap
                rts
                
ShowInstrDlg    lda #%00000011
                sta VIC+21
                jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_INSTRUCTS
                sta DialogMode
                lda #<Instr_DlgMap
                sta $fb
                lda #>Instr_DlgMap
                sta $fc
                lda #38
                sta MapWidth
                lda #23
                sta MapHeight
                ldy #1
                ldx #1
                jsr PosToScrMem
                jsr DrawMap
                rts

; Expects no of transferable cards in A
ShowTMCDlg      jsr ByteToDeciChar
                jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_TOOMANYCARDS
                sta DialogMode
                lda #<TMC_DlgMap
                sta $fb
                lda #>TMC_DlgMap
                sta $fc
                lda #20
                sta MapWidth
                lda #11
                sta MapHeight
                ldy #10
                ldx #6
                jsr PosToScrMem
                jsr DrawMap
                ; Fill in digits
                ldy #11
                ldx #12
                jsr PosToScrMem
                ldy #0
                lda DigitHi
                sta ($fd),y
                iny
                lda DigitLo
                sta ($fd),y
                rts

ShowWonDlg      jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_WON
                sta DialogMode
                lda #<Won_DlgMap
                sta $fb
                lda #>Won_DlgMap
                sta $fc
                lda #19
                sta MapWidth
                lda #12
                sta MapHeight
                ldy #10
                ldx #6
                jsr PosToScrMem
                jsr DrawMap
                rts

StatsDlgBtnRect !byte 208,240,127,142
YesNoBtnsRect   !byte 168,248,159,174
InstDlgBtnRect  !byte 120,248,215,230
TMCDlgBtnRect   !byte 224,255,159,174; = AboutRect = ThanksRect
WonDlgBtnsRect  !byte 168,248,167,181

; BtnRect in sprite coords (with VIC+16 = 0): xfrom, xto, yfrom, yto
; Requires pointer to struct in FB
IsInBtnRect     tya
                pha
                lda VIC+16
                and #%00000001
                bne not_in_btn
                lda VIC
                ldy #0
                cmp ($fb),y
                bcc not_in_btn
                iny
                cmp ($fb),y
                bcs not_in_btn
                lda VIC+1
                iny
                cmp ($fb),y
                bcc not_in_btn
                iny
                cmp ($fb),y
                bcs not_in_btn
                lda #1
                sta res
                jmp +
                rts
not_in_btn      lda #0
                sta res
+               pla
                tay
                rts

IsInStatsDlgBtn lda #<StatsDlgBtnRect
                sta $fb
                lda #>StatsDlgBtnRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInYesNoBtns   lda #<YesNoBtnsRect
                sta $fb
                lda #>YesNoBtnsRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInInstDlgBtn  lda #<InstDlgBtnRect
                sta $fb
                lda #>InstDlgBtnRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInTMCDlgBtn   lda #<TMCDlgBtnRect
                sta $fb
                lda #>TMCDlgBtnRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInWonDlgBtns  lda #<WonDlgBtnsRect
                sta $fb
                lda #>WonDlgBtnsRect
                sta $fc
                jsr IsInBtnRect
                rts

PressRelease    ldx #123
                ldy #235
                jsr SwapChars
                ldx #224
                ldy #236
                jsr SwapChars
                ldx #122
                ldy #237
                jsr SwapChars
                ldx #118
                ldy #238
                jsr SwapChars
                ldx #120
                ldy #239
                jsr SwapChars
                rts

PressOk         lda #1
                sta bButtonPressing
                jmp PressReleaseOk

ReleaseOk       lda #0
                sta bButtonPressing
                jmp PressReleaseOk

PressReleaseOk  lda $d012
                cmp #255
                bne PressReleaseOk
                ldx #99
                ldy #228
                jsr SwapChars
                ldx #100
                ldy #229
                jsr SwapChars
                jmp PressRelease

PressYes        lda #1
                sta bButtonPressing
                sta bIsYes
                jmp PressReleaseYes

ReleaseYes      lda #0
                sta bButtonPressing
                jmp PressReleaseYes

PressReleaseYes lda $d012
                cmp #255
                bne PressReleaseYes
                ldx #101
                ldy #230
                jsr SwapChars
                ldx #102
                ldy #231
                jsr SwapChars
                ldx #103
                ldy #232
                jsr SwapChars
                jmp PressRelease

PressNo         lda #1
                sta bButtonPressing
                lda #0
                sta bIsYes
                jmp PressReleaseNo

ReleaseNo       lda #0
                sta bButtonPressing
                jmp PressReleaseNo

PressReleaseNo  lda $d012
                cmp #255
                bne PressReleaseNo
                ldx #104
                ldy #233
                jsr SwapChars
                ldx #105
                ldy #234
                jsr SwapChars
                ldx #246
                ldy #241
                jsr SwapChars
                ldx #240
                ldy #242
                jsr SwapChars
                ldx #247
                ldy #243
                jsr SwapChars
                ldx #248
                ldy #244
                jsr SwapChars
                ldx #249
                ldy #245
                jsr SwapChars
                rts

Pause2          ldx #32
                lda #0
                sta dummy
-               dec dummy
                bne -
                dex
                bne -
                rts

; Expects indices in X and Y
SwapChars       ;Prepare char addrs in FBFC and FDFE
                lda #0
                sta $fc
                sta $fe
                
                stx $fb
                asl $fb
                rol $fc
                asl $fb
                rol $fc
                asl $fb
                rol $fc
                lda $fc
                clc
                adc #$40
                sta $fc
                
                sty $fd
                asl $fd
                rol $fe
                asl $fd
                rol $fe
                asl $fd
                rol $fe
                lda $fe
                clc
                adc #$40
                sta $fe
                
                ldy #7
-               lda ($fb),y
                sta dummy
                lda ($fd),y
                sta ($fb),y
                lda dummy
                sta ($fd),y
                dey
                bpl -
                rts

!zone MenuRoutines
;Expects menu item index in SelMenuItem
HM_SelMenuItem  ;obtain selected menu index
                lda VIC+1
                sec
                sbc #62
                lsr
                lsr
                lsr
                lsr
                cmp SelMenuIndex
                bne highlight1
                rts
                ;highlight
highlight1      sta SelMenuIndex
                ;------------------X
                lda #76
                sta VIC+8
                lda #124
                sta VIC+10
                ;------------------Y
                lda SelMenuIndex
                asl
                asl
                asl
                asl
                clc
                adc #62
                sta VIC+9
                sta VIC+11
                ;------------------
                lda #%00111111
                sta VIC+21
                
                ;sel item->white, others->black
                jsr DrawHelpMenuMap
                ldy #87
                jsr SelectText
                
                rts

;Expects menu item index in SelMenuItem
GM_SelMenuItem  ;;obtain selected menu index
                lda VIC+1
                sec
                sbc #62
                lsr
                lsr
                lsr
                lsr
                cmp SelMenuIndex
                bne highlight
                rts
                ;highlight
highlight       sta SelMenuIndex
                ;------------------X
                lda #28
                sta VIC+8
                lda #68
                sta VIC+10
                ;------------------Y
                lda SelMenuIndex
                asl
                asl
                asl
                asl
                clc
                adc #62
                sta VIC+9
                sta VIC+11
                ;------------------
                lda #%00111111
                sta VIC+21
                
                ;sel item->white, others->black
                jsr DrawGameMenuMap
                ldy #81
                jsr SelectText
                rts

;Expects ... in Y
SelectText      lda #$0
                sta $fb
                lda #$d8
                sta $fc
                jsr AddYtoFBFC
                
                tya
                sec
                sbc #81
                lsr
                lsr
                clc
                adc #10
                sta dummy
                
                lda SelMenuIndex
                asl
                asl
                clc
                adc SelMenuIndex
                asl
                asl
                asl
                asl
                tay
                ldx #0
                jsr AddYXtoFBFC

                lda #CL_WHITE
                ldy dummy; (10 if GM, 11 if HM)
                dey
white_it        sta ($fb),y
                dey
                bpl white_it
                rts

IsInGameMenu    lda #1
                sta res
                lda VIC+16
                bne no_gm
                lda VIC
                cmp #28
                bcc no_gm
                cmp #116
                bcs no_gm
                lda VIC+1
                cmp #62
                bcc no_gm
                cmp #126
                bcs no_gm
                rts
no_gm           lda #0
                sta res
                rts
                
IsInHelpMenu    lda #1
                sta res
                lda VIC+16
                bne no_hm
                lda VIC
                cmp #76
                bcc no_hm
                cmp #164
                bcs no_hm
                lda VIC+1
                cmp #62
                bcc no_hm
                cmp #94
                bcs no_hm
                rts
no_hm           lda #0
                sta res
                rts

ShowGameMenu    lda #GM_MENU
                sta GameMode
                lda #MM_GAME
                sta MenuMode
                lda #255
                sta SelMenuIndex
                jsr SaveScreen
                ldx #5
invert_game     lda SCRMEM,x
                clc
                adc #128
                sta SCRMEM,x
                dex
                bpl invert_game
DrawGameMenuMap lda #<GameMenuMap
                sta $fb
                lda #>GameMenuMap
                sta $fc
                ; Show with Undo or Redo
                lda MayUndo
                bne showUndo
                ; else: show Redo
                ldy #37
                lda #82; R
                sta ($fb),y
                iny
                lda #5;  e
                sta ($fb),y
                jmp drawGM_Map
showUndo        ldy #37
                lda #85; U
                sta ($fb),y
                iny
                lda #14;  n
                sta ($fb),y
drawGM_Map      lda #12
                sta MapWidth
                lda #9
                sta MapHeight
                lda #<(SCRMEM+$28)
                sta $fd
                lda #>(SCRMEM+$28)
                sta $fe
                jsr DrawMap
                rts
                
ShowHelpMenu    lda #GM_MENU
                sta GameMode
                lda #MM_HELP
                sta MenuMode
                lda #255
                sta SelMenuIndex
                jsr SaveScreen
                ldx #6
invert_help     lda SCRMEM,x
                clc
                adc #128
                sta SCRMEM,x
                inx
                cpx #13
                bne invert_help
DrawHelpMenuMap lda #<HelpMenuMap
                sta $fb
                lda #>HelpMenuMap
                sta $fc
                lda #13
                sta MapWidth
                lda #5
                sta MapHeight
                lda #<(SCRMEM+$2e)
                sta $fd
                lda #>(SCRMEM+$2e)
                sta $fe
                jsr DrawMap
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

                
AddYXtoFBFC     tya
                clc
                adc $fb
                sta $fb
                txa
                adc $fc
                sta $fc
                rts
                
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

!zone Game
; Records an action by pushing it onto the action stack
; Needs: Action (pileFrom, pileTo, no of cards)
RecordAction    lda ActionStackPtr
                sta $fb
                lda ActionStackPtr+1
                sta $fc
                ldy #0
                lda Action
                sta ($fb),y
                iny
                lda Action+1
                sta ($fb),y
                iny
                lda Action+2
                sta ($fb),y
                ; Move ActionStack pointer
                lda ActionStackPtr
                clc
                adc #3
                sta ActionStackPtr
                lda ActionStackPtr+1
                adc #0
                sta ActionStackPtr+1
                rts


;; Expects: card_lower and card_upper (0-51 each)
;DoCardsMatch    lda #0
;                sta res
;                lda card_lower
;                jsr GetSuitAndValue
;                lda Suit
;                sta SuitCmp
;                lda Value
;                sta ValueCmp
;                lda card_upper
;                jsr GetSuitAndValue
;                lda Suit
;                eor SuitCmp
;                and #%00000010
;                bne check_value
;                rts
;check_value     inc ValueCmp
;                lda ValueCmp
;                cmp Value
;                beq they_do_match
;                rts
;they_do_match   lda #1
;                sta res
;                rts
;card_lower      !byte 0
;card_upper      !byte 0


; Performs an action (card transfer)
; Needs: Action (pileFrom, pileTo, no of cards)
PerformAction   lda #1
                sta result
                jsr PlaceCardSound
                lda Action+1
                sta PileTo
                ldx Action; pileFrom
                stx PileFrom
                jsr GetPileInfo
                lda PileLength
                sec
                sbc Action+2; no of cards
                sta PileIndexFrom
                
                ldx PileFrom
                cpx #12
                bcc not_from_aces
                
; From an ace pile (only for undo)
                txa
                sec
                sbc #12
                tax
                lda Aces,x
                sta dummy; card in dummy
                jsr GetSuitAndValue
                lda Value
                bne +
                lda #$ff
                sta Aces,x
                jmp ++
+               dec Aces,x
                
++              ldx PileTo
                jsr GetPileInfo
                ldy PileLength
                lda dummy
                sta ($02),y
                inc PileLengths,x
                jsr Set_bWon
                rts
                
                ; Copy the stuff
not_from_aces   jsr GetPileInfo; of PileFrom
                lda $02
                clc
                adc PileIndexFrom
                sta $fb
                lda $03
                adc #0
                sta $fc
                
                ldx PileTo
                jsr GetPileInfo
                lda $02
                clc
                adc PileLength
                sta $02
                lda $03
                adc #0
                sta $03

                ldy #0
transloop       lda ($fb),y
                sta ($02),y
                iny
                cpy Action+2
                bne transloop

                ; Shorten PileFrom
                ldx PileFrom
                lda PileLengths,x
                sec
                sbc Action+2
                sta PileLengths,x
                
                ; Expand PileTo
                lda PileTo
                cmp #12
                bcc can_expand; don't expand if aces pile
                jsr Set_bWon
                rts
can_expand      lda PileLength
                clc
                adc Action+2
                ldx PileTo
                sta PileLengths,x
                jsr Set_bWon
                rts


Set_bWon        ldx #3
-               lda Aces,x
                cmp Kings,x
                bne +
                dex
                bpl -
                ; Player has won
                lda #1
                sta bWon
+               rts


;Expects pile no in X
;Writes to BestPilePos
GetBestPilePos  jsr GetPileInfo
                ldy PileLength
                cpy #1
                beq simple
                
                dey
next_try        lda ($02),y
                jsr GetSuitAndValue
                lda Suit
                sta SuitCmp
                lda Value
                sta ValueCmp
                dey
                lda ($02),y
                jsr GetSuitAndValue
                ldx ValueCmp
                inx
                cpx Value
                bne no_fit
                lda SuitCmp
                eor Suit
                and #%00000010
                beq no_fit
                tya
                beq simple
                jmp next_try
                
no_fit          iny
                sty BestPilePos
                rts
simple          lda #0
                sta BestPilePos
                rts


;Expects pile no in X
;writes addr to 0203
GetPileInfo     cpx #8
                bcc area_pile
                txa
                sec
                sbc #8
                tay
                lda #<Repository
                sta $02
                lda #>Repository
                sta $03
                jsr AddYto0203
                
                lda PileLengths,x
                bne has_card
                lda #0
                sta PileLength
                rts
has_card        lda #1
                sta PileLength
                rts
area_pile       lda #<Piles
                sta $02
                lda #>Piles
                sta $03
                lda Mult19,x
                tay
                jsr AddYto0203
                lda PileLengths,x
                sta PileLength
                rts


;Expects card no in A
GetSuitAndValue pha
                lda #13
                sta modulo
                pla
                pha
                jsr Mod
                sta Value
                stx Suit
                pla
                rts


swap_ind        !byte 0,0
ShuffleDeck     lda #52
                sta modulo
                ldy #0
once_more       jsr Random
                jsr Mod
                sta swap_ind
                jsr Random
                jsr Mod
                sta swap_ind+1
                tax
                lda Deck,x
                sta $fb
                ldx swap_ind
                lda Deck,x
                ldx swap_ind+1
                sta Deck,x
                lda $fb
                ldx swap_ind
                sta Deck,x
                dey
                bne once_more
                rts

                
DeckToPiles     lda #<Deck
                sta $fb
                lda #>Deck
                sta $fc
                ldy #51
                ldx #7
                stx PileLengths
                dex
pile0           lda ($fb),y
                sta Piles,x
                dey
                dex
                bpl pile0
                
                ldx #7
                stx PileLengths+1
                dex
pile1           lda ($fb),y
                sta Pile1,x
                dey
                dex
                bpl pile1
                
                ldx #7
                stx PileLengths+2
                dex
pile2           lda ($fb),y
                sta Pile2,x
                dey
                dex
                bpl pile2
                
                ldx #7
                stx PileLengths+3
                dex
pile3           lda ($fb),y
                sta Pile3,x
                dey
                dex
                bpl pile3
                
                ldx #6
                stx PileLengths+4
                dex
pile4           lda ($fb),y
                sta Pile4,x
                dey
                dex
                bpl pile4
                
                ldx #6
                stx PileLengths+5
                dex
pile5           lda ($fb),y
                sta Pile5,x
                dey
                dex
                bpl pile5
                
                ldx #6
                stx PileLengths+6
                dex
pile6           lda ($fb),y
                sta Pile6,x
                dey
                dex
                bpl pile6
                ldx #6
                stx PileLengths+7
                dex
pile7           lda ($fb),y
                sta Pile7,x
                dey
                dex
                bpl pile7
                rts

                
!zone Kernal
ConfigKernal    lda #0
                sta $9d; kernal messages off
DisableSTOP     LDA #234
                STA $0328
                rts
                
                
DefaultKernal   lda #128
                sta $9d; kernal messages on
EnableSTOP      LDA #237
                STA $0328
                rts
                
                
lname           !text "STATSCELL,P,R"
lname_end
sname           !text "@0:STATSCELL"
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

                LDA #<Stats_Time0
                STA $C1
                LDA #>Stats_Time0
                STA $C2

                LDX #<Seconds
                LDY #>Seconds
                LDA #$C1      ; start address located in $C1/$C2
                JSR $FFD8     ; call SAVE
                BCS error    ; if carry set, a save error has happened
                jsr DisableSTOP
                RTS
error           ; Akkumulator contains BASIC error code
                jsr DisableSTOP
                RTS


UpdateTimes     ;if time < time0 then time2 = time1 : time1 = time0 : time0 = time
                ;if time < time1 then time2 = time1 : time 1 = time
                ;if time < time2 then time2 = time
                lda #<Stats_Time0
                sta $fb
                lda #>Stats_Time0
                sta $fc
                jsr IsTimeSmallerThanFBFC
                lda res
                bne best
                
                lda #<Stats_Time1
                sta $fb
                lda #>Stats_Time1
                sta $fc
                jsr IsTimeSmallerThanFBFC
                lda res
                bne second_best
                
                lda #<Stats_Time2
                sta $fb
                lda #>Stats_Time2
                sta $fc
                jsr IsTimeSmallerThanFBFC
                lda res
                bne third_best
                
                rts
                
best            lda Stats_Time1
                sta Stats_Time2
                lda Stats_Time1+1
                sta Stats_Time2+1
                lda Stats_Time0
                sta Stats_Time1
                lda Stats_Time0+1
                sta Stats_Time1+1
                lda Seconds
                sta Stats_Time0
                lda Minutes
                sta Stats_Time0+1
                rts

second_best     lda Stats_Time1
                sta Stats_Time2
                lda Stats_Time1+1
                sta Stats_Time2+1
                lda Seconds
                sta Stats_Time1
                lda Minutes
                sta Stats_Time1+1
                rts

third_best      lda Seconds
                sta Stats_Time2
                lda Minutes
                sta Stats_Time2+1
                rts

; Expects 16 bit value in XY (hilo)
IsTimeSmallerThanFBFC
                ldy #1
                lda #0
                sta res
                lda Minutes
                cmp ($fb),y
                bcc is_smaller
                beq hi_equal
                rts
hi_equal        lda Seconds
                ldy #0
                cmp ($fb),y
                bcc is_smaller
                rts
is_smaller      lda #1
                sta res
                rts

!zone Sound
RattleDelay = 3
Freq            !byte 0
Freqs           !byte 30,31,32,33,34,35,36,37,38;30,32,34,36,38,40,42,44,46

SimpleCardSound lda #0
                sta sid
                sta sid+4
                sta sid+5
                sta sid+6
                ; frequency hi
                lda Freq
                sta sid+1
                ; turn on waveform
                lda #129
                sta sid+4
                rts

PlaceCardSound  lda #15
                sta Freq
                jsr SimpleCardSound
                
                ldx #0
                ldy #30
noploop         dex
                bne noploop
                dey
                bne noploop
                
                lda #20
                sta Freq
                jsr SimpleCardSound
                rts

                

StartRattle     lda #RattleDelay
                sta RattleCounter1
                lda #7
                sta RattleCounter2
                lda #1
                sta RattleSoundOn
                rts

!zone Data
bIsYes          !byte 0
bButtonPressing !byte 0
RattleSoundOn   !byte 0
RattleCounter1  !byte 0
RattleCounter2  !byte 0
; To save on disk -----------
Stats_Time0     !byte 0,10 ;best, format: sec, min
Stats_Time1     !byte 0,20 ;2nd best
Stats_Time2     !byte 0,30 ;3rd best
Stats_Wins      !byte 0,0
Stats_Lost      !byte 0,0
;----------------------------
Seconds         !byte 0
Minutes         !byte 0
DigitLo         !byte 0; For conversion
DigitHi         !byte 0; byte to decimal
WonFlag         !byte 0
PlayAgainFlag   !byte 0
MayUndo         !byte 0
SelMenuIndex    !byte 255
ClickedMenuIndex!byte 255
Transferable    !byte 0
RepositoryPos   !byte 0
UpperCard       !byte 0
LastCard        !byte 0
MapColor        !byte 0
PileFrom        !byte 0
PileTo          !byte 0
PileIndexFrom   !byte 0
BestPilePos     !byte 0
bSelected       !byte 0
SelPilePos      !byte 0
PileLength      !byte 0
Mult19          !byte 0,19,38,57,76,95,114,133
PileIndex       !byte 0
xPos            !byte 0
yPos            !byte 0
xPosDB          !byte 0
yPosDB          !byte 0
dummy           !byte 0
ValueToChar     !byte 65,50,51,52,53,54,55,56,57,88,74,81,75
SuitToChar      !byte 46,47,45,44
Suit            !byte 0;0,1,2,3
SuitCmp         !byte 0
Value           !byte 0;0-12
ValueCmp        !byte 0
Kings           !byte 12,25,38,51
Piles           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile1           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile2           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile3           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile4           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile5           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile6           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile7           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Repository      !byte 255,255,255,255
Aces            !byte 255,255,255,255
PileLengths     !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
TimerOn         !byte 1
Timer           !byte 60; counts down from 60 to 0
exit_code       !byte 0
; Exit Codes -------------------
bMoved          !byte 0
bLBtnPress      !byte 0
bLBtnRelease    !byte 0
bDblClk         !byte 0
bRestart        !byte 0
bWon            !byte 0
bExit           !byte 0
;-------------------------------
dx              !byte 0 ;joystick
dy              !byte 0 ;directions
res             !byte 0 ;return value for various functions
result          !byte 0 ;return value for card transfer functions
MouseInfo       !byte 0,0,0,0,0
counter         !byte 0
GameMode        !byte GM_NORMAL; 0: normal, 1: menu, 255: dialog
MenuMode        !byte 0; 0: MM_GAME, 1: MM_HELP
DialogMode      !byte 0;0: exit dlg, 1: won dlg, 2: stats dlg, 3: too many cards dlg, 4: instructions dlg, 5: about
MapWidth        !byte 0
MapHeight       !byte 0
Deck            !byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
                !byte 20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35
                !byte 36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51
ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
                !byte $05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07
ColorTabHi      !byte $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9
                !byte $d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db
;To be filled
DBScrTabLo      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
DBScrTabHi      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
DBColTabLo      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
DBColTabHi      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0

Colors          !bin "attribs.bin"; per char, thus 256 bytes
MenuMap         !bin "MenuMap.bin"
HelpMenuMap     !bin "HelpMenuMap.bin"
RepAcesMap      !bin "FC_RepAcesMap.bin"
FullCardMap     !bin "FullCardMap.bin"
CoverCardMap    !bin "CoverCardMap.bin"
RepCardMap      !bin "FC_RepCardMap.bin"
GameMenuMap     !bin "GameMenuMap.bin"
DBScrMap        !bin "GreenStripMap.bin"
DBColMap        !bin "GreenStripMap.bin"
TMC_DlgMap      !bin "TooManyCardsDlgMap.bin"
Won_DlgMap      !bin "WonDlgMap.bin"
Instr_DlgMap    !bin "InstrDlgMap.bin"
About_DlgMap    !bin "AboutDlgMap.bin"
RUSure_DlgMap   !bin "AreYouSureDlgMap.bin"
Stats_DlgMap    !bin "StatsDlgMap.bin"
Thanks_DlgMap   !bin "ThanksDlgMap.bin"

; Action: fromPile, toPile, no of cards
; ActionStack is a list of Actions (unlimited no)
Action          !byte 0,0,0
ActionStackPtr  !byte 0,0
ActionStack     !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
