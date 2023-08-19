!to "solitaire.d64",d64

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
CHAR_BASE = $4000
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
DM_OPTIONS = 7
GM_NORMAL = 0
GM_MENU = 1
GM_DIALOG = $ff
MM_GAME = 0
MM_HELP = 1
GM_MI_NEW = 0
GM_MI_OPTIONS = 1
GM_MI_STATS = 2
GM_MI_EXIT = 3
HM_MI_HELP = 0
HM_MI_ABOUT = 1
;Sprite Blocks
SP_Mouse0 = (Mousepointer0Addr - VIC_BANK)/64
SP_Mouse1 = (Mousepointer1Addr - VIC_BANK)/64
SP_MenuItemHighliter = (MenuItemHighliterAddr - VIC_BANK)/64
SP_EmptyPlace1 = (EmptyPlace1Addr - VIC_BANK)/64
SP_EmptyPlace2 = (EmptyPlace2Addr - VIC_BANK)/64
SP_ButtonUL = (ButtonULAddr - VIC_BANK)/64
SP_ButtonLR = (ButtonLRAddr - VIC_BANK)/64


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

*=$4c00
MouseOn         !byte 1

; 7 Sprites (free space for 1 more)
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

EmptyPlace1Addr
!byte $7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$7f,$ff,$ff
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$0d

EmptyPlace2Addr
!byte $ff,$ff,$fe,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$0d

ButtonULAddr
!byte $ff,$ff,$00,$ff,$fe,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$80,$00,$00
!byte $80,$00,$00,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00

ButtonLRAddr
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$ff,$ff,$00,$ff,$ff,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00


!zone Main
*=$4e00
                jsr SetGlobals
                jsr InstallIRQ
                jsr LoadStats
                jsr SetColor
StartGame       jsr Initialize
                ldy #CL_DARKGREEN
                jsr ClearScreen
                jsr DrawMenu
                jsr ShuffleDeck
                jsr DeckToPiles
                jsr StartRattle
                jsr DrawPiles
                jsr DrawPanel


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

Restart         lda #0
                sta bRestart
                jmp StartGame

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



!zone ____________Events______________
;============================================= OnButtonPress
OnLBtnPress     jsr MouseToScr
                lda $d001
                sta Rect+2
                lda GameMode
                cmp #GM_NORMAL
                bne +
                lda bMayFillEmAll
                beq +
                jsr IsInButton
                lda res
                beq +
                jsr PressButton
                lda #1
                sta bMouseInButton
                sta bButtonPressing
                rts
+               cmp #GM_DIALOG
                bne pr_not_in_btn
                ; Pressed in Dialog Mode
                lda DialogMode
                cmp #DM_INSTRUCTS
                beq pr_instructs
                cmp #DM_ABOUT
                beq pr_about
                cmp #DM_STATS
                beq pr_stats
                cmp #DM_OPTIONS
                beq pr_options
                cmp #DM_NEW
                beq pr_newexit
                cmp #DM_EXIT
                beq pr_newexit
                cmp #DM_WON
                beq pr_won
                rts
pr_about        jsr IsInAboutDlgBtn
-               lda res
                beq pr_not_in_btn
                jsr PressOk
                rts
pr_instructs    jsr IsInInstDlgBtn
                jmp -
pr_stats        jsr IsInStatsDlgBtn
                jmp -
pr_options      jsr IsInOptsDlgBtn
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

;============================================= OnDoubleClick
OnDoubleClick   lda GameMode
                cmp #GM_NORMAL
                bne +
                lda Offset
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
                cmp #DM_INSTRUCTS
                beq instructs
                cmp #DM_ABOUT
                beq about
                cmp #DM_NEW
                beq asknew
                cmp #DM_EXIT
                beq askexit
                cmp #DM_STATS
                beq stats
                cmp #DM_WON
                beq won
                cmp #DM_OPTIONS
                beq options
                rts
about           jsr IsInAboutDlgBtn
                lda res
                beq not_in_dlg_btn
same2           jsr ReleaseOk
                jsr Pause
                jsr GotoNormalMode
                rts
instructs       jsr IsInInstDlgBtn
                lda res
                beq not_in_dlg_btn
                jsr same2
                lda #1
                sta TimerOn
                rts
stats           jsr IsInStatsDlgBtn
                lda res
                beq not_in_dlg_btn
                jsr same2
                lda WonFlag
                bne start_over
                rts
start_over      lda #0
                sta WonFlag
                lda #1
                sta bRestart
                rts
askexit         lda #<bExit
                sta here+1
                lda #>bExit
                sta here+2
                jmp asknewexit
asknew          lda #<WonFlag
                sta here+1
                lda #>WonFlag
                sta here+2
                jmp asknewexit
not_in_dlg_btn  rts
won             jsr IsInWonDlgBtns
                lda res
                beq not_in_dlg_btn
                lda VIC
                cmp #208
                bcc play_again
                cmp #216
                bcc not_in_dlg_btn
                lda bIsYes
                bne not_in_dlg_btn
                jsr ReleaseNo
                jsr Pause
                lda #1
                sta bExit
                rts
options         jsr ReactOptsDlg
                lda res
                beq +
                jsr ReleaseOk
                jsr Pause
                jsr GotoNormalMode
                jsr SetOptions
+               rts
play_again      lda bIsYes
                bne +
                rts
+               jsr ReleaseYes
                jsr Pause
                jsr RestoreScreen
                jsr ShowStatsDlg
                rts
asknewexit      jsr IsInYesNoBtns
                lda res
                beq not_in_dlg_btn
                lda VIC
                cmp #208
                bcc newexit
                cmp #216
                bcc not_in_dlg_btn
                lda bIsYes
                bne not_in_dlg_btn  
                jsr ReleaseNo
                jsr Pause
                jmp GotoNormalMode
newexit         lda bIsYes
                bne +
                rts
+               lda #1
here            sta $ffff; put your addr here before calling
                jsr ReleaseYes
                jsr Pause
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
;MM_GameMenu    ;Game Menu is pulled down
                jsr IsInGameMenu
                lda res
                beq GotoNormalMode
            ;GM_Action
                jsr GotoNormalMode
                jsr GM_Actions
                rts
MM_HelpMenu     ; Help Menu is pulled down
                jsr IsInHelpMenu
                lda res
                beq GotoNormalMode
            ;HM_Action
                jsr GotoNormalMode
                jsr HM_Actions
                rts
GotoNormalMode  lda VIC+21
                and #%11001111
                sta VIC+21
                jsr RestoreScreen
                lda Stats_WithTimer
                beq +
                jsr display_time
+               lda #GM_NORMAL
                sta GameMode
                rts
;----- In normal mode ------------------------
RelInNormalMode2 lda bButtonPressing
                beq +
                lda #0
                sta bButtonPressing
                jsr IsInButton
                lda res
                beq +
                ; Pressed on button
                jsr FillEmAll
                jsr DrawPiles
                jsr DrawPanel
                jsr ReleaseButton
                lda #1
                sta bWon
                rts
+               lda Offset
                beq +
                ; Offset > 0
                lda bSelected
                bne +
                ; not selected
                jsr IsInPileFrom
                lda res
                bne +
                ; not in PileFrom
                jsr Deselect; back to normal
                rts
+               lda Rect+1
                cmp #1
                bcs rel_in_area
;released in menu
                lda Rect
                cmp #6
                bcc in_game_menu
                cmp #12
                bcc in_help_menu
                rts
in_help_menu    jsr ShowHelpMenu
                rts
in_game_menu    jsr ShowGameMenu
                rts
rel_in_area     lda Rect
                cmp #35
                bcc in_std_piles
                ; here: if clicked in right pane
                jsr ClickedPanel
                rts
in_std_piles    jsr ClickedPiles
                rts
;---------------------------------
IsInPileFrom    lda #0
                sta res
                ;jsr MouseToScr
                lda Rect+1
                bne +
                rts
+               lda Rect
                ldx PileFrom
                cmp Mult5,x
                bcs +
                rts
+               inx
                cmp Mult5,x
                bcc +
                rts
+               lda #1
                sta res
                rts
;============================================= OnMouseMove

OnMouseMove     lda GameMode
                beq MovInNormalMode
                bpl MovInMenuMode
                jmp MovInDlgMode
                
;----- Rel jump too far ----------------------
MovInMenuMode   jsr MovInMenuMode2
                rts
MovInNormalMode jsr MovInNormalMode2
                rts
MovInDlgMode    jsr MovInDlgMode2
                rts

;----- In dialog mode ------------------------
MovInDlgMode2   lda #0
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
                cmp #DM_OPTIONS
                beq prrel_options
                cmp #DM_NEW
                beq prrel_newexit
                cmp #DM_EXIT
                beq prrel_newexit
                cmp #DM_WON
                beq prrel_won
                rts
prrel_about     jsr IsInAboutDlgBtn
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
prrel_options   jsr IsInOptsDlgBtn
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
;----- In normal mode ------------------------
MovInNormalMode2
                lda #1
                sta ProcessDblClk
                lda bMayFillEmAll
                beq ++
                ; if buttonpressing==true
                ; {
                ;     if mouseinbutton=false
                ;          if mouseinbutton()
                ;          {   
                ;              mouseinbutton=true
                ;              pressButton()
                ;          }
                ;     if mouseinbutton==true
                ;          if !mouseinbutton()
                ;          {
                ;              mouseinbutton=false
                ;              releaseButton()
                ;          }
                ; }
                lda bButtonPressing
                beq ++
                jsr IsInButton
                lda bMouseInButton
                bne +
                lda res
                beq ++
                lda #1
                sta bMouseInButton
                jsr PressButton
                rts
+               lda res
                bne ++
                lda #0
                sta bMouseInButton
                jsr ReleaseButton
++              rts
;----- In menu mode --------------------------
MovInMenuMode2  lda #0
                sta ProcessDblClk
                jsr MouseToScr
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
;=============================================

!zone MenuActions
GM_Actions      lda SelMenuIndex
                cmp #GM_MI_NEW
                beq ACTION_NEW
                cmp #GM_MI_OPTIONS
                beq ACTION_OPTIONS
                cmp #GM_MI_STATS
                beq ACTION_STATS
                cmp #GM_MI_EXIT
                beq ACTION_EXIT
ACTION_NEW      jsr ShowRUSureDlg
                lda #DM_NEW
                sta DialogMode
                rts
ACTION_OPTIONS  jsr ShowOptionsDlg
                rts
ACTION_STATS    jsr ShowStatsDlg
                rts
ACTION_EXIT     jsr ShowRUSureDlg
                lda #DM_EXIT
                sta DialogMode
                rts
HM_Actions      lda SelMenuIndex
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
                lda VIC+21
                ora #%00110000
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
                lda VIC+21
                ora #%00110000
                sta VIC+21
                
                ;sel item->white, others->black
                jsr DrawGameMenuMap
                ldy #81
                jsr SelectText
                rts

;Expects #81 or #87 in Y (depends on menu [GM/HM])
SelectText      lda #$00
                sta $fb
                lda #$d8
                sta $fc
                jsr AddYtoFBFC
                ; fill dummy (10 or 11)
                lda #10
                sta dummy
                cpy #87
                bne gm
                inc dummy
                ; compute col mem pos
gm              ldx #0
                lda SelMenuIndex
                asl
                asl
                clc
                adc SelMenuIndex
                asl
                asl
                asl                
                asl
                bcc cc
                inx ;ldx #1
cc              tay
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
                and #%00000001
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
                and #%00000001
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
                lda #12
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
                cpx #12
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
acc      = 1     ; accelaration
ProcessDblClk   !byte 1; If set, processes double clicks
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
                lda dc_counter
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
                
                ; Install empty place sprite
                lda #SP_EmptyPlace1
                sta SPRPTR_2
                lda #SP_EmptyPlace2
                sta SPRPTR_3
                lda #CL_LIGHTGREEN
                sta col2
                sta col3
                ;y=134,   x=44
                lda #52
                sta VIC+4
                lda #60
                sta VIC+6
                lda #134
                sta VIC+5
                sta VIC+7
                
                ; Install menu item highliter sprite
                lda #SP_MenuItemHighliter
                sta SPRPTR_4
                sta SPRPTR_5
                lda #2
                sta col4
                sta col5
                ;select box behind text
                lda #%00110000
                sta VIC+27
                
                ;Install button sprites
                lda #SP_ButtonUL
                sta SPRPTR_6
                lda #SP_ButtonLR
                sta SPRPTR_7
                lda #CL_LIGHTGREY
                sta col6
                lda #CL_DARKGREY
                sta col7
                ; stretch in x direction (also menu highlighter)
                lda #%11110000
                sta VIC+29
                lda #52
                sta VIC+12
                sta VIC+14
                lda #134
                sta VIC+13
                sta VIC+15
                
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
                
                ;Prepare DB line start tables (scr/col)
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
                cpx #24
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
                cpx #24
                bcc loopcoltab
                
                ;Prepare OneTwoThree
                lda #<One
                sta OneTwoThreeLo
                lda #>One
                sta OneTwoThreeHi
                lda #<Two
                sta OneTwoThreeLo+1
                lda #>Two
                sta OneTwoThreeHi+1
                lda #<Three
                sta OneTwoThreeLo+2
                lda #>Three
                sta OneTwoThreeHi+2
                rts


Initialize      lda #0
                sta bSelected
                sta bMayFillEmAll
                ;prevents Undo before first action
                sta LastAction+2
                ; init time
                lda #0
                sta Seconds
                sta Minutes
                lda #1
                sta TimerOn
                lda #60
                sta Timer
                lda #255
                sta SelMenuIndex
                ; init aces
                lda #$ff
                ldx #3
init_aces       sta Aces,x
                dex
                bpl init_aces
                ; Normal game mode
                lda #GM_NORMAL
                sta GameMode
                ; Only mouse pointer sprites
                lda #%00000011
                sta VIC+21
                lda VIC+16
                and #%00000011
                sta VIC+16
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
display_time    lda Stats_WithTimer
                bne +
                lda #32
                sta SCRMEM+35
                sta SCRMEM+36
                sta SCRMEM+37
                sta SCRMEM+38
                sta SCRMEM+39
                rts
+               lda Seconds
                jsr ByteToDeciChar
                lda DigitHi
                sta SCRMEM+38
                lda DigitLo
                sta SCRMEM+39
                lda Minutes
                jsr ByteToDeciChar
                lda DigitHi
                sta SCRMEM+35
                lda DigitLo
                sta SCRMEM+36
                lda #30
                sta SCRMEM+37
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
PressButton     lda #CL_LIGHTGREY
                sta col7
                lda #CL_DARKGREY
                sta col6
                lda #<ButtonPrMap
                sta $fb
                lda #>ButtonPrMap
                sta $fc
                lda #5
                sta MapWidth
                lda #3
                sta MapHeight
                ldy #35
                ldx #10
                jsr PosToScrMem
                jsr DrawMap
                rts


ReleaseButton   lda #CL_LIGHTGREY
                sta col6
                lda #CL_DARKGREY
                sta col7
                lda #<ButtonMap
                sta $fb
                lda #>ButtonMap
                sta $fc
                lda #5
                sta MapWidth
                lda #3
                sta MapHeight
                ldy #35
                ldx #10
                jsr PosToScrMem
                jsr DrawMap
                rts
                

IsInButton      lda #0
                sta res
                lda VIC+16
                and #%00000001
                beq +
                lda VIC
                cmp #52
                bcc +
                cmp #84
                bcs +
                lda VIC+1
                cmp #134
                bcc +
                cmp #150
                bcs +
                inc res
+               rts


DoubleClick     lda bSelected
                beq +
                rts
+               lda Rect
                cmp #35
                bcs in_panel
                ; here: if dbl clicked in piles
                ; Find PileFrom
                lda #5
                sta modulo
                lda Rect
                jsr Mod
                stx PileFrom
                ; Check if aces transfer is ok
                jsr GetPileInfo
                ldy PileLength
                cpy PileVisibleInd
                bne check_aces_tr
                rts
check_aces_tr   dey
                sty PileIndexFrom
                lda ($02),y
                jsr GetSuitAndValue
                lda Value
                sta ValueCmp
                ldx Suit
                lda Aces,x
                jsr GetSuitAndValue
                ldx Value
                inx
                cpx ValueCmp
                beq yesgoaces
                rts
yesgoaces       lda Suit
                clc
                adc #8
                sta PileTo
                jsr TransferCards
                ; Update piles visually
                ldx PileFrom
                lda #0
                sta Offset
                jsr DrawPile
                jsr DrawPanel
                jsr CheckWon
                rts
in_panel        ; Dblclicked in panel
                lda Rect+2
                cmp #155
                bcs +
                rts
+               ldx #7
                stx PileFrom
                jsr GetPileInfo
                ldy PileLength
                cpy PileVisibleInd
                ; if there is a card
                bne check_aces_tr
                rts
                

Deselect        lda #0
                sta bSelected
                sta Offset
                ldx PileFrom
                jsr DrawPile
                rts


ClickedPanel    lda Rect+1
                cmp #10
                bcc +
                cmp #13
                bcs +
                jsr MouseToScr
                lda Rect
                cmp #35
                bcs +
                rts
+               lda bMayFillEmAll
                beq +
                jsr IsInButton
                lda res
                beq +
                jsr ReleaseButton
                rts
+               lda Rect+2
                cmp #127
                bcc clicked_aces
                cmp #135
                bcc nothing
                cmp #155
                bcc clicked_udpile
                ; Clicked stack
                jsr ClickedStack
nothing         rts
clicked_udpile  jsr ClickedUDPile
                rts
clicked_aces    lda bSelected
                beq nothing
                jsr ToAcesTransfer
                rts


ClickedStack    ldx #7
                jsr GetPileInfo
                lda PileLength
                cmp PileVisibleInd
                bne +
                rts
+               lda bSelected
                beq +
                jsr Deselect
                rts
+               jsr MarkPile7
                lda #1
                sta bSelected
                rts


ClickedUDPile   lda bSelected
                beq +
                jsr Deselect
                rts
                ; not selected
+               ldx #7
                lda PileLengths,x
                bne +
                rts
                ; complete pile isn't empty
+               jsr PlaceCardSound
                ldx #7
                jsr GetPileInfo
                ldy PileVisibleInd
                beq stack_to_ud
                ;
                dec PileVisibleInds,x
                cpy PileLength
                beq +
                dey
                jsr MoveCardToEnd
                ;
+               jsr DrawPanel
                rts

stack_to_ud     ; all cards are on stack -> turn it upside down
                lda PileLength
                beq letitbe
                ldx #7
                sta PileVisibleInds,x
                tax
                dex
                ldy #0
-               lda Pile7,x
                sta DummyPile,y
                iny
                dex
                bpl -
                ldx PileLength
                dex
-               lda DummyPile,x
                sta Pile7,x
                dex
                bpl -
                jsr DrawPanel
letitbe         rts
                
MarkPile7       jsr PanelToDB; fills xPos/yPos for DrawDB
                ;lda Stats_DrawOne
                ;beq itsthree
                ; one card to select
                lda #12
                sta yPosDB
                lda #CL_DARKBLUE
                sta MapColor
                lda #0
                sta is_ud
                ldx #7
                ldy PileLengths,x
                dey
                stx PileFrom
                sty PileIndexFrom
                lda ($02),y
                sta UpperCard
                jsr DrawTopCardDB
                jsr DrawDB
                lda #CL_DARKGREEN
                sta MapColor
                rts
;itsthree        ; three cards to select
;                rts
                
                
ClickedPiles    lda bSelected
                bne selected
                jsr NoSelClick
                lda res; if 0 then no selection
                sta bSelected
                rts
selected        jsr ToPileTransfer
                rts


NoSelClick      lda #0
                sta res
                ; Find pile
                lda #5
                sta modulo
                lda Rect
                jsr Mod
                ; now pile_from is in X
                cpx #7
                bcc +
                rts
+               lda Offset
                beq offset_is_zero
                cpx PileFrom
                beq offset_is_zero
                jsr Deselect
                rts
offset_is_zero  stx PileFrom
                jsr GetPileInfo
                ; Decide select/open last card
                lda PileLength
                beq +
                cmp PileVisibleInd
                bne WannaSelect
                ; Open last card
                dec PileVisibleInds,x
                lda #0
                sta Offset
                jsr DrawPile
                jsr PlaceCardSound
                jsr MayFillEmAll
                lda bMayFillEmAll
                beq +
                jsr DrawPanel; draws the button instead of free place
+               rts

WannaSelect     ; Find selected pos in pile => PileIndexFrom
                jsr GetPileIndAbstr; fills SelPileInd
                lda SelPileInd
                cmp #$ff
                bne +
                ; Draw lower part of pile ;;;;;;;;;;;;;;;;;;;;
                ; MAYBE: - SaveScreen
                ;        - GreyOutEverythingElse
                lda #8
                sta Offset
                jsr DrawPile
                rts
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
+               ldx PileFrom; has been filled in NoSelClick
                lda SelPileInd
                sta PileIndexFrom
                cmp PileVisibleInd
                bcs +
                lda PileVisibleInd
                sta PileIndexFrom
                
                ; Exit if PileIndexFrom >= PileLength (actually never happens)
+               ldx PileFrom
                jsr GetPileInfo;     necessary????????????????????????????
                lda PileIndexFrom
                cmp PileLength
                ;bcs stop_draw2; zero selection length
                bcc +
                jmp stop_draw
+               ; Fill yPosDB
                ldx PileFrom
                jsr PileToDB; Draw pile to buffer first
                lda PileIndexFrom
                sta pile_index
                jsr GetYFromPileInd
                lda res
                sta yPosDB
                ; Draw first card
                lda #CL_DARKBLUE
                sta MapColor
                lda #0
                sta is_ud
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
                txa
                sec
                sbc Offset
                cmp #10
                bcc normal_pail
;it's a long pile
                ldy PileIndexFrom
                iny
draw_down2      lda ($02),y
                inc yPosDB
                inc yPosDB
                jsr DrawCoverCardDB
                iny
                cpy #9
                bne draw_down2
                
                inc yPosDB
                ldy PileLength
                dey
                lda ($02),y
                jsr DrawCoverCardDB
                jsr DrawDB
                lda #CL_DARKGREEN
                sta MapColor
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
                
stop_draw2      jsr DrawDB; xPos/yPos have been filled in NoSelClick
                lda #CL_DARKGREEN
                sta MapColor
                rts

                
pilelen         !byte 0; internal
;Expects pile no in X
;Writes to SelPilePos
GetPileIndAbstr lda PileLength
                sec
                sbc Offset
                sta pilelen
                cmp #11
                bcs pile_long
                lda Rect+2; contains $d001
pile_normal     sec
                sbc #62
                lsr
                lsr
                lsr
                lsr
                cmp #14
                bcc go_on
                lda #0
go_on           cmp pilelen
                bcs last
                clc
                adc Offset
                sta SelPileInd
                rts
last            ldx PileLength
                dex
                stx SelPileInd
                rts
pile_long       lda Rect+2
                cmp #189
                bcc pile_normal
                cmp #197
                bcs last
                lda #$ff
                sta SelPileInd
                rts


pile_index      !byte 0
;Expects pile_index to be filled, result in res
GetYFromPileInd lda PileLength
                sec
                sbc Offset
                sta pilelen
                cmp #11
                bcs pile_long1
                lda pile_index
                sec
                sbc Offset
doch_normal     asl
                sta res
                rts
pile_long1      lda pile_index
                cmp #8
                bcc doch_normal
                lda #17
                sta res
                rts


ToAcesTransfer  ; Get suit and value from card
                ldx PileFrom
                jsr GetPileInfo
                ldy PileLength
                dey
                lda ($02),y
                jsr GetSuitAndValue
                lda Suit
                sta SuitCmp
                lda Value
                sta ValueCmp
                ; Get ace suit
                lda Rect+2
                sec
                sbc #62
                lsr
                lsr
                lsr
                lsr
                cmp #15
                bcs desel
                sta Suit
                tax
                clc
                adc #8
                sta PileTo
                lda Aces,x
                ; Check if transfer is ok
                cmp #255
                bne check_normal
                ; no ace yet on pile
                lda ValueCmp
                bne desel; not ok
                lda SuitCmp
                cmp Suit
                bne desel; not ok
                jmp aces_go
check_normal    jsr GetSuitAndValue; of Aces,x
                ldx Value
                inx
                cpx ValueCmp
                bne desel; not ok
                lda SuitCmp
                cmp Suit
                bne desel; not ok
aces_go         ; Ok
                ldx PileFrom
                lda PileLengths,x
                tax
                dex
                stx PileIndexFrom;only last card in pile
                jsr TransferCards
                jsr DrawPanel
desel           jsr Deselect
                jsr CheckWon
                rts
                

CheckWon        lda Aces
                cmp #12
                bne +
                lda Aces+1
                cmp #25
                bne +
                lda Aces+2
                cmp #38
                bne +
                lda Aces+3
                cmp #51
                bne +
                lda #1
                sta bWon
+               rts

                
IsKing          lda #0
                sta res
                lda #13
                sta modulo
                lda UpperCard
                sec
                sbc #12
                jsr Mod
                bne +
                lda #1
                sta res
+               rts
                

ToPileTransfer  ; Find PileTo
                lda #5
                sta modulo
                lda Rect
                jsr Mod
                stx PileTo
                ; Check if transfer is ok
                jsr GetPileInfo
                ldy PileLength
                bne +
                jsr IsKing
                lda res
                bne go_transfer
                beq DeSelect
                ; Do suit and value match
+               dey
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
go_transfer     jsr TransferCards
                ldx PileTo
                lda #0
                sta Offset
                jsr DrawPile
                ; Deselect
DeSelect        jsr Deselect
                rts

!zone Graphics
; Expects scr pos in Y,X
; Output: scr mem adr in FDFE
PosToScrMem     sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $fd
                lda ScrTabHi,x
                adc #0
                sta $fe
                rts

; Returns Mouse pos in scr coords in Rect
MouseToScr      lda $d000
                sec
                sbc #24
                sta Rect
                lda $d010
                and #%00000001
                sbc #0
                lsr; carry or not!
                lda Rect
                ror
                lsr
                lsr
                sta Rect

                lda $d001
                sec
                sbc #50
                lsr
                lsr
                lsr
                sta Rect+1
                rts

ClearDB         ldx #119
loopclrdb       lda #160
                sta DBScrMap,x
                lda #CL_DARKGREEN
                sta DBColMap,x
                dex
                bpl loopclrdb
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

; Expects color in Y
ClearScreen     ldx #0
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

;Expects
;char in A
;color in clr
;Scr Pos in xPos,yPos
clr             !byte 0
                
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
                

; Expects:
;  map mem pos in $fb
;  map height in MapHeight
;  yPosDB (offset)
DrawMapToDB     lda #<DBScrMap
                sta $fd
                lda #>DBScrMap
                sta $fe
                lda yPosDB
                asl
                asl
                clc
                adc yPosDB
                tay
                jsr AddYtoFDFE
                
                lda #<DBColMap
                sta $04
                lda #>DBColMap
                sta $05
                jsr AddYto0405
                
                ldx MapHeight
                dex
                stx counter
outer_draw1     lda #5
                tay
                dey
draw_map1       lda ($fb),y
                sta ($fd),y
                ; Adjust color---------
                tax
                lda Colors,x
                sta ($04),y
                ;----------------------
                dey
                bpl draw_map1
                
                ldy #5
                jsr AddYtoFBFC
                jsr AddYtoFDFE
                jsr AddYto0405
                dec counter
                bpl outer_draw1
                rts

; Uses internally: $04/$05 (for color mem ptr)
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
                stx Offset
draw_pile_loop  jsr DrawPile
                inx
                cpx #7
                bcc draw_pile_loop
                rts
                
;Expects pile no in X and Offset filled
DrawPile        jsr PileToDB
                jsr DrawDB
                rts
                
DrawPanel       jsr PanelToDB
                jsr DrawDB
                rts


PanelToDB       jsr ClearDB
                lda #<AcesMap
                sta $fb
                lda #>AcesMap
                sta $fc
                lda #9
                sta MapHeight
                lda #35
                lda #0
                sta yPosDB
                jsr DrawMapToDB
                
                ; Draw UD pile
                ; One, two or more on UD pile
                ldx #7
                ldy PileVisibleInds,x
                sty UD_HasCards
                beq free_place
                ; has cards
                dey
                cpy #2
                bcc +
                ldy #2
+               lda OneTwoThreeLo,y
                sta $fb
                lda OneTwoThreeHi,y
                sta $fc
                lda #1
                sta MapHeight
                lda #11
                sta yPosDB
                jsr DrawMapToDB
                lda VIC+21
                and #%11110011
                sta VIC+21
                jsr DrawUDPile
                jmp fill_aces
                
free_place      lda VIC+16
                ora #%11001100
                sta VIC+16
                jsr MayFillEmAll
                lda bMayFillEmAll
                bne +
                lda VIC+21
                and #%00111111
                ora #%00001100
                sta VIC+21
                jmp fill_aces
                ; bMayFillEmAll = true
+               jsr DrawButtonChars
                lda VIC+21
                and #%11110011
                ora #%11000000
                sta VIC+21
                
fill_aces       lda #9
                sta yPosDB
                ; Start a loop
                lda #4
                sta counter
-               dec counter
                bmi draw_stack
                dec yPosDB
                dec yPosDB
                ldx counter
                lda Aces,x
                bmi -
                ; draw suit and value
                jsr GetSuitAndValue
                lda #1
                sta xPosDB
                lda Suit
                and #%11111110
                sta clr
                ldx Value
                cpx #9
                beq its_a_rep_ten
                ; Not a ten
                ldx Value
                lda ValueToChar,x
                jsr DrawCharColorDB
                inc xPosDB
                lda #32
                jsr DrawCharColorDB
                inc xPosDB
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColorDB
                jmp -
its_a_rep_ten   ; A ten
                lda #49
                jsr DrawCharColorDB
                inc xPosDB
                lda #58
                jsr DrawCharColorDB
                inc xPosDB
                ldx Suit
                lda SuitToChar,x
                jsr DrawCharColorDB
                jmp -
                
draw_stack      ; Draw stack (waste)
                ;lda Stats_DrawOne
                ;beq three_cards
                ; one card to pull
                lda #12
                sta yPosDB
                lda #0
                sta is_ud
                ldx #7
                jsr GetPileInfo
                ldy PileLength
                tya
                sec
                sbc PileVisibleInd
                sta cards_on_stack
                beq draw_panel; nothing to paint
                dey
                lda ($02),y; Show 'last card' of stack
                jsr DrawTopCardDB
                ; One, two or more on stack
                lda PileLength
                sec
                sbc PileVisibleInd
                tay; length in y
                dey
                cpy #2
                bcc +
                ldy #2
+               lda OneTwoThreeLo,y
                sta $fb
                lda OneTwoThreeHi,y
                sta $fc
                lda #1
                sta MapHeight
                lda #17
                sta yPosDB
                jsr DrawMapToDB
                
;                jmp draw_panel
;three_cards     ; three cards to pull
;                lda #12
;                sta yPosDB
;                lda #0
;                sta is_ud
;                ldx #7
;                jsr GetPileInfo
;                ldy PileVisibleInd
;                cpy PileLength
;                bcs draw_panel; nothing to paint
;                ldy PileLength
;                dey
;                lda ($02),y; Show 'last card' of stack
;                jsr DrawTopCardDB
                
draw_panel      ; Perpare draw buffer
                lda #35
                sta xPos
                lda #1
                sta yPos
                rts
cards_on_stack  !byte 0


DrawButtonChars lda #<ButtonMap
                sta $fb
                lda #>ButtonMap
                sta $fc
                lda #5
                sta MapWidth
                lda #3
                sta MapHeight
                lda #9
                sta yPosDB
                jsr DrawMapToDB
                rts


DrawUDPile      lda #<UDMap
                sta $fb
                lda #>UDMap
                sta $fc
                lda #2
                sta MapHeight
                lda #9
                sta yPosDB
                jsr DrawMapToDB
                ; Correct pattern
                inc yPosDB
                lda #1
                sta xPosDB
                lda Stats_CardColor
                sta clr
                lda Stats_Pattern
                asl
                clc
                adc #166
                sta char_ptrn
                jsr DrawCharColorDB
                inc xPosDB
                lda char_ptrn
                jsr DrawCharColorDB
                inc xPosDB
                lda char_ptrn
                jsr DrawCharColorDB
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
                lda #24
                sta MapHeight
                jsr DrawScrClrMap
                pla
                tax
                rts


;Expects pile no in X and Offset filled
PileToDB        cpx #7
                bcc +
                jmp PanelToDB
                ; save X on stack
+               txa
                pha
                ; Initialize MapColor and x/yPosDB
                lda #CL_DARKGREEN
                sta MapColor
                lda #0
                sta yPosDB
                sta xPosDB
                ; Fill x/yPos (where will DB be painted on screen)
                lda #1
                sta yPos
                stx dummy
                txa
                asl
                asl
                clc
                adc dummy
                sta xPos
                ; Fill DB with green
                jsr GetPileInfo
                jsr ClearDB
                ; Compute no of cards to draw
                lda PileLength
                sec
                sbc Offset
                beq stop_draw
                ; Draw first card to DB
                ; Offset < VisibleInd <=> ud=true
                lda #0
                sta is_ud
                ldy Offset
                cpy PileVisibleInd
                bcs +
                lda #1
                sta is_ud
+               lda ($02),y
                jsr DrawTopCardDB
                ; Compute no of cards to draw
                lda PileLength
                sec
                sbc Offset
                cmp #1; If only 1=>stop
                beq stop_draw
                ; Distinguish long/short pile to draw(!)
                cmp #11; If less than 11=>short pile
                bcc short_pile
                
                ; It's a long pile
                ldy Offset
                iny
draw_down1      cpy PileVisibleInd
                php
                pla
                and #%00000001
                eor #%00000001
                sta is_ud
                lda ($02),y
                inc yPosDB
                inc yPosDB
                jsr DrawCoverCardDB
                iny
                cpy #9
                bne draw_down1
                
                inc yPosDB
                ldy PileLength
                dey
                lda ($02),y
                jsr DrawCoverCardDB
                pla
                tax
                rts
                
short_pile      ldy Offset
                iny
draw_down       cpy PileVisibleInd
                php
                pla
                and #%00000001
                eor #%00000001
                sta is_ud
                lda ($02),y
                inc yPosDB
                inc yPosDB
                jsr DrawCoverCardDB
                iny
                cpy PileLength
                bne draw_down
                
stop_draw       pla
                tax
                rts


is_ud           !byte 0; upside-down or not
;Expects pos in yPosDB, card no in A, MapColor, and is_ud (upside-down)
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


covers_ud       !byte 0; internal variable
;Expects pos in yPosDB, card no in A, MapColor
DrawCoverCardDB pha
                ; Find out if card covers an ud card
                dey
                cpy PileVisibleInd
                php
                pla
                and #%00000001
                eor #%00000001
                sta covers_ud
                iny
                
                pla
                tax
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
                
                ; correct this if card covers upside-down card
                lda covers_ud
                beq covers_normal
                lda #1
                sta xPosDB
                lda Stats_CardColor
                sta clr
                ; Set char for pattern
                lda Stats_Pattern
                asl
                clc
                adc #167
                sta char_ptrn
                jsr DrawCharColorDB
                inc xPosDB
                lda char_ptrn
                jsr DrawCharColorDB
                inc xPosDB
                lda char_ptrn
                jsr DrawCharColorDB
covers_normal   pla
                tay
                rts

cntr            !byte 0
char_ptrn       !byte 0
;Draws card content to double buffer with Value and Suit at yPosDB
DrawSuitValDB   lda yPosDB
                pha
                inc yPosDB
                lda #1
                sta xPosDB
                
                lda is_ud
                beq not_ud
                ; Draw upside-down card
                lda Stats_CardColor
                sta clr
                lda #3
                sta cntr
                ; Set char for pattern
                lda Stats_Pattern
                asl
                clc
                adc #166
                sta char_ptrn
wieeda          lda char_ptrn
                jsr DrawCharColorDB
                inc xPosDB
                lda char_ptrn
                jsr DrawCharColorDB
                inc xPosDB
                lda char_ptrn
                jsr DrawCharColorDB
                lda #1
                sta xPosDB
                inc yPosDB
                dec cntr
                bpl wieeda
                
                pla
                sta yPosDB               
                rts
                
not_ud          lda Suit
                and #%00000010
                sta clr
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

!zone GeneralDialogRoutines
; Is invoked when mouse button is released
; Returns 1 iff Ok button was pressed
ReactOptsDlg    lda #0
                sta res
                ; Is mouse in relevant area
                ldx Rect
                ldy Rect+1
                cpx #11
                bcc +
                cpx #28
                bcs +
                cpy #6
                bcc +
                cpy #23
                bcs +
                jmp ++
+               rts

++              jsr IsInOptsDlgBtn; res is filled here
                lda res
                beq +
                rts
                
+               ; NOT clicked in button
                cpy #9
                bcc InPatternArea
                cpy #10
                beq InColorArea
                cpy #15
                bcc +
                cpy #18
                bcc InMiscArea
+               rts

InMiscArea      cpx #13
                bcs +
                rts
+               cpx #23
                bcc +
                rts
+               cpy #15
                beq SoundOn
                ; Timer On/Off option pressed
                lda Stats_WithTimer
                and #%00000001
                eor #%00000001
                sta Stats_WithTimer
                lda #155
                clc
                adc Stats_WithTimer
                sta SCRMEM+693
                rts
SoundOn         lda Stats_SoundOn
                and #%00000001
                eor #%00000001
                sta Stats_SoundOn
                lda #155
                clc
                adc Stats_SoundOn
                sta SCRMEM+613
                rts

InPatternArea   jsr ChoosePattern
                rts
                
InColorArea     lda Stats_CardColor
                and #%00001111
                cpx #19
                beq decCol
                cpx #25
                beq incCol
                rts
decCol          tax
                dex
                cpx #1
                bne +
                dex
+               stx Stats_CardColor
                jmp change_color
incCol          tax
                inx
                cpx #1
                bne +
                inx
+               stx Stats_CardColor
change_color    ; Change color visually
                lda Stats_CardColor
                sta $d9a5
                sta $d9a6
                sta $d9a7
                sta $d925
                sta $d926
                sta $d927
                sta $d92a
                sta $d92b
                sta $d92c
                sta $d92f
                sta $d930
                sta $d931
                rts

ChoosePattern   ; Makem all green
                lda #<($d800+252); d8fc
                sta $fd
                lda #>($d800+252)
                sta $fe
                ldy #14
                lda #CL_DARKGREEN
-               sta ($fd),y
                dey
                bpl -
                ldy #40
                jsr AddYtoFDFE
                lda #CL_DARKGREEN
                ldy #0
                sta ($fd),y
                ldy #4
                sta ($fd),y
                iny
                sta ($fd),y
                ldy #9
                sta ($fd),y
                iny
                sta ($fd),y
                ldy #14
                sta ($fd),y
                ldy #40
                jsr AddYtoFDFE
                ldy #14
                lda #CL_DARKGREEN
-               sta ($fd),y
                dey
                bpl -
                ; Select
                lda #5
                sta modulo
                txa
                sec
                sbc #12
                jsr Mod
                stx Stats_Pattern
                jsr SelectPattern
                rts
                

SetColor        lda Stats_CardColor
                ldx #166
-               sta Colors,x
                inx
                cpx #172
                bcc -
                ;sta Colors+255
                rts


SetOptions      jsr SetColor
                ; Redraw piles if color or pattern has changed
                cmp prev_cardcolor
                bne RedrawAll
                lda Stats_Pattern
                cmp prev_pattern
                bne RedrawAll
                rts; no change (maybe only sound)
RedrawAll       jsr DrawPiles
                jsr DrawPanel
                rts

prev_cardcolor  !byte 0
prev_pattern    !byte 0
ShowOptionsDlg  lda Stats_CardColor
                sta prev_cardcolor
                lda Stats_Pattern
                sta prev_pattern
                
                jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_OPTIONS
                sta DialogMode
                lda #<OptionsDlgMap
                sta $fb
                lda #>OptionsDlgMap
                sta $fc
                lda #19
                sta MapWidth
                lda #22
                sta MapHeight
                ldy #10
                ldx #2
                jsr PosToScrMem
                jsr DrawMap
                ; Fill in information
                lda Stats_SoundOn
                clc
                adc #155
                sta SCRMEM+613
                lda Stats_WithTimer
                clc
                adc #155
                sta SCRMEM+693
                ; Fill color
                lda Stats_CardColor
                sta $d9a5
                sta $d9a6
                sta $d9a7
                ; Select pattern
SelectPattern   ldx Stats_Pattern
                lda Mult5,x
                clc
                adc #12
                tay
                ldx #6
                jsr PosToScrMem
                lda $fe
                clc
                adc #($d8 - >SCRMEM)
                sta $fe
                ldy #4
                lda #CL_DARKBLUE
-               sta ($fd),y
                dey
                bpl -
                ldy #40
                jsr AddYtoFDFE
                lda #CL_DARKBLUE
                ldy #0
                sta ($fd),y
                ldy #4
                sta ($fd),y
                ldy #40
                jsr AddYtoFDFE
                ldy #4
                lda #CL_DARKBLUE
-               sta ($fd),y
                dey
                bpl -
                rts
                

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
                ;; Wins
                ;lda Stats_Wins
                ;jsr ByteToDeciChar
                ;lda DigitHi
                ;ldy #80
                ;sta ($fb),y
                ;lda DigitLo
                ;iny
                ;sta ($fb),y
                ;; Lost
                ;lda Stats_Lost
                ;jsr ByteToDeciChar
                ;lda DigitHi
                ;ldy #98
                ;sta ($fb),y
                ;lda DigitLo
                ;iny
                ;sta ($fb),y
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
                lda #22
                sta MapHeight
                ldy #1
                ldx #2
                jsr PosToScrMem
                jsr DrawMap
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
                lda #11
                sta MapHeight
                ldy #10
                ldx #7
                jsr PosToScrMem
                jsr DrawMap
                rts

YesNoBtnsRect   !byte 168,248,159,174
StatsDlgBtnRect !byte 208,240,127,142
InstDlgBtnRect  !byte 120,248,215,230
AboutDlgBtnRect !byte 224,255,159,174
OptsDlgBtnRect  !byte 203,245,215,229
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

IsInYesNoBtns   lda #<YesNoBtnsRect
                sta $fb
                lda #>YesNoBtnsRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInStatsDlgBtn lda #<StatsDlgBtnRect
                sta $fb
                lda #>StatsDlgBtnRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInInstDlgBtn  lda #<InstDlgBtnRect
                sta $fb
                lda #>InstDlgBtnRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInAboutDlgBtn lda #<AboutDlgBtnRect
                sta $fb
                lda #>AboutDlgBtnRect
                sta $fc
                jsr IsInBtnRect
                rts

IsInOptsDlgBtn  lda #<OptsDlgBtnRect
                sta $fb
                lda #>OptsDlgBtnRect
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
                ldy #142
                jsr SwapChars
                ldx #143
                ldy #159
                jsr SwapChars
                ldx #122
                ldy #158
                jsr SwapChars
                ldx #118
                ldy #214
                jsr SwapChars
                ldx #120
                ldy #215
                jsr SwapChars
                ldx #245
                ldy #216
                jsr SwapChars
                ldx #246
                ldy #217
                jsr SwapChars
                ldx #247
                ldy #218
                jsr SwapChars
                ldx #242
                ldy #187
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
                ldy #130
                jsr SwapChars
                ldx #100
                ldy #131
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
                ldy #134
                jsr SwapChars
                ldx #102
                ldy #135
                jsr SwapChars
                ldx #103
                ldy #136
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
                ldy #137
                jsr SwapChars
                ldx #105
                ldy #138
                jsr SwapChars
                ldx #132
                ldy #142
                jsr SwapChars
                ldx #139
                ldy #159
                jsr SwapChars
                ldx #172
                ldy #158
                jsr SwapChars
                ldx #224
                ldy #214
                jsr SwapChars
                ldx #255
                ldy #215
                jsr SwapChars
                rts

Pause           ldx #32
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
; Expects card index in Y and pile index in X
MoveCardToEnd   lda Pile7,y
                sta dummy
                tya
                tax
                inx
-               lda Pile7,x
                sta Pile7,y
                inx
                iny
                cpx PileLength
                bcc -
                ldx PileLength
                dex
                lda dummy
                sta Pile7,x
                rts


FillEmAll       ldx #6
                lda #0
-               sta PileLengths,x
                sta PileVisibleInds,x
                dex
                bpl -
                lda #12
                sta Aces
                lda #25
                sta Aces+1
                lda #38
                sta Aces+2
                lda #51
                sta Aces+3
                rts
                
                
; Fills bMayFillEmAll
MayFillEmAll    lda #0
                sta bMayFillEmAll
                ldx #6
-               lda PileVisibleInds,x
                bne +
                dex
                bpl -
                ldx #7
                lda PileLengths,x
                bne +
                lda #1
                sta bMayFillEmAll
+               rts


DeckToPiles     lda #<Deck
                sta $fb
                lda #>Deck
                sta $fc
                ldy #51
                
                ldx #1
                stx PileLengths+0
                dex
                stx PileVisibleInds+0
                lda ($fb),y
                sta Piles,x
                dey
                
                ldx #2
                stx PileLengths+1
                dex
                stx PileVisibleInds+1
pile1           lda ($fb),y
                sta Pile1,x
                dey
                dex
                bpl pile1
                
                ldx #3
                stx PileLengths+2
                dex
                stx PileVisibleInds+2
pile2           lda ($fb),y
                sta Pile2,x
                dey
                dex
                bpl pile2
                
                ldx #4
                stx PileLengths+3
                dex
                stx PileVisibleInds+3
pile3           lda ($fb),y
                sta Pile3,x
                dey
                dex
                bpl pile3
                
                ldx #5
                stx PileLengths+4
                dex
                stx PileVisibleInds+4
pile4           lda ($fb),y
                sta Pile4,x
                dey
                dex
                bpl pile4
                
                ldx #6
                stx PileLengths+5
                dex
                stx PileVisibleInds+5
pile5           lda ($fb),y
                sta Pile5,x
                dey
                dex
                bpl pile5
                
                ldx #7
                stx PileLengths+6
                dex
                stx PileVisibleInds+6
pile6           lda ($fb),y
                sta Pile6,x
                dey
                dex
                bpl pile6
                
                ; pile on the right
                ldx #24
                stx PileLengths+7
                stx PileVisibleInds+7
                dex
pile7           lda ($fb),y
                sta Pile7,x
                dey
                dex
                bpl pile7
                rts


;Transfers cards internally.
;Needs PileFrom, PileIndexFrom, and PileTo filled
TransferCards   jsr PlaceCardSound
                ldx PileFrom
                stx LastAction
                jsr GetPileInfo
                
                ; Copy to dummy pile
                ldy PileIndexFrom
                ldx #0
transloop1      lda ($02),y
                sta DummyPile,x
                inx
                iny
                cpy PileLength
                bne transloop1
                ; Save no of cards for transfer
                stx LastAction+2
                ; Shorten pilefrom by X cards
                ldx PileFrom
                lda PileLengths,x
                sec
                sbc LastAction+2
                sta PileLengths,x
                
                ; Transfer to target pile
                ldx PileTo
                stx LastAction+1
                jsr GetPileInfo
                ldx #0
                ldy PileLength
transloop2      lda DummyPile,x
                sta ($02),y
                iny
                inx
                cpx LastAction+2
                bne transloop2
                
                ; LastAction+3 will serve as IndexFrom for Undo
                lda PileLength
                sta LastAction+3
                
                ; Expand pileTo->Length by LastAction+2 cards
                lda PileTo
                cmp #8
                bcc can_expand; don't expand if aces pile
                rts
can_expand      lda PileLength
                clc
                adc LastAction+2
                ldx PileTo
                sta PileLengths,x
                rts

                
;Expects pile no in X
;writes addr to 0203
;changes: A,Y
GetPileInfo     cpx #8
                bcc noaces_pile
                lda #0
                sta PileLength
                sta PileVisibleInd
                txa
                sec
                sbc #8
                tay
                lda #<Aces
                sta $02
                lda #>Aces
                sta $03
                jsr AddYto0203
                rts
noaces_pile     lda #<Piles
                sta $02
                lda #>Piles
                sta $03
                lda Mult19,x
                tay
                jsr AddYto0203
                lda PileLengths,x
                sta PileLength
                lda PileVisibleInds,x
                sta PileVisibleInd
                rts
                
;Expects card no in A
GetSuitAndValue ; If Aces,x still equals $ff
                cmp #$ff
                bne +
                sta Value
                ; Suit not important
                rts
+               pha
                lda #13
                sta modulo
                pla
                pha
                jsr Mod
                sta Value
                stx Suit
                pla
                rts

; Shuffles the deck
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
swap_ind        !byte 0,0

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


lname          !text "STATSSOL,P,R"
lname_end
sname           !text "@0:STATSSOL"
sname_end


LoadStats       LDA #lname_end-lname
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
                BCS eror    ; if carry set, a load error has happened
                RTS
eror            ; Accumulator contains BASIC error code
                ; most likely errors:
                ; A = $05 (DEVICE NOT PRESENT)
                ; A = $04 (FILE NOT FOUND)
                ; A = $1D (LOAD ERROR)
                ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
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
                BCS error    ; if carry set, a load error has happened
                jsr DisableSTOP
                RTS
error           ; Akkumulator contains BASIC error code
                jsr DisableSTOP
                RTS


UpdateTimes     lda Stats_WithTimer
                beq +
                ;if time < time0 then time2 = time1 : time1 = time0 : time0 = time
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
                
+               rts
                
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

PlaceCardSound  lda Stats_SoundOn
                beq no_sound
                lda #15
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
no_sound        rts

                

StartRattle     lda Stats_SoundOn
                beq no_sound
                lda #RattleDelay
                sta RattleCounter1
                lda #7
                sta RattleCounter2
                lda #1
                sta RattleSoundOn
                rts
!zone Data
bIsYes          !byte 0
bMouseInButton  !byte 0
bButtonPressing !byte 0
bMayFillEmAll   !byte 0
RattleSoundOn   !byte 0
RattleCounter1  !byte 0
RattleCounter2  !byte 0
Mult5           !byte 0,5,10,15,20,25,30,35,40
Offset          !byte 0; From which index to draw pile
UD_HasCards     !byte 0
OneTwoThreeLo   !byte 0,0,0 ;Map addresses of
OneTwoThreeHi   !byte 0,0,0 ;One, Two, and Three
; To save on disk -----------
Stats_Time0     !byte 0,10 ;best, format: sec, min
Stats_Time1     !byte 0,20 ;2nd best
Stats_Time2     !byte 0,30 ;3rd best
Stats_Wins      !byte 0,0
Stats_Lost      !byte 0,0
Stats_WithTimer !byte 1; If 0, timer is not shown and time not considered
Stats_CardColor !byte 9;14; color code for card color
Stats_Pattern   !byte 0; [0,1, or 2]
Stats_SoundOn   !byte 1
;----------------------------
Seconds         !byte 0
Minutes         !byte 0
DigitLo         !byte 0; For conversion
DigitHi         !byte 0; byte to decimal
WonFlag         !byte 0
SelMenuIndex    !byte 255
LastAction      !byte 0,0,0,0;For Undo: pileFrom, pileTo, no of cards, pileTo length
UpperCard       !byte 0
MapColor        !byte 0
PileFrom        !byte 0
PileTo          !byte 0
PileIndexFrom   !byte 0
bSelected       !byte 0
SelPileInd      !byte 0
PileLength      !byte 0
Mult19          !byte 0,19,38,57,76,95,114,133
PileVisibleInd  !byte 0
xPos            !byte 0
yPos            !byte 0
xPosDB          !byte 0
yPosDB          !byte 0
dummy           !byte 0
ValueToChar     !byte 65,50,51,52,53,54,55,56,57,88,74,81,75;<-----------------------
SuitToChar      !byte 46,47,45,44
Suit            !byte 0;0,1,2,3
SuitCmp         !byte 0
Value           !byte 0;0-12
ValueCmp        !byte 0
Piles           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile1           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile2           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile3           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile4           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile5           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile6           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Pile7           !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Aces            !byte 255,255,255,255
PileLengths     !byte 0,0,0,0,0,0,0,0,0,0,0,0
PileVisibleInds !byte 0,1,2,3,4,5,6,24
DummyPile       !byte 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
Deck            !byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
                !byte 20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35
                !byte 36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51
TimerOn         !byte 1
Timer           !byte 60; counts down from 60 to 0
exit_code       !byte 0
; Exit codes
bMoved          !byte 0;
bLBtnPress      !byte 0;
bLBtnRelease    !byte 0;
bDblClk         !byte 0;
bRestart        !byte 0;
bWon            !byte 0;
bExit           !byte 0;
;------------------------------------
dx              !byte 0 ;joystick
dy              !byte 0 ;directions
res             !byte 0 ;return value for various functions
Rect            !byte 0,0,0,0
counter         !byte 0
GameMode        !byte GM_NORMAL; 0: normal, 1: menu, 255: dialog
MenuMode        !byte 0; 0: MM_GAME, 1: MM_HELP
DialogMode      !byte 0;0: exit dlg, 1: won dlg, 2: stats dlg, 3: too many cards dlg, 4: instructions dlg, 5: about
MapWidth        !byte 0
MapHeight       !byte 0
ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
                !byte $05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07
;ColorTabHi      !byte $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9
;                !byte $d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db
;To be filled
DBScrTabLo      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98
DBScrTabHi      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98
DBColTabLo      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98
DBColTabHi      !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98

Colors          !bin "attribs.bin"; per char, thus 256 bytes
MenuMap         !bin "MenuMap.bin"
HelpMenuMap     !bin "HelpMenuMap.bin"
FullCardMap     !bin "FullCardMap.bin"
CoverCardMap    !bin "CoverCardMap.bin"
GameMenuMap     !bin "GameMenuMap.bin"
DBScrMap        !bin "GreenStripMap.bin"
DBColMap        !bin "GreenStripMap.bin"
Won_DlgMap      !bin "WonDlgMap.bin"
Instr_DlgMap    !bin "InstrDlgMap.bin"
About_DlgMap    !bin "AboutDlgMap.bin"
RUSure_DlgMap   !bin "AreYouSureDlgMap.bin"
Stats_DlgMap    !bin "StatsDlgMap.bin"
AcesMap         !bin "AcesMap.bin"
UDMap           !bin "UDMap.bin"
One             !bin "One.bin"
Two             !bin "Two.bin"
Three           !bin "Three.bin"
OptionsDlgMap   !bin "OptionsDlgMap.bin"
ButtonMap       !bin "ButtonMap.bin"
ButtonPrMap     !bin "ButtonPrMap.bin"
