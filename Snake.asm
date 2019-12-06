; Erik Lipsky

; Description: A simple snake game
; Compiler: http://www.pixelatedrobot.com/6502js/


define lo $01                 ; backdrop lo 
define hi $02                 ; backdrop hi 

define wall $03               ; wall color
define backdrop $0c           ; backdrop color

define appleLo $06            ; apple lo byte
define appleHi $07            ; apple hi byte
define appleColor $08         ; apple color

define snakeLength $09        ; $00 breaks snake
define snakeColor $05         ; color of snake
define snakeHeadL $10         ; low byte for head
define snakeHeadH $11         ; hi byte for head
define snakeBodyL $12         ; lo byte for body
define snakeBodyH $13         ; hi byte for body

define ASCII_a $61            ; left
define ASCII_d $64            ; right
define ASCII_s $73            ; down
define ASCII_w $77            ; up

define currentDirection $04   ; direction of snake
define randomByte $FE         ; Random Byte
define lastKeyPressed $FF     ; user input

init:
  JSR background    ; background
  JSR walls         ; perimeter of wall
  JSR trogdor       ; snake
  JSR crabApple     ; may change to a mouse
  JMP loop          ; jump to main game loop
   
;=========  
background:  ; draw background in backdrop color 
;=========

  LDA #$00        ; start at $0200
  STA lo
  LDA #$02        ; left to right, top to bottom
  STA hi 
  
draw:
  LDA #backdrop   ; load backdrop pixel
  STA (lo), Y     ; draw the color
 
incLo:
  LDA lo          ; load lo byte
  CLC             ; clear carry flag
  ADC #$01        ; add #$01
  BCS incHi       ; branch if carry flag set
  STA lo          ; store new lo byte
  JMP draw        ; JMP because I start at $0200
 
incHi:
  LDA hi          ; load hi byte
  CLC             ; always clear before adding
  ADC #$01        ; add #$01
  CMP #$06        ; compare it to #$06
  BEQ endLoop     ; RTS if = #$06
  STA hi          ; else, store new hi byte 
  LDA #$00        ; load left side of screen
  STA lo          ; store new lo byte
  JMP draw        ; Not RTS

endLoop:
  RTS             ; because you can't BEQ RTS
  
;=====  
 walls:  ; draw perimeter around screen
;=====

  JSR drawTop
  JSR drawRight
  JSR drawBottom
  JSR drawLeft
  RTS

drawTop:       
  
  LDA #$00        ; start at $0200 
  STA lo
  LDA #$02        ; go left to right
  STA hi
  
drawT:  
  LDA #wall       ; load wall color
  STA (lo), Y     ; draw wall pixel 
  
lowT:
  LDA lo          ; load lo byte
  CLC             ; clear carry flag
  ADC #$01        ; add #$01
  CMP #$20        ; compare it to right most pixel
  BEQ endLoop     ; done writing top
  STA lo          ; else, store it as lo byte
  JMP drawT       ; JMP if still drawing
  
drawRight:
  
  LDA #$1F        ; start at $021F 
  STA lo
  LDA #$02        ; go top to bottom
  STA hi 
  
drawR:  
  LDA #wall       ; load wall color
  STA (lo), Y     ; draw wall pixel 
  
lowR:
  LDA lo          ; load lo byte
  CLC             ; always clc before adding
  ADC #$20        ; add $20 to go down one pixel
  BCS incHiRight  ; if carry flag set, hi = x + 1
  STA lo          ; else, store new lo byte
  JMP drawR       ; not rts
  
incHiRight:
  LDA hi          ; load hi byte
  CLC             ; clear carry flag
  ADC #$01        ; add #$01
  CMP #$06        ; compare it to bottom of screen
  BEQ endLoop     ; end if successful
  STA hi          ; else, store new hi byte
  LDA #$1F        ; restart lo byte
  STA lo          ; store new lo byte
  JMP drawR       ; jmp until finished drawing

drawBottom:  
  
  LDA #$FF        ; start at $05FF 
  STA lo
  LDA #$05        ; go right to left
  STA hi
  
drawB:  
  LDA #wall       ; load wall color
  STA (lo), Y     ; store wall pixel
  
lowB:
  LDA lo          ; load lo byte
  SEC             ; set carry flag
  SBC #$01        ; go left one pixel
  CMP #$DF        ; compare it to bottom left pixel
  BEQ endLoop     ; if equal, rts
  STA lo          ; store new lo byte
  JMP drawB       ; jmp to 'draw bottom'
  
drawLeft:
  
  LDA #$00        ; start at $0200 go top to bottom
  STA lo
  LDA #$02        ; I couldnt get bottom to top
  STA hi          ; to work
  
drawL:  
  LDA #wall       ; load wall color
  STA (lo), Y     ; draw wall pixel
  
lowL:
  LDA lo          ; here is what i did
  CLC             ; SEC
  ADC #$20        ; SBC #$20
  BCS incHiLeft   ; BCC incHiLeft
  STA lo          ; store new lo byte
  JMP drawL       ; jmp to 'draw left'
  
incHiLeft:
  LDA hi          ; load hi byte
  CLC             ; sec
  ADC #$01        ; sbc #$01
  CMP #$06        ; compare to #$01
  BEQ endLoop     ; maybe I put #$02 instead of 
  STA hi          ; #$01
  LDA #$00        ; maybe I will change it later
  STA lo          ; store new lo byte
  JMP drawL       ; restart loop
 
endLoop: 
  RTS             ; end loop if complete
  
;======
trogdor:  ; nickname for snake #homestarrunner
;======

  LDA #ASCII_s 
  STA lastKeyPressed
  
  LDA #$2F                    ; initialize head
  STA snakeHeadL              ; to $02Ff
  LDA #$02                    ; in the middle of
  STA snakeHeadH              ; the screen
  
  LDA #$2F                    ; initalize body
  STA snakeBodyL              ; to $022F
  LDA #$02                    ; in the middle of
  STA snakeBody               ; the screen
    
  LDX #$04                    ; initialize body 
  STX snakeLength             ; to 2 bytes
  
  RTS
  
;========
crabApple:  ; Draw the initial apple.
;========
  
initApple:
  LDA #$03          ; load $03CE
  STA appleHi       ; store at hi byte
  LDA #$CE          ; load lo byte 
  STA appleLo       ; store at lo bytes

updateAppleHi:
  LDA randomByte    ; load a random byte
  AND #$03          ; and it with 3 to get 
  CLC               ; 00, 01, 02, 03
  ADC #$02          ; 02, 03, 04, 05
  STA appleHi       ; new hi byte is now valid
  
updateAppleLo:
  LDA randomByte    ; load a random byte
  STA appleLo       ; we aren't restricted here
    
drawInitialApple: 
  LDA #$7            ; draw yellow
  STA (appleLo), Y   ; store at head

RTS

;================================================
;Main game loop
;================================================
 
loop:  
  JSR readKeys  
  JSR checkWallCollision
  JSR checkAppleCollision
  JSR updateSnake
  JSR checkSnakeCollision
  JSR drawSnake
  JSR drawApple  
  JSR sleep 
  JMP loop 
  JMP gameOver
 
;================================================
;End of main game loop
;================================================
  
;=======
readKeys:  ; take in user input and then branch
;=======

LDA lastKeyPressed      ; user input (w, a, s, d)
STA currentDirection    ; store this direction
 
CMP #ASCII_w            ; compare to up direction
BEQ upDir               ; Add #$20 to go up

CMP #ASCII_a            ; compare to left direction
BEQ leftDir             ; Minus #$01 to go left

CMP #ASCII_s            ; compare to down direction
BEQ downDir             ; Minus #$20 to go down

CMP #ASCII_d            ; compare to right side
BEQ rightDir            ; Add #$01 to go right
   
RTS 

upDir:
  LDA snakeHeadL        ; load snake head lo
  SEC                   ; set carry flag
  SBC #$20              ; subtract #$20
  STA snakeHeadL        ; store new snake head lo
  BCC continueUp        ; branch if carry clear
  RTS     

continueUp:
  LDA snakeHeadH        ; load snake head hi
  SEC                   ; set carry flag
  SBC #$01              ; subtract #$01
  STA snakeHeadH        ; store new snake head hi
  RTS  
   
leftDir:
  LDA snakeHeadL        ; load snake head lo
  SEC                   ; set carry flag
  SBC #$01              ; subtract #$01
  STA snakeHeadL        ; store new snake head lo
  RTS

downDir:
  LDA snakeHeadL        ; load snake head lo
  CLC                   ; clear carry flag
  ADC #$20              ; add #$20
  STA snakeHeadL        ; store new snake head lo
  BCS continueDown      ; branch if carry set
  RTS

continueDown:
  LDA snakeHeadH        ; load snake head hi
  CLC                   ; clear carry flag
  ADC #$01              ; add #$01
  STA snakeHeadH        ; store new snake head hi
  RTS

rightDir:
  LDA snakeHeadL        ; load snake head lo
  CLC                   ; clear carry flag
  ADC #$01              ; add #$01
  STA snakeHeadL        ; store new snake head lo
  RTS
  
;=============
checkWallCollision:  ; check collision for trogdor
;=============
  
  LDA currentDirection    ; load current direction
   
  CMP #ASCII_w            ; compare to 'w'
  BEQ checkUp             ; branch to check up
  
  CMP #ASCII_a            ; compare to 'a'
  BEQ checkLeft           ; branch to check left
  
  CMP #ASCII_s            ; compare to 's'
  BEQ checkDown           ; branch to check down
 
  CMP #ASCII_d            ; compare to 'd'
  BEQ checkRight          ; branch to check right
  
  RTS 
   
checkUp:
  LDA snakeHeadH         ; check if snake went
  CMP #$01               ; outside of $0200
  BEQ gameOver
  RTS 
    
checkLeft:
  LDA snakeHeadL         ; load snake head lo
  AND #$1F               ; compare all cases of xF
  CMP #$1F               ; from $1F + $20
  BEQ checkFutherLeft    ; branch to check hi byte
  RTS
  
checkFutherLeft:
  LDA currentDirection   ; load current way
  CMP #ASCII_a           ; compare it to left
  BEQ gameOver
  RTS
    
checkDown:
  LDA snakeHeadH          ; check if snake went
  CMP #$06                ; outside of $05FF
  BEQ gameOver
  RTS 
      
checkRight:
  LDA snakeHeadL          ; load snake head lo
  AND #$1F                ; all cases of 
  CMP #$1F                ; compare all cases of xFxF          
  BEQ checkFurtherRight   ; branch to check hi byte
  RTS
   
checkFurtherRight:
  LDA currentDirection    ; load current way
  CMP #ASCII_d            ; If direction is right
  BEQ gameOver
  RTS
    
;==================
checkAppleCollision: 
;==================
    
checkApplePosition:
  LDA snakeHeadL         ; load snake head lo
  CMP appleLo            ; compare it to apple lo
  BEQ checkSnakeHeadHi   ; branch to hi byte
  RTS 
    
checkSnakeHeadHi:
  LDA snakeHeadH         ; load in snake head hi
  CMP appleHi            ; compare it to apple hi
  BEQ eatApple           ; success!
  RTS 

eatApple:
  LDX snakeLength        ; load in snake  length
  INX                    ; increment the lenght
  INX                    ; two times
  STX snakeLength        ; store new snake length
  JMP drawAppleHi        ; RTS doesn't work

drawAppleHi:
  LDA randomByte         ; load random byte
  AND #$03               ; mask random byte
  CLC                    ; prep work
  ADC #$02               ; valid hi byte for apple
  STA appleHi            ; store apple hi byte
  JMP drawAppleLo        ; RTS works now

drawAppleLo:
  LDA randomByte         ; load random byte
  STA appleLo            ; no restrictions
  RTS

;==================
checkSnakeCollision: ; 
;==================

  LDX snakeLength        ; load snake length 
  LDA (snakeBodyL, x)    ; load snake body lo
  CMP #snakeColor        ; compare it to green
  BEQ gameOver           ; game over if it is.
  RTS
 
;========== 
updateSnake:  ; shift values down the queue line
;==========
        
  LDX snakeLength         ; load snake address
  ;DEX                    ; added in office hours
          
shiftValues:  
  LDA snakeHeadH, X       ; Do high byte before 
  STA snakeBodyH, X       ; doing lo byte
   
  LDA snakeHeadL, X       ; load in head
  STA snakeBodyL, X       ; store two bytes away 
  DEX
     
  BPL shiftValues         ; Branch if PLus
                          
;======== 
drawSnake:  ; draws the snake based on updated
;========   
    
  LDX snakeLength       ; load snake length
      
  LDA #backdrop         ; Draw tail consistent to
  STA (snakeBodyL, X)   ; background 
     
  LDX #$00              ; restart counter
  
  LDA #$05              ; draw green
  STA (snakeHeadL, X)   ; store at head
    
  RTS  

;======== 
drawApple:  ; This function didn't do what I wanted
;========    
   
  LDA #appleColor    ; Oh well, it works 
  
  CMP #$8            ; was trying to make the 
  BEQ drawRed        ; apple flash 3 colors
  
  CMP #$10           ; red/green/yellow
  BEQ drawGreen      ; instead i got 10 colors
  
  CMP #$13           ; light green
  BEQ drawOrange     ; draw yellow
  
  RTS
  
drawRed:
  LDA appleColor     ; load apple color
  CLC                ; clear carry as usual
  ADC #$02           ; add 2 to go to next color
  STA appleColor     ; store 10 (8+2=10) 
  STA (appleLo), Y   ; store apple byte
  RTS

drawGreen:
  LDA appleColor     ; load apple color
  CLC                ; clear carry as usual
  ADC #$03           ; add 2 to go to next color
  STA appleColor     ; store 13 (10+3=13) 
  STA (appleLo), Y   ; store apple byte
  RTS

drawOrange:
  LDA appleColor     ; load apple color
  SEC                ; clear carry as usual
  SBC #$05           ; go back to first color
  STA appleColor     ; store 7 (13-5=8)
  STA (appleLo), Y   ; store apple byte
  RTS

RTS
  
;====
sleep:  ; slows down the game 
;====

LDX #$FF          ; start at $FF
 
REM:
  NOP             ; do nothing
  NOP             ; do nothing
  DEX             ; decrement (now $FE)
  BNE REM         ; repeat loop
  RTS             ; exit when loop = 0

;======= 
gameOver:  ; added death pixel. ^_^      
;=======
  
  LDA currentDirection  ; load current direction
  
  CMP #ASCII_d          ; if last key pressed is 'd'
  BEQ snakeHorizon      ; collision pixel on right
  
  CMP #ASCII_a          ; horizontal check doesn't
  BEQ snakeVertistop    ; work properly
  
  CMP #ASCII_w          ; if last key pressed is 'w'
  BEQ snakeVertistop    ; collision pixel
  
  CMP #ASCII_s          ; if last key pressed is 's'
  BEQ snakeVertistop    ; collision pixel
  
  snakeHorizon:
  LDA #$02              ; draw red and kill snake
  STA (snakeHeadL, X)   ; left and right collision
  BRK                   ; end
  
  snakeVertistop:
  LDA #$02              ; draw red and kill snake
  STA (snakeBodyL, X)   ; up and down collision
  BRK                   ; end