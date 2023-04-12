.model small
.stack 100h
.data 
mult        DW  25173
incr        DW  13849          
SEED        DW  ?
uLimit      DB  12
lLimit      DB  1
welcome     DB  'Welcome to Blackjack$' 
DHandMsg    DB  'Dealer Hand$'
PHandMSg    DB  'Your Hand$'
PTotalMSg    DB  'Your Total:$' 
DTotalMSg    DB  'Dealer Total:$'
msgPbust    db "Bust! You lose!$"
msgPwin     db "Blackjack! You win!$"
msgAskHitOrStay db 'Do you want to hit or stay? (h/s): $'
msgInvalidInput db 'Invalid input. Please enter "h" for hit or "s" for stay: $'
msgPlayerHit db 'You chose to hit.$'
msgPlayerStay db 'You chose to stay.$'
msgPlayerWin DB 'YOU WON!$'
msgDealerWin DB 'DELAER WON. YOU LOST!$'
;msgPcontinue db "Your score is less than 21. Continue playing.$", 0
msgClear    db "                                    $"    
round       DB  0
dealerHand  db  10 dup (0)
playerHand  db  10 dup (0)
dealerSize  db  0
playerSize  db  0 
dealerRow   db  3
dealerCol   db  1
playerRow   db  16
playerCol   db  1
currDCard   dW  0
currPCard   dW  0 
deck        db  65,50,51,52,53,54,55,56,56,74,81,75,42
pScore db  ?
dScore db  ?
row db ?
column db ?
character db ?
.code               

printStr  MACRO row, column 
    saveRegs
    ;calculate length of the string    
    MOV     CX, 00      ;length will be stroed in cx
    MOV     SI, BP
    CALL    strLen  
    ;print the string
    MOV     AX, 1301H 
    MOV     BH, 0
    MOV     BL, 02H  ; color
    DEC     CX       ;the calculated length includes the $ symbol. So decrement length by 1
    MOV     DH, row
    MOV     DL, column  
    INT     10H
    restoreRegs 
endm  

saveRegs    MACRO
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX
    PUSH    SI
endm  

restoreRegs MACRO
    POP     SI
    POP     DX
    POP     CX
    POP     BX
    POP     AX
endm
setCursor   MACRO   row, column
    saveRegs
    MOV     AH, 02H
    MOV     DH, row 
    MOV     DL, column
    INT     10H
    restoreRegs
endm 

dispCard    MACRO   cardNum  
    saveRegs
    MOV     AH, 0 
    LEA     DX, deck
    MOV     AL, cardNum
    DEC     AL
    ADD     DX, AX
    MOV     SI, DX
    MOV     AL, [SI]

    MOV     AH, 09H
    MOV     BX, 2
    MOV     CX, 1
    INT     10H


    restoreRegs
endm
;---------------------------------------------------------------------------------------
start:    
    MOV     AX, @data
    MOV     DS, AX
    MOV     ES, AX
    PUSH    BP
    MOV     bp,sp
      
    LEA     BP, welcome
    printStr   0,25
    LEA     BP, DHandMsg
    printStr   2,1              
    LEA     BP, PHandMsg
    printStr   14,1 
    LEA     BP, PTotalMsg
    printStr   14,15
;Initialise the game
    CALL    createSeed
    CALL    dealDealer
    CALL    dealPlayer
    CALL    dealDealer
    CALL    dealPlayer
    CALL    dispDHand 
    CALL    dispPHand
    CALL    checkP21
    CALL    askHitOrStay
    MOV     AX, 4C00H
    INT     21H 
;---------------------------------------------------------------------------------------
dealDealer    PROC    
    ;dealerHand
    CALL    genRand 
    PUSH    DX  
    MOV     AL, dealerSize 
    MOV     AH, 0
    LEA     DX, dealerHand
    ADD     DX,AX
    MOV     SI, DX
    POP     DX       
    MOV     [SI], DL
    
    INC     dealerSize
    RET
dealDealer    ENDP

dealPlayer    PROC    
    ;playerHand
    CALL    genRand 
    PUSH    DX  
    MOV     AL, playerSize 
    MOV     AH, 0
    LEA     DX, playerHand
    ADD     DX, AX
    MOV     SI, DX
    POP     DX
    MOV     [SI], DL
    
    INC     playerSize
    RET
dealPlayer    ENDP
    
dispCardNew MACRO r, char, currCard
    saveRegs                  
    MOV     AH, 0 
    LEA     DX, deck
    DEC     AL
    ADD     DX, AX
    MOV     SI, DX
    MOV     AL, [SI]
    mov character, al
    xor dx, dx
    mov ax, currCard
    mov bx, 11
    ;int 3h
    mul bx
    mov row, r
    mov column, al
    
    call printCard
    restoreRegs
ENDM

dispDHand     PROC     
    dispNextDcard:   
        MOV     BL, dealerSize
        LEA     DX, dealerHand
        ADD     DX, currDCard
        MOV     SI, DX 
        SUB     BX, currDCard 
        dh1:
          CMP       BL, 0
          JE        exdispDHand 
          MOV       AL, [SI] 
          setCursor dealerRow, dealerCol
          cmp       currDCard, 1, 
          je        dispMask
          dispCardNew  3, AL,currDCard
          
          updateDvals:   
              INC       currDCard
              INC       dealerCol
              INC       SI
              DEC       BL
              JMP       dh1
        
        dispMask:
           MOV      AL, 13
           dispCardNew 3, AL, 1
           jmp      updateDvals                              
   
    exdispDHand:    
        RET
dispDHand     ENDP 
printCard PROC 
    push ax
    push bx
    push cx
    push dx
    ;top border
    mov ah, 02h
    mov dh, row
    mov dl, column
    inc dl
    int 10h
    mov ah, 09h
    mov al, '-'
    mov bl, 02
    mov cx, 8
    int 10h 
    ;left border  
    mov cx, 8
    dec dl
    print_left_border: 
        inc dh
        mov ah, 02h
        int 10h 
        push cx
        mov ah, 09h
        mov al, '|'
        mov bl, 02 
        mov cx, 1
        int 10h 
        pop cx
        loop print_left_border
    ;bottom border 
    inc dh
    inc dl
    mov ah, 02h
    int 10h
    mov ah, 09h
    mov al, '-'
    mov bl, 02
    mov cx, 8
    int 10h 
    
    ;left border
    mov cx, 8
    add dl, 8 
    print_right_border:
        dec dh
        mov ah, 02h
        int 10h 
        push cx
        mov ah, 09h
        mov al, '|'
        mov bl, 02 
        mov cx, 1
        int 10h 
        pop cx
        loop print_right_border
    mov dh, row
    mov dl, column
    add dh, 1
    add dl, 1
    mov ah, 02
    int 10h
    mov ah, 09h
    mov al, character
    mov bl, 2
    mov cx, 1
    int 10h 
    
    add dh, 7
    add dl, 7
    mov ah, 02
    int 10h
    mov ah, 09h
    mov al, character
    mov bl, 2
    mov cx, 1
    int 10h 
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp
dispPHand   PROC
    MOV     BL, playerSize
    LEA     DX, playerHand
    ADD     DX, currPCard
    MOV     SI, DX 
    SUB     BX, currPCard
    dh2:
          CMP       BL, 0
          JE        exdispPHand 
          MOV       AL, [SI] 
          setCursor playerRow, playerCol
          dispCardNew   15,AL, currPCard
          INC       currPCard
          INC       playerCol
          INC       SI
          DEC       BL
          JMP       dh2 
    exdispPHand:    
        RET                  

dispPHand   ENDP

checkP21     PROC
   saveRegs
   LEA      BP, msgClear
   printstr 25,10
   printstr 26,10
   MOV      BX, 00  ;TOTAL VALUE
   MOV      CX, 00  ;TOTAL NUMBER OF ACES
   LEA      DX, playerHand
   MOV      SI, DX
   ch1:
    MOV     AX, [SI]
    XOR      AH, AH
    INC     SI
    CMP     AX, 0   
    JE      calcP
    CMP     AX, 1   ;check if ace. Value of ACE's will be calculated at the end
    JE      ACE1
    CMP     AX, 10  ;check if face card
    JAE     FACE1   ;add 10 if it is
    ADD     BL, AL  ;add value of non-ace card to the total
    JMP     ch1
   FACE1:
    ADD     BL, 10
    JMP     CH1
   ACE1:
    INC     CL
    JMP     ch1
    
   addAce1:
    ADD     BL, 11
    DEC     CL
     
   calcP:
    CMP     CL, 0   ;check if there are any aces. if not then leave
    JE      exChp21
    CMP     BL, 10  ;check if current total is <= 10
    JLE     addAce1  ;if it is then add 11 for that ACE's values
    ch2: 
        ADD     BL, 1 ;otherwise add 1 for it's value
        LOOP    ch2    
    
   exChp21:
    MOV     pScore, BL
    CALL    updatePScore
    CALL    checkPScore    
    restoreRegs
    RET
checkP21     ENDP

updatePScore     PROC
    saveRegs
    XOR     CX, CX
     
    MOV AL, pScore
    ; Convert the hex value to decimal and store the digits in the stack
    MOV AH, 0         ; Initialize the quotient to 0
    MOV CX, 10        ; Initialize the divisor to 10 
    DIV_LOOP:
        XOR DX, DX        ; Clear the DX register
        DIV CX            ; Divide the quotient by the divisor
        ADD DL, '0'       ; Convert the remainder to ASCII
        PUSH DX           ; Save the digit on the stack
        CMP AL, 0         ; Check if the quotient is 0
        JNE DIV_LOOP      ; If not, continue dividing
    CMP pScore, 9
    JA  PRINT
    MOV AX, 30H
    PUSH    AX
    PRINT:
        setCursor   14, 28
        ; Pop each digit from the stack and print it to the screen
        POP AX
        MOV AH, 09H ; Set up the function code for printing a character
        MOV CX, 1
        MOV BL, 2
        INT 10H     ; Call the BIOS to print the character
        setCursor   14, 29
        ; Pop each digit from the stack and print it to the screen
        POP AX
        MOV AH, 09H ; Set up the function code for printing a character
        MOV CX, 1
        MOV BL, 2
        INT 10H     ; Call the BIOS to print the character
    restoreRegs
    RET  
updatePScore    ENDP

checkPScore PROC
    saveRegs
    CMP pScore, 21
    JA pBust

    ; Check if Pscore is equal to 21
    CMP pScore, 21
    JE pWin

    ; If Pscore is less than 21, continue the game
    JMP pContinue
    
    pBust:
        ; Player has bust and lost the game
        LEA         BP, msgPbust
        printstr    25, 10
        CALL        endGame
    pWin:
        LEA         BP, msgPwin
        printstr    25, 10
        CALL        endGame     
    
    pContinue:
        restoreRegs

    RET
checkPScore ENDP

askHitOrStay PROC
    saveRegs
    ; Display message to ask player to hit or stay
    LEA BP, msgAskHitOrStay
    printstr 25, 10
    
    ; Wait for player input (h for hit, s for stay)
    CALL    KEYPRESS
    
    ; Check if player chose to hit or stay
    cmp al, 'h'
    je hit
    cmp al, 's'
    je stay
    
    ; Invalid input, ask again
    LEA BP, msgInvalidInput
    printstr 25, 10
    JMP askHitOrStay
    
    hit:
        ; Player chose to hit
        LEA BP, msgPlayerHit
        printstr 26, 10
        JMP hitP
        restoreRegs
        RET
        
    stay:
        ; Player chose to stay
        LEA BP, msgPlayerStay
        printstr 26, 10
        JMP stayP
        restoreRegs
        RET
        
askHitOrStay ENDP
hitP     PROC
    CALL    dealPlayer
    CALL    dispPHand
    CALL    checkP21
    CALL    askHitOrStay
    RET
hitP     ENDP 

STAYP    PROC 
    DEC dealerCol
    setCursor dealerRow, dealerCol 
    INC dealerCol
    MOV BL, 1
    LEA DX, dealerHand
    ADD DX, 1
    MOV SI, DX
    MOV AL, [SI]
    dispCardNew 3, AL, 1
    
    
    
    LEA      BP, msgClear
    printstr 25,10
    printstr 26,10 
    LEA     BP, DTotalMsg
    PRINTSTR    14, 35
    d17Check:
        CALL    CalcDTotal
        CALL    showDScore
        
        ; Check if dealer's total is less than 17
        CMP dScore, 17
        JL  DEALP
        
        ; Check if dealer has bust
        CMP dScore, 22
        JAE  PLAYER_WIN
        
        MOV AH, dScore
        MOV AL, pScore
        ; Check if dealer has higher hand than player
        CMP AH, AL
        JG  DEALER_WIN
        
        ; If none of the above conditions are true, player wins
        JMP PLAYER_WIN        


    DEALP:
        CALL dealDealer
        CALL dispDHand
        JMP  d17Check
    
    
    PLAYER_WIN:
        LEA BP, msgPlayerWin
        PRINTSTR 25, 10
        JMP END_GAME 
        
    DEALER_WIN:
        LEA BP, msgDealerWin
        PRINTSTR 25, 0
        JMP END_GAME
        
    END_GAME:
        MOV AX, 4C00H
        INT 21H
STAYP    ENDP

CalcDTotal     PROC
   saveRegs
   MOV      BX, 0  ;TOTAL VALUE
   MOV      CX, 0  ;TOTAL NUMBER OF ACES
   LEA      DX, dealerHand
   MOV      SI, DX
   ch3:
    MOV     AX, [SI]
    XOR     AH, AH
    INC     SI
    CMP     AX, 0   
    JE      calcD
    CMP     AX, 1   ;check if ace. Value of ACE's will be calculated at the end
    JE      ACE2
    CMP     AX, 10  ;check if face card
    JAE     FACE2   ;add 10 if it is
    ADD     BL, AL  ;add value of non-ace card to the total
    JMP     ch3
   FACE2:
    ADD     BL, 10
    JMP     CH3
   ACE2:
    INC     CX
    JMP     ch3
    
   addAce2:
    ADD     BL, 11
    DEC     CX
     
   calcD:
    CMP     CX, 0   ;check if there are any aces. if not then leave
    JE      exChd21
    CMP     BL, 10  ;check if current total is <= 10
    JLE     addAce2  ;if it is then add 11 for that ACE's values
    ch4: 
        ADD     BL, 1 ;otherwise add 1 for it's value
        LOOP    ch4    
    
   exChd21:
    MOV     dScore, BL
    
    ;CALL    checkDScore    
    restoreRegs
    RET
CalcDTotal     ENDP

showDScore  PROC
    saveRegs
    XOR     CX, CX
     
    MOV AL, dScore
    ; Convert the hex value to decimal and store the digits in the stack
    MOV AH, 0         ; Initialize the quotient to 0
    MOV CX, 10        ; Initialize the divisor to 10 
    DIV_LOOP2:
        XOR DX, DX        ; Clear the DX register
        DIV CX            ; Divide the quotient by the divisor
        ADD DL, '0'       ; Convert the remainder to ASCII
        PUSH DX           ; Save the digit on the stack
        CMP AL, 0         ; Check if the quotient is 0
        JNE DIV_LOOP2     ; If not, continue dividing
    CMP dScore, 9
    JA  PRINT2
    MOV AX, 30H
    PUSH    AX
    PRINT2:
        setCursor   14, 50
        ; Pop each digit from the stack and print it to the screen
        POP AX
        MOV AH, 09H ; Set up the function code for printing a character
        MOV CX, 1
        MOV BL, 2
        INT 10H     ; Call the BIOS to print the character
        setCursor   14, 51
        ; Pop each digit from the stack and print it to the screen
        POP AX
        MOV AH, 09H ; Set up the function code for printing a character
        MOV CX, 1
        MOV BL, 2
        INT 10H     ; Call the BIOS to print the character
    restoreRegs
    RET  
showDScore ENDP


endGame     PROC
    MOV     AX, 4C00H
    INT     21H
endGame     ENDP
    
strLen  PROC  
    INC     CX
    MOV     AX,[SI]
    INC     SI
    CMP     AL, '$'
    JNE     strLen
    RET
strLen  ENDP
     
keyPress PROC
        MOV     AH,08H            
        INT     21H
        CALL    toLower
        RET
keyPress ENDP   

toLower PROC
        CMP AL, 65    ;'A'
        JB  CONTINUE ;IF THE KEY IS LESS THAN 'A' IT DOES NOTHING
        CMP AL, 90    ;'Z'
        JA  CONTINUE ;IF THE KEY IS GREATER THAN 'Z' IT DOES NOTHING
        ADD AL, 32    ;Converts uppercase to lowercase.
     CONTINUE:
        RET
toLower ENDP

saveRegs    PROC
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX
    RET
saveRegs    ENDP

;Random number generator from https://github.com/ade3l/Pseudo-random-number-generator
genRand PROC 
    PUSH    AX
    PUSH    BX
    PUSH    CX
     
    ;calculate next value and make that the new seed
    MOV     AX, mult
    MOV     BX, SEED
    MUL     BX
    ADD     AX, incr 
    MOV     SEED, AX
    shr     ax, 4                                     
    
    MOV     BX, 00
    ;Reducing the number to in the required range
    MOV     BL, uLimit
    SUB     BL, lLimit
    
    MOV     DX,0
    DIV     BX
    ADD     DL, lLimit
    
    POP     CX
    POP     BX
    POP     AX 
    RET
genRand ENDP 

createSeed PROC
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX 
    
    ;We will use Low-order part of clock count as the seed
    MOV     AH, 00H
    INT     1AH
    MOV     SEED, DX
    
    POP     DX
    POP     CX
    POP     BX
    POP     AX 
    RET
createSeed ENDP
      
END

