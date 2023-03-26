.model small
.stack 100h
.data 
mult        DW  25173
incr        DW  13849          
SEED        DW  ?
uLimit      DB  12
lLimit      DB  1
welcome     DB  'Welcome to Blackjack$' 
DHandMsg    DB  'Dealers Hand$'
PHandMSg    DB  'Your Hand$'
PTotalMSg    DB  'Your Total:$'
round       DB  0
dealerHand  db  5 dup (0)
playerHand  db  5 dup (0)
dealerSize  db  0
playerSize  db  0 
dealerRow   db  4
dealerCol   db  1
playerRow   db  7
playerCol   db  1
currDCard   dW  0
currPCard   dW  0 
deck        db  65,50,51,52,53,54,55,56,56,74,81,75,42
pScore db  0
.code               

printStr  MACRO row, column
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
    printStr   1,1
    LEA     BP, DHandMsg
    printStr   3,1              
    LEA     BP, PHandMsg
    printStr   6,1 
    LEA     BP, PTotalMsg
    printStr   14,15
;Initialise the game
    CALL    createSeed
    CALL    dealDealer
    CALL    dealDealer
    CALL    dealPlayer
    CALL    dealPlayer
    CALL    dispDHand 
    CALL    dispPHand
    CALL    checkP21
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
          cmp       currDCard, 1
          je        dispMask
          dispCard  AL 
          
          updateDvals:   
              INC       currDCard
              INC       dealerCol
              INC       SI
              DEC       BL
              JMP       dh1
        
        dispMask:
           MOV      AL, 13
           dispCard AL, dealerRow, dealerCol
           jmp      updateDvals                              
   
    exdispDHand:    
        RET
dispDHand     ENDP 

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
          dispCard  AL
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
   MOV      BX, 00  ;TOTAL VALUE
   MOV      CX, 00  ;TOTAL NUMBER OF ACES
   LEA      DX, playerHand
   MOV      SI, DX
   ch1:
    MOV     AX, [SI]
    INC     SI
    CMP     AX, 0   
    JE      calcP
    CMP     AX, 1   ;check if ace. Value of ACE's will be calculated at the end
    JE      ACE1
    
    ADD     BL, AL  ;add value of non-ace card to the total
    JMP     ch1
   
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

