.model small
.stack 100h
.data 
mult        DW  25173
incr        DW  13849          
SEED        DW  ?
uLimit      DB  13
lLimit      DB  1
welcome     DB  'Welcome to Blackjack$' 
DHandMsg    DB  'Dealers Hand$'
PHandMSg    DB  'Your Hand$'
round       DB  0
dealerHand  db  5 dup (0)
playerHand  db  5 dup (0)
dealerSize  db  0
playerSize  db  0 
dealerRow   db  5
dealerCol   db  1
currDCard   dW  0
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
endm  

restoreRegs MACRO
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

dispCard    MACRO   cardNum, Drow, Dcol   
    saveRegs
    MOV     AH, 09H
    MOV     AL, cardNum
    MOV     BX, 2
    ADD     AL,'0'
    MOV     CX, 1
    INT     10H
    restoreRegs
endm

start:    
    MOV     AX, @data
    MOV     DS, AX
    MOV     ES, AX
    PUSH    BP
    MOV     bp,sp  
    LEA     BP, welcome
    printStr   1,1     
    CALL    createSeed
    CALL    dealDealer
    CALL    dealDealer
    CALL    dispDHand
    CALL    dealDealer
    CALL    dealDealer
    CALL    dealPlayer 
    CALL    dispDHand
    MOV     AX, 4C00H
    INT     21H 

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
    
    INC     dealerSize
    RET
dealPlayer    ENDP
    

dispDHand     PROC
    LEA     BP, DHandMsg
    printStr   3,1   
    MOV     BL, dealerSize
    LEA     DX, dealerHand
    ADD     DX, currDCard
    MOV     SI, DX 
    SUB     BX, currDCard 
    dh1:
      MOV       AL, [SI] 
      setCursor dealerRow, dealerCol
      dispCard  AL, dealerRow, dealerCol 
      INC       currDCard
      INC       dealerCol
      INC       SI
      DEC       BL
      CMP       BL, 1 
      setCursor   4,2
      JNE       dh1
             
    
    RET
dispDHand     ENDP 

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

