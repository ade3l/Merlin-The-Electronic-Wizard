.model small
.stack 100h
.data 
mult    DW  25173
incr    DW  13849          
SEED    DW  ?
uLimit  DB  13
lLimit  DB  1
msg DB 'Welcome to Blackjack$'
.code  

printStr  MACRO row, column
    ;calculate length of the string    
    MOV     CX, 00      ;length will be stroed in cx
    MOV     SI, BP
    CALL    strLen  
    ;print the string
    MOV     AX, 1301H 
    MOV     BH, 0
    MOV     BL, 02H; color
    DEC     CX       ;the calculated length includes the $ symbol. So decrement length by 1
    MOV     DH, row
    MOV     DL, column  
    INT     10H 
endm      

start:    
    MOV     AX, @data
    MOV     DS, AX
    MOV     ES, AX
    PUSH    BP
    MOV     bp,sp  
    LEA     BP, msg
    printStr   1,1     
    
    MOV     AX, 4C00H
    INT     21H 


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

