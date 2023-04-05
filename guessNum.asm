.model small
.stack 100h
.data
mult    DW  25173
incr    DW  13849          
SEED    DW  ?
uLimit  DW  ?
lLimit  DW  1   
diff_msg dw "Select your difficulty level 1-3:" 
diff_msg_len equ $-diff_msg
diff db ?
.code 
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

printStr  MACRO row, column, length 
    saveRegs
    MOV     CX, length  
    ;print the string
    MOV     AX, 1301H 
    MOV     BH, 0
    MOV     BL, 02H  
    MOV     DH, row
    MOV     DL, column  
    INT     10H
    restoreRegs 
endm  

start:
    MOV     AX,@data
    MOV     DS,AX 
    MOV     ES,AX
    PUSH    BP
    MOV     BP,SP
    
    CALL    createSeed
    MOV BP,OFFSET diff_msg 
    printStr    10, 10, diff_msg_len
    call getDiff 
    MOV     AX, 4C00H
    INT     21H 

getDiff proc 
    saveRegs
    getDiffLoop:
        MOV     AH, 00H
        INT     16H
        CMP     AL, 33H
        JG      getDiffLoop
        CMP     AL, 31H
        JL      getDiffLoop
        SUB     AL, 30H
        MOV     diff, AL 
    restoreRegs
    RET
getDiff ENDP

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
    
    MOV     BX, uLimit
    SUB     BX, lLimit
    
    MOV     DX,0
    DIV     BX
    ADD     DX, lLimit
    
    POP     CX
    POP     BX
    POP     AX
    INT 3h 
    RET
genRand ENDP 

createSeed PROC
    PUSH    AX
    PUSH    BX
    PUSH    CX
    PUSH    DX 
    
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
