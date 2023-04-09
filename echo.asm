.model small
.stack 100h
.data 

mult    DW  25173
incr    DW  13849          
SEED    DW  ?
uLimit  DW  ?
lLimit  DW  1  
  
diff_msg dw "Select your difficulty level 1-9:" 
diff_msg_len equ $-diff_msg   

selected_msg db "Selected level: "
selected_msg_len equ    $-selected_msg 
diff    db ?
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
    MOV     BL, 2H  
    MOV     DH, row
    MOV     DL, column  
    INT     10H
    restoreRegs 
endm  
CLRSCR MACRO
    mov ax, 3
    int 10h
ENDM
setCursor   MACRO   row, column
    saveRegs
    MOV     AH, 02H
    MOV     DH, row 
    MOV     DL, column
    INT     10H
    restoreRegs
endm 
;----------------------------------------------------------------------------------------
start:
    MOV     AX,@data
    MOV     DS,AX 
    MOV     ES,AX

    PUSH    BP
    MOV     BP,SP 
    
    CALL    createSeed
    MOV BP,OFFSET diff_msg 
    printStr    1, 1, diff_msg_len
    call getDiff
MOV AX, 4C00H
INT 21H 
;---------------------------------------------------------------------------------------- 
getDiff proc 
    saveRegs
    getDiffLoop:
        MOV     AH, 00H
        INT     16H
        CMP     AL, 39H
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
    
    shr     ax, 3                                     
    
    MOV     BX, uLimit
    
    MOV     DX,0
    DIV     BX
    ADD     DX, lLimit
    
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
    
    MOV     AH, 00H
    INT     1AH
    MOV     SEED, DX
    
    POP     DX
    POP     CX
    POP     BX
    POP     AX 
    RET
createSeed ENDP
               
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
END


