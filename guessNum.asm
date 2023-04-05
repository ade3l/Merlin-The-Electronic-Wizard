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
selected_msg db "Selected level: "
selected_msg_len equ    $-selected_msg
l1_msg db "Mystery number is between 10 - 99" 
l1_msg_len dw 33
l2_msg db "Mystery number is between 100 - 999"
l2_msg_len dw 35
l3_msg db "Mystery number is between 1000 - 9999" 
l3_msg_len dw 37
secLen dw ? 
diff db ?
secret dw ?
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
CLRSCR MACRO
    ; Macro to clear the screen
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
    CLRSCR 
    ;Show selected level---
    MOV BP,OFFSET selected_msg 
    printStr    1, 1, selected_msg_len
    setCursor   1, selected_msg_len+1 
    MOV AH, 09H
    MOV AL, diff
    ADD  AL, 30H
    MOV BL, 02H
    MOV CX, 1
    INT 10H 
    
    ;Show range 
    call showRange
    call createSecret
    MOV BP, OFFSET SECRET
    PRINTSTR 3, 1, SEClEN
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

createSecret    proc
    saveRegs
    cmp diff, 1
    je  createSecret1
    cmp diff, 2
    je  createSecret2
    MOV lLimit, 1000
    MOV uLimit, 9999
    mov secLen, 4 
    jmp getRand
    createSecret1:
        MOV lLimit, 10
        MOV uLimit, 99 
        mov secLen, 2
        jmp getRand
        
    createSecret2:
        MOV lLimit, 100
        MOV uLimit, 999
        mov secLen, 3
    getRand:
        CALL genRand 
   
        MOV DI, OFFSET secret ; Pointer to the target string
        MOV BX, 10        ; Divisor to extract the rightmost digit
        MOV AX, DX        ; Copy the number to AX
    
        ExtractDigit:
            XOR DX, DX        ; Clear DX to prepare for division
            DIV BX            ; Divide by 10 to get the remainder in DX
            ADD DL, '0'       ; Convert the remainder to ASCII code
            MOV [DI], DL      ; Store the digit in the target string
            INC DI            ; Increment the string pointer
            TEST AX, AX       ; Check if quotient is zero
            JNZ ExtractDigit  ; If not, repeat
        
            
        
            ; Reverse the string in place
            MOV SI, OFFSET secret
            DEC DI            ; Point to the last digit in the string
        ReverseLoop:
            CMP SI, DI        ; Check if pointers have crossed
            JGE Done          ; If so, we're done
            MOV AL, [SI]      ; Swap characters
            MOV DL, [DI]
            MOV [SI], DL
            MOV [DI], AL
            INC SI            ; Move pointers toward each other
            DEC DI
            JMP ReverseLoop
            ; The reversed string is now in 'secret'
    Done:        
    restoreRegs
    ret
createSecret endp 

showRange   proc 
    cmp diff, 1
    je  range1 
    cmp diff, 2
    je  range2
    
    MOV BP,OFFSET l3_msg 
    printStr    2, 1, l3_msg_len
    ret
    
    range1:
        MOV BP,OFFSET l1_msg 
        printStr    2, 1, l1_msg_len
        ret 
    range2:
        MOV BP,OFFSET l2_msg 
        printStr    2, 1, l2_msg_len
        ret         
    
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
