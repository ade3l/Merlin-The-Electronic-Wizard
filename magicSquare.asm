.model small
.stack 100h
.data
mult        DW  25173
incr        DW  13849 
start_square db "000000000"
square db "000000000"
square_win db "111101111"
score_str db "SCORE"
steps db 1 
SEED        DW  ?
uLimit      DB  3
lLimit      DB  1
.code 

printStr  MACRO row, column, length 
    ;saveRegs
    MOV     CX, length  
    ;print the string
    MOV     AX, 1301H 
    MOV     BH, 0
    MOV     BL, 02H  ; color
    MOV     DH, row
    MOV     DL, column  
    INT     10H
    ;restoreRegs 
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

PRINTSQUARE MACRO: 
    LEA DX, SQUARE
    MOV SI, DX
    MOV DH, 11
    MOV DL, 35
    MOV CX, 3
    
    PRINT_LOOP:
        PUSH CX
        MOV AX, 3
        PRINT_ROW:
            PUSH AX
            MOV AH, 02H
            MOV BH, 00H
            INT 10H  
            MOV AH, 09H
            MOV BL, 02
            MOV CX, 1
            MOV AL, [SI]
            INT 10H
            
            INC SI
            INC DL
            POP AX
            DEC AX
            JNZ PRINT_ROW
        MOV DL, 35
        INC DH
        POP CX
        DEC CX
        JNZ PRINT_LOOP

ENDM             

START:
MOV AX, @DATA
MOV DS, AX
MOV ES, AX
PUSH BP    
MOV BP, SP 
;PRINTSQUARE
call createSeed
call genSquare
call copyStart 

MOV BP, OFFSET SCORE_STR
PRINTSTR 1, 10, 5

loop1: 
    CALL printScore
    PRINTSQUARE
    call    evalSquare
    call    keyPress
    call    updateSquare
    jmp loop1
MOV AX, 4C00H
INT 21H   

genSquare PROC
    MOV SI, offset start_square 
    MOV CX, 8
    setValue:
        CALL genRand
        ADD DX, 29
        MOV SI, DX
        INC SI
        LOOP setValue
      ret
genSquare   ENDP

copyStart   PROC
    MOV dI, offset start_square 
    MOV si, offset square
    MOV CX, 8
    copy:   
        mov aX, [di]
        mov si, aX
        inc di
        inc si
        loop copy
    ret
copyStart   ENDP   
;------------------
evalSquare PROC
    cld
    lea si, square
    lea di, square_win
    mov cx, 9
    repe cmpsb
    je A
    RET  
    A:
        MOV AX, 4C00H
        INT 21H
evalSquare ENDP
;-------------------
updateSquare PROC
    ; Update magic square based on key pressed
    cmp al, '1'
    je update_1
    cmp al, '2'
    je update_2
    cmp al, '3'
    je update_3
    cmp al, '4'
    je update_4
    cmp al, '5'
    je update_5
    cmp al, '6'
    je update_6
    cmp al, '7'
    je update_7
    cmp al, '8'
    je update_8
    cmp al, '9'
    je update_9
    ; Invalid key pressed
    ret
    
    update_1: 
        INC steps
        xor byte ptr [square + 0], 01h
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 4], 01h
        jmp exit_update
    update_2:    
        INC steps
        xor byte ptr [square + 0], 01h
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 2], 01h
        jmp exit_update      
    update_3:    
        INC steps
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 2], 01h
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 5], 01h
        jmp exit_update
    update_4:    
        INC steps
        xor byte ptr [square], 01h
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 6], 01h
        jmp exit_update  
    update_5:    
        INC steps
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 5], 01h
        xor byte ptr [square + 7], 01h
        jmp exit_update
    update_6:    
        INC steps
        xor byte ptr [square + 2], 01h    
        xor byte ptr [square + 5], 01h    
        xor byte ptr [square + 8], 01h   
        jmp exit_update
    update_7: 
        INC steps
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 6], 01h
        xor byte ptr [square + 7], 01h
        jmp exit_update
    update_8:    
        INC steps
        xor byte ptr [square + 6], 01h
        xor byte ptr [square + 7], 01h
        xor byte ptr [square + 8], 01h
        jmp exit_update
    update_9:    
        INC steps
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 5], 01h
        xor byte ptr [square + 7], 01h
        xor byte ptr [square + 8], 01h
        jmp exit_update
    exit_update:
        ret
updateSquare endp   


printScore PROC
    saveRegs
    XOR     CX, CX
     
    MOV AL, STEPS
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
    CMP STEPS, 9
    JA  PRINT
    MOV AX, 30H
    PUSH    AX
    PRINT:
        setCursor   2, 12
        ; Pop each digit from the stack and print it to the screen
        POP AX
        MOV AH, 09H ; Set up the function code for printing a character
        MOV CX, 1
        MOV BL, 2
        INT 10H     ; Call the BIOS to print the character
        setCursor   2, 13
        ; Pop each digit from the stack and print it to the screen
        POP AX
        MOV AH, 09H ; Set up the function code for printing a character
        MOV CX, 1
        MOV BL, 2
        INT 10H     ; Call the BIOS to print the character
    restoreRegs
    RET  
printScore ENDP 

;---------------------------
keyPress PROC
        MOV     AH,08H            
        INT     21H
        CALL    toLower
        RET
keyPress ENDP

; Convert a character to lower case
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