.model small
.stack 100h
.data
square db "000000000"
steps db 0
.code 

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
PUSH BP    
MOV BP, SP 
;PRINTSQUARE
loop1:
    PRINTSQUARE
    call    evalSquare
    call    updateSquare
    
    jmp loop1
MOV AX, 4C00H
INT 21H

updateSquare PROC
    ; Get key press and convert to lowercase
    call keyPress
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
        xor byte ptr [square + 0], 01h
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 4], 01h
        jmp exit_update
    update_2:
        xor byte ptr [square + 0], 01h
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 2], 01h
        jmp exit_update      
    update_3:
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 2], 01h
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 5], 01h
        jmp exit_update
    update_4:
        xor byte ptr [square], 01h
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 6], 01h
        jmp exit_update  
    update_5:
        xor byte ptr [square + 1], 01h
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 7], 01h
        jmp exit_update
    update_6:
        xor byte ptr [square + 2], 01h    
        xor byte ptr [square + 5], 01h    
        xor byte ptr [square + 8], 01h   
        jmp exit_update
    update_7:
        xor byte ptr [square + 3], 01h
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 6], 01h
        xor byte ptr [square + 7], 01h
        jmp exit_update
    update_8:
        xor byte ptr [square + 6], 01h
        xor byte ptr [square + 7], 01h
        xor byte ptr [square + 8], 01h
        jmp exit_update
    update_9:
        xor byte ptr [square + 4], 01h
        xor byte ptr [square + 5], 01h
        xor byte ptr [square + 7], 01h
        xor byte ptr [square + 8], 01h
        jmp exit_update
    exit_update:
        ret
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

 
END