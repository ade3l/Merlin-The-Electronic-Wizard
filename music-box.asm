.model small
.stack 100h
.data
.code
; Get a series of characters from the keyboard
; and store them in stack
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

; Print the stack
PRINT PROC
    beginPrint:
        CMP    CX, 0 ;if the counter is 0 then there is nothing to print
        JE     leavePrint 
        ADD    BP,2
        MOV    AX,[BP-2] ;get the character from the stack
        MOV    DL, AL
        MOV    AH, 02H 
        INT    21H ;print the character
        DEC    CX 
        JMP    beginPrint ;print the next character
    leavePrint:
        RET
PRINT ENDP


player PROC
    push    bp
    mov     bp,sp
    MOV     CX, 0 
    BEGIN:
        CALL    keyPress
        CMP     AL, 27 ; ESC
        JNE     KEYED
        RET
    
    KEYED:    
        ;if the key is p then move onto printing the stack
        CMP     AL, 'p' ; 
        JNE     SAVEKEY
        CALL    PRINT
        MOV     sp,bp       ;restore sp
        POP     bp 
        RET
    
    SAVEKEY:
        PUSH    AX  ;save the key pressed in the stack
        SUB     BP,02
        INC     CX  ;increment the counter
        JMP     BEGIN

player ENDP
start:
    MOV     AX, @data
    MOV     DS, AX
    CALL    player    
    MOV     AX, 4C00H
    INT     21H
END start