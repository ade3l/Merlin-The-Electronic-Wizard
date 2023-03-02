.model-small
.org 100h
.code
; Get a series of characters from the keyboard
; and store them in stack
keyPress PROC
        MOV     AH,8            
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
        CMP    CX, 0 ;if the counter is 0 then there is nothing to print
        JE     EXIT
        MOV    AH, 9 ;print a character
        POP    AL ;get the character from the stack
        INT    21H ;print the character
        JMP    PRINT ;print the next character
    
    EXIT:
        RET
PRINT ENDP
player PROC
    BEGIN:
        CALL    keyPress
        CMP     AL, 27 ; ESC
        JE      EXIT
        ;if the key is p then move onto printing the stack
        CMP     AL, 'p' ; 
        JE      toPrint
        PUSH    AL  ;save the key pressed in the stack
        INC     CX  ;increment the counter
        JMP     BEGIN
    EXIT:
        RET
    toPrint:
        CALL    PRINT
        RET
player ENDP
start:
    MOV     AX, @data
    MOV     DS, AX
    MOV     CX, 0
    CALL    player
    MOV     AX, 4C00H
    INT     21H
END start