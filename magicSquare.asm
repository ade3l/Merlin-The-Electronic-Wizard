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
PRINTSQUARE
MOV AX, 4C00H
INT 21H

 
END