.model small
.stack 100h
.data
msg DB 'Hello World$'
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
    MOV     DL,g column  
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
    CMP     AX, '$'
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
    
            
        
END

