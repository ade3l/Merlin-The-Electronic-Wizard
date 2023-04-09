.model small
.stack 100h
.data 

mult    DW  25173
incr    DW  13849          
SEED    DW  ?
uLimit  DW  8
lLimit  DW  1  
  
diff_msg db "Select your difficulty level 1-9:" 
diff_msg_len equ $-diff_msg   

selected_msg db "Selected level: "
selected_msg_len equ    $-selected_msg 

tune_secret DB "000000000"
diff    db ?
octave dw 2280, 2031, 1809, 1715, 1521, 1355, 1207, 1140
.code


PLAY MACRO frequency
        saveRegs               
        MOV     AX,frequency       
        CALL    setFreq
        CALL    activateSpkr 
        restoreRegs
ENDM

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

pause MACRO
        saveRegs
        MOV     CX, 07H           
        MOV     DX, 0A120H        
        MOV     AH, 86H
        INT     15H
        restoreRegs
endm
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
    CLRSCR    
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
    
    call createTune
                   
    LEA BP, TUNE_SECRET
    printstr 3, 1, 9  
    
    call playTune
MOV AX, 4C00H
INT 21H 
;---------------------------------------------------------------------------------------- 
createTune proc
    saveRegs
    XOR CX, CX
    MOV CL, diff
    mov si, offset tune_secret
    createNote:
        CALL    genRand 
        ADD DL, '0'
        mov [si], DL
        inc si
        LOOP createNote
    restoreRegs
    RET
createTune endp

playTune proc
    saveRegs
    mov si, offset tune_secret
    
    xor ax, ax 
    xor cx, cx
    mov cl, diff 
    
    playNote:
        pause 
        mov di, offset octave 
        mov dx, 2
        mov al, [si]
        inc si
        sub al, '0'
        sub al, 1
        mul dx  
        add di, ax
        mov bX, [di] 
        play bx
        loop playNote
    restoreRegs
    ret
playTune endp


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

setFreq  PROC                   
        OUT     42h, AL 
        MOV     AL, AH
        OUT     42h, AL 
        RET
setFreq  ENDP

speakerOn  PROC                  
        MOV AL, 182
        OUT 43H, AL 
        
        IN      AL, 61h
        OR      AL, 11B
        OUT     61h, AL
        RET
speakerOn  ENDP

speakerOff  PROC               
        IN      AL, 61h
        AND     AL, 11111100b
        OUT     61h, AL
        RET
speakerOff  ENDP


activateSpkr proc                      
        CALL speakerOn           
        
        MOV     CX, 07H           
        MOV     DX, 0A120H        
        MOV     AH, 86H
        INT     15H

        CALL    speakerOff       
        RET
activateSpkr endp

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
    mov     cl, 4
    shr     ax, cl                                     
    
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


