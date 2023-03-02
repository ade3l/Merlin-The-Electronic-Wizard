.model small
.stack 100h
.data
.code
; Get a series of characters from the keyboard
; and store them in stack

PLAY MACRO frequency               ;This macro receives the tone
        MOV     AX,frequency       ;and sends to call the procedures
        CALL    setFreq
        CALL    activateSpkr
ENDM

;SEND THE FREQUENCY IN TWO SEPARATE BYTES TO THE PORT.
setFreq  PROC                   
        OUT     42h, AL 
        MOV     AL, AH
        OUT     42h, AL 
        RET
setFreq  ENDP
;------------------------------------------------------------------------------------
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
;------------------------------------------------------------------------------------

activateSpkr proc                      ;Activate the horn and place the name of
        CALL speakerOn           ;The key
        
        MOV     CX, 07H           ; Wait 0.5 second
        MOV     DX, 0A120H        ;before turning off the speaker
        MOV     AH, 86H
        INT     15H

        CALL    speakerOff       ;Horn disconnects and returns.
        RET
activateSpkr endp

;------------------------------------------------------------------------------------

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
;------------------------------------------------------------------------------------

PLAYBACK PROC
    beginPop:
        CMP    BX, 0 ;if the counter is 0 then there is nothing to print
        JE     leavePrint 
        DEC BX
        ADD    BP,2
        MOV    AX,[BP-2] ;get the character from the stack
        CMP     AL,'q'   ;DO high
        JNE     S1       ;IF NOT THE EXPECTED KEY, JUMP TO CHECK NEXT.
        PLAY    2280      ;IF IT IS THE EXPECTED KEY, IT GENERATES THE CORRESPONDING SOUNDE
        JMP     beginPop ;AFTER THE SOUND, RESTART TO WAIT FOR ANOTHER SOUND.
        S1:     
            CMP     AL,'w'   ;RE high
            JNE     S2
            PLAY    2031
            JMP     beginPop
        S2:     
            CMP     AL,'e'   ;MI high
            JNE     S3
            PLAY    1809
            JMP     beginPop
        S3:     
            CMP     AL,'r'   ;FA high
            JNE     S4
            PLAY    1715
            JMP     beginPop
        S4:     
            CMP     AL,'t'   ;SOL high
            JNE     S5
            PLAY    1521
            JMP     beginPop
        S5:     
            CMP     AL,'y'   ;LA high
            JNE     S6
            PLAY    1355
            JMP     beginPop
        leavePrint:
            RET
        S6:     
            CMP     AL,'u'   ;TI high
            JNE     S7
            PLAY    1207
            JMP     beginPop
        S7:     
            CMP     AL,'i'
            JNE     beginPop 
            PLAY    1140
        addCount:
            ADD     BX, 1
            JMP     beginPop
PLAYBACK ENDP

;------------------------------------------------------------------------------------

player PROC
    push    bp
    mov     bp,sp
    MOV     BX, 0 
    BEGIN:
        CALL    keyPress
        CMP     AL, 27 ; ESC
        JNE     KEYED
        MOV     sp,bp       ;restore sp
        POP     bp 
        RET
    
    KEYED:    
        ;if the key is p then move onto printing the stack
        CMP     AL, 'p' ; 
        JNE     SAVEKEY
        CALL    PLAYBACK
        MOV     sp,bp       ;restore sp
        POP     bp 
        RET
    
    SAVEKEY:
        PUSH    AX  ;save the key pressed in the stack
        SUB     BP,02
        INC     BX  ;increment the counter
        JMP     BEGIN

player ENDP
start:
    MOV     AX, @data
    MOV     DS, AX
    CALL    player    
    MOV     AX, 4C00H
    INT     21H
END start