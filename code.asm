                                            
name "CONNECT 4"

org  100h

jmp code

; set video mode 13h - 320x200

code:  


; TESTING

 ;CALL COMPUTER_PLAYER
; END TESTING


; CLEAR MEMORY

LEA BX,first_address
MOV CX,36
CLEAR:
MOV [BX],0
ADD BX,2
LOOP CLEAR
    
MOV PLAYER_NUMBER,1

        mov ah, 0     ;cx-> x col  dx-> y row   
        mov al, 13h 
        int 10h                                  
             

;MOVE CURSOR             
MOV DH,1H    ;Y
MOV DL,15    ;X
MOV AH,2H
MOV BH,0
INT 10H
                                                     
LEA DX,INTRO
MOV AH,9H
INT 21H


;MOVE CURSOR             
MOV DH,4H    ;Y
MOV DL,7H    ;X
MOV AH,2H
MOV BH,0
INT 10H

LEA DX,OUR_NAMES
MOV AH,9H
INT 21H


;MOVE CURSOR             
MOV DH,8H    ;Y
MOV DL,2H    ;X
MOV AH,2H
MOV BH,0
INT 10H


LEA DX,OPTION
MOV AH,9H
INT 21H





;TAKE INPUT
MOV AH,0
INT 16H
CMP AL,30H ; 0
JE WOW_0               
MOV COMPUTER,1
JMP WOW 
WOW_0:
MOV COMPUTER,0
            	                   
WOW:

; CLEAR SCREEN
MOV AH,15
INT 10H
XOR AH,AH
INT 10H
            	                   
mov ax, 0
int 33h 
;mov ax, 1      ;  after render calc of mouse input      dx = -1 left    cx = 6 right
;int 33h   
seek:
mov ax,3
int 33h  
cmp bl,2
jne here
JMP CODE
here:
shr cx,1

call render

;mouse click

cmp bl,0
jne mouse1
mov bc,0 
jmp seek
mouse1:
cmp bc,0
jne seek 
mov bc,1 

inc bo
mov cx,curtx
mov dx,curty

call addin_memory


jmp seek
    


mov cx,3 ; wenta bass mn l shmal cx col
mov dx,-1 ; dx row
mov al,5    

;wait for keypress
  mov ah,00
  int 16h			                                 

; return to text mode:
  mov ah,00
  mov al,03 ;text mode 3
  int 10h



ret      
int 20h

  
draw_players_peace proc near 
mov ax,istart
push dx  
mov dx,0
div nob
mov point_y,ax
mov ax,dx
push bx
mov bx,2
mov dx,0
div bx   
pop bx
mov point_x,ax
pop dx


mov cx,point_x
mov dx,point_y
mov ax,player_number     ;color   
                            call drawpeace
MOV AX,0                           
ret   
endp draw_players_peace                   

get_starting_address proc near
; cx true dx change
push ax
push bx
push cx
push dx
cmp cx,6
jne  left_gs

mov ax,dx
inc ax
mov bx,10
push dx
mul bx  
pop dx
mov cx,ax 

mov bx,2
mov ax,dx
mul bx
add ax,cx
mov dx,ax
mov diff_in_add,-2
jmp endgetstartaddress

left_gs: 

shl cx,1
;
mov dx,cx 
mov diff_in_add,12

endgetstartaddress: 
mov istart,dx   

pop dx
pop cx
pop bx
pop ax


ret
endp get_starting_address   


addin_memory proc near
 ;   pusha
     
    call get_starting_address
    COMPUTER_BEGIN:
    lea bx,first_address 
    add bx,istart
    push ax 
    mov ax,0
    cmp [bx],ax
    pop ax      
    MOV AX,0
    jne  addinend
    mov cx,5
    
    addinl1:
    lea bx,first_address   
    add bx,istart
    add bx,diff_in_add 
    push ax 
    mov ax,0
    cmp [bx],ax
    pop ax
    jne addin_now
    push dx
    mov dx,diff_in_add
    add istart,dx 
    pop dx
    loop addinl1
    
    addin_now:     ;put in istart  
    lea bx,first_address
    add bx,istart
    mov ax,player_number
    CMP COMPUTER_LABLE,1
    JE  RETURN_TO_COMPUTER
    mov [bx],ax
    mov dx,cx  
    
    call draw_players_peace
    
    MOV NUMOFCELLSSEARCHFOR,4
    call check_winning_player
    
    cmp ax,0
    jNe terminate
    
    cmp player_number,4
    je p1
    
    ; IT WAS FIRST PLAYER  PLAYER_NUMBER = 1
    CMP COMPUTER,0
    JE P2
    ; COMPUTER PLAYER
    CALL COMPUTER_PLAYER            ; CALLING COMPUTER_PLAYER WHERE PLAYER_NUMBER = 1
    MOV  PLAYER_NUMBER,1
    JMP addinend
    
    P2:
    mov player_number,4  
    jmp addinend
    
    p1:
    mov player_number,1  
  
    addinend:     
             
      push ax
    push cx
    push dx
    mov ax,0
    mov dx,0
    mov cx,0
    mov cl,prevx
    mov dl,prevy
    cmp dl,00FFh
    jne dr21
    mov dx,0FFFFh   
    dr21:
    mov ax,player_number
    call drawpeace
    
    pop dx
    pop cx
    pop ax         
    jmp return 
  ;  popa 
    
    terminate: 
    JMP END_GAME
    
   return:
     
ret
endp addin_memory

render proc near
pusha
call calculate_out_index
jg  haha
push ax
push bx

mov al,cl
mov ah,dl
mov bl,prevx
mov bh,prevy 
cmp ax,bx  

pop bx
pop ax
je haha
push cx
push dx 
mov curtx,cx
mov curty,dx

mov ax,player_number 
                             call drawpeace 

mov cx,0
mov dx,0
mov cl,prevx
mov dl,prevy
cmp dl,00FFh
jne dr
mov dx,0FFFFh   
dr:
mov ax,0
                     call drawpeace 
pop dx
pop cx 
mov prevx,cl 
mov prevy,dl   		                                 
haha:
                      call drawgrid

popa
ret
endp         
         
         
calculate_out_index proc near
call calculate_indx 
cmp cx,6
jge ymen
mov dx,-1 ; dx row
mov al,5  
jmp drp
ymen:
  cmp cx,11 
  jg drp
  mov dx,cx 
  sub dx,6
  cmp cx,11
  mov cx,6

drp:  
ret    
endp calculate_out_index  

calculate_indx proc near
   
    mov ax,cx
    sub ax,67  
    mov bx,15
    mov dx,0
    div bx
    mov cx,ax
    
    
    ret
endp calculate_indx

drawpeace proc near
    push cx
    push dx
    push ax
    push bx
    
    push dx
    
    push cx 
    mov bx,cx
    add bx,dx
    

    mov ax,15
    mul bx
    
    mov cx,ax 
    add cx,67
    
    pop bx
    pop dx
    
    sub dx,bx
                   
     
    mov ax,15
    mul dx   

    mov dx,ax
    pop bx
    pop ax
    add dx,100  
    call drawrumbas
    
    pop dx
    pop cx
    
    
 ret
endp drawpeace  

drawgrid proc near


mov dx,115
mov cx,50 
mov bx,91
mov count,7
nxtl:
sub dx,15
add cx,15
call draw45rline
dec count
cmp count,0
ja nxtl

mov dx,85
mov cx,50
mov count,7
nxtr:
add dx,15
add cx,15
call draw45lline
dec count
cmp count,0
ja nxtr
    
ret
endp drawgrid


draw45rline proc near
    push ax
    push bx
    push cx
    push dx
     
    mov al,15       ; white  
un45r: 
     
     mov ah,0ch     ; put pixel
     int 10h 
     inc cx
     inc dx 
     dec bx
     cmp bx,0 
     ja un45r   
     
     
    pop dx  
    pop cx
    pop bx
    pop ax
    
  ret  
endp draw45line

draw45lline proc near  
    push ax
    push bx
    push cx
    push dx
    
    mov al,15       ; white 
un45l: 
     
     mov ah,0ch     ; put pixel
     int 10h 
     inc cx 
     dec dx
     dec bx
     cmp bx,0 
     ja un45l
    
    pop dx  
    pop cx
    pop bx
    pop ax
    
    ret  
endp draw45line



drawrumbas proc near

     push di
     push cx
     push dx
     push cx
     push dx
     
     mov di,dimondSize  

row1:      
    mov bx,cx 
    add cx,di

col1:     
          cmp dx,0
          je  erow1
          mov ah,0ch     ; put pixel
          int 10h
          dec cx
          cmp cx,bx
          ja col1           
    dec di 
    dec di
    inc cx
    dec dx 
    cmp di,0
    jg row1 
erow1:    
 mov di,dimondSize 
 pop dx
 pop cx  
 inc dx    
           
row2:      
  
    mov bx,cx 
    add cx,di

col2:     
          mov ah,0ch     ; put pixel
          int 10h
          dec cx
          cmp cx,bx
          ja col2
    dec di 
    dec di
    inc cx
    inc dx 
    cmp di,0
    jg row2        
            
     pop dx 
     pop cx
     pop di
     
ret
endp drawrumbas   


check_winning_player proc near
MOV DIFF_BTE_CELLS,2    ;2, 12
MOV START_FROM,4
CHECK_WINNING:
MOV AX,0             ; @ LAST IF AL==1 THEN FIRST WIN ELSIF == 2 THEN SECOND WIN ELSE NOONE WINS.
LEA BX,FIRST_ADDRESS ;ADDRESS OF FIRST 
MOV BP,PLAYER_NUMBER
BEGIN:             ;# OF THE PLAYER
MOV DX,0             ; number of row
NEXT_ROW:
MOV CX,3             ;#OF POSSIBOL FOURTH
ANOTHER_FOURTH:
MOV DI,0
CMP DIFF_BTE_CELLS,12
JE  CONTIN 
MOV DI,DX            ; # OF RWO * 12 (6 WORD (ROWS)) THEN ADD START ADDRESS
PUSH AX
MOV AX,DI
MUL NUMOFRWOS
MOV DI,AX  
POP AX
CONTIN:          
ADD DI,BX
ADD DI,START_FROM
PUSH CX
MOV CX,NUMOFCELLSSEARCHFOR
THERE:
CMP [DI],BP
JNE NOT_WIN
ADD DI,DIFF_BTE_CELLS
LOOP THERE

WINNING:
POP CX
MOV AX,BP           ;AL = # OF WINNER
JMP STOP

NOT_WIN:      
POP CX
PUSH AX
MOV AX,DIFF_BTE_CELLS
SUB START_FROM,AX
POP AX
LOOP ANOTHER_FOURTH ;PREV. FOURTH
; HERE NOT WIN && RWO ENDS
; CHECK IF IT IS LAST RWO

CMP DX,5H
JE  END_RWOS_FORPLAYER
INC DX
CMP DIFF_BTE_CELLS,2
JE  RWOS
MOV START_FROM,24
PUSH DX
ADD DX,DX
ADD START_FROM,DX
POP DX
JMP NEXT_ROW

RWOS:
MOV START_FROM,4
JMP NEXT_ROW
                                                                    
                                                                    
END_RWOS_FORPLAYER:                     ;IF IT WAS FIRST PLAYER       


CHECH_COLUMN:                ;NOW PLAYER WINS OR THERE IS NO FOURTH IN ROWS FOR BOTH PLAYERS

CMP DIFF_BTE_CELLS,2         ;IF RWOS AND COLUMNS AND CHECHED THEN FINISH_ROWSANDCOLUMNS ELSE CALL BY COLUMNS
JNE FINISH_ROWSANDCOLUMNS
MOV DIFF_BTE_CELLS,12
MOV START_FROM,24
JMP CHECK_WINNING

FINISH_ROWSANDCOLUMNS:       ;NOW CHECK DIAGONALS

MOV BP,PLAYER_NUMBER

BEGIN_DIAGONAL:
LEA DI,FIRST_ADDRESS            ;FIRST ADDRESS
LEA BX,DIAGONALS_CELLS_NUMBER   ;CELLS #            
LEA SI,DIAGONALS_CELLS_ADDRESS  ;CELLS ADDRESS
MOV CX,18 
 
AGAIN:
MOV DX,0 
MOV DL,[BX]            ; CX = ADDRESS OF THE CELL IN THE ARRAY (ONE CELL = 2 BYTE)
ADD DX,DI              ; THEN ADD STARTING ADDRESS             
MOV [SI],DX                                       
INC BX
ADD SI,2
LOOP AGAIN
 
;STARTING CELLS STORED 

;TEST FIRST 9 WITH DIFF_CELL = 10 
MOV DIFF_BTE_CELLS,10
MOV CX,9
LEA SI,DIAGONALS_CELLS_ADDRESS  ;CELLS ADDRESS
           
NEXT_DIAGONAL:           
MOV DI,[SI]
PUSH CX
MOV CX,NUMOFCELLSSEARCHFOR
THERE_DIAG:
CMP [DI],BP
JNE NOT_WIN_@_DIAGONAL
ADD DI,DIFF_BTE_CELLS
LOOP THERE_DIAG

WINNING_@_DIAGONAL:
POP CX
MOV AX,BP
JMP STOP

NOT_WIN_@_DIAGONAL:
POP CX
ADD SI,2
LOOP NEXT_DIAGONAL
CMP  DIFF_BTE_CELLS,10
JNE  FINISH
MOV DIFF_BTE_CELLS,14
MOV CX,9
JMP NEXT_DIAGONAL



FINISH:

STOP: 
ret
endp check_winning_PLAYER 


REACHABLE_OR_NOT PROC NEAR    ;INPUT: ADDRESS OF THE CELL STORD IN (DI) PUT IN AX,1 IF YES , 0 IF CAN'T  

LEA BP,FIRST_ADDRESS
MOV BX,DI     
SUB BX,BP                     ; BX HAS ADDRESS OF THE CELL
MOV CL,12
MOV AX,BX
MOV DX,0
DIV CX
MOV RIGHT,AX
MOV AX,DX
MOV CX,2
MOV DX,0
DIV CX
MOV LEFT,AX

;CHECK ADDING FROM RIGHT
MOV AX,RIGHT
ADD AX,1
MOV DL,12
MUL DL
SUB AX,2
MOV ISTART,AX
MOV diff_in_add,-2
MOV COMPUTER_LABLE,1

JMP COMPUTER_BEGIN

RETURN_TO_COMPUTER:
MOV COMPUTER_LABLE,0
CMP BX,DI
JE  AVAILABLE

CMP diff_in_add,12
JE  NOT_AVAILABE    ; IT WAS NEXT TIME (LEFT)

;CHECK ADDING FROM LEFT
MOV AX,LEFT
MOV DL,2
MUL DL
MOV ISTART,AX
MOV diff_in_add,12
MOV COMPUTER_LABLE,1

JMP COMPUTER_BEGIN

NOT_AVAILABE:
MOV AX,0
JMP END_REACHABLEEEE 

AVAILABLE:
MOV AX,1

END_REACHABLEEEE:
         
RET 
ENDP REACHABLE_OR_NOT      
      

                  
                  
COMPUTER_PLAYER proc near    

MOV NUMOFCELLSSEARCHFOR,3
SEARCH_FOR_CELLS:
MOV  PLAYER_NUMBER,1   
CALL CHECK_WINNING_PLAYER
CMP AX,0
JE  COMPUTER_NOT_WINNING
JMP COMPUTER_ADD

COMPUTER_NOT_WINNING:      ; CHECK TO NOT LOSS 
MOV PLAYER_NUMBER,4
CALL CHECK_WINNING_PLAYER
CMP AX,0
JE GENERAT_RANDOM
JMP COMPUTER_ADD
                
                           
GENERAT_RANDOM:            ;NOT WIN,NOT LOSS THEN PLAY RANDOM 
;CHECK ABOUT 2 CELLS FIRST

CMP NUMOFCELLSSEARCHFOR,3
JNE START_RANDOM
MOV NUMOFCELLSSEARCHFOR,2
JMP SEARCH_FOR_CELLS     

START_RANDOM:
LEA BX,FIRST_ADDRESS
MOV DI,BX        
MOV AH,0
INT 1AH
MOV AX,DX
MOV DX,0
MOV CX,36
DIV CX
MOV DI,DX

;BE SURE DX IS EVEN
TO_EVEN:
MOV CX,254 ; 1111 1110
AND DI,CX


LEA BX,COMPUTER_SELECT
ADD BX,DI 
MOV DI,[BX] 
LEA BX,FIRST_ADDRESS
ADD DI,BX

COMPUTER_ADD:                          
CMP [DI],0
JNE GENERAT_RANDOM             

; CHECK THIS CELL IS REACHABLE

mov si,di
CALL REACHABLE_OR_NOT
CMP AX,0 
JE  START_RANDOM     ; NOT REACHABLE                                 
mov DI,si


MOV [DI],4

; DRAW
       
LEA BX,FIRST_ADDRESS       
SUB DI,BX
MOV ISTART,DI
MOV PLAYER_NUMBER,4   
CALL draw_players_peace

MOV NUMOFCELLSSEARCHFOR,4
MOV PLAYER_NUMBER,4
CALL CHECK_WINNING_PLAYER 
CMP AX,0
JNE END_GAME
                      
                       
RET
ENDP COMPUTER_PLAYER


END_GAME:
MOV CX,AX
; CLEAR SCREEN
;MOV AH,15
;INT 10H
;XOR AH,AH
;INT 10H

; MOVE CURSOR
MOV DH,5
MOV DL,3
MOV AH,2
MOV BH,0
INT 10H


; PRINT WINNING PLAYER
CMP CX,1
JNE SECOND
LEA DX,FIRST_PLAYER
JMP PRINT

SECOND:
CMP COMPUTER,1
JE COMPUTER_WINN
LEA DX,SECOND_PLAYER
JMP PRINT
COMPUTER_WINN:
LEA DX,COMPUTER_THE_WINNER

PRINT:
MOV AH,9H
INT 21H


; MOVE CURSOR AGAIN
MOV DH,7
MOV DL,1
MOV AH,2
MOV BH,0
INT 10H

; PRINT ENTER_ESCAP
LEA DX,ENTER_ESCAP
MOV AH,9H
INT 21H

CHECK_NEW_GAME:
MOV AH,0
INT 16H

CMP AL,13 ; ENTER
JE  CODE

CMP AL,1BH
JE CLOSE
JMP CHECK_NEW_GAME

CLOSE:
; CLEAR SCREEN
MOV AH,15
INT 10H
XOR AH,AH
INT 10H

INT 20H
RET

                                
dimondSize dw 25
count dw 0 
prevx db 0
prevy db 0
curty dw 0
curtx dw 0 
bc    db 0
first_address dw 36 Dup (0)    
pcounter dw 0     
bo dw 0 
istart dw 0  
diff_in_add dw 0 
player_number dw 1    
nob dw 12  ; number of bytes/row      
point_x dw 0
point_y dw 0  
DIAGONALS_CELLS_NUMBER DB   06,08,10,18,20,22,30,32,34, 00,02,04,12,14,16,24,26,28

DIAGONALS_CELLS_ADDRESS DW      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

NUMOFRWOS DB 12
DIFF_BTE_CELLS DW 2    ;2, 12
START_FROM DW 4        ;4, 24
FIRST_PLAYER DW 'CONGRATULATIONS TO FIRST PLAYER ;) $'
SECOND_PLAYER DW 'CONGRATULATIONS TO SECOND PLAYER ;) $'
COMPUTER_THE_WINNER DW 'YOU ARE THR LOSSER :P $'
ENTER_ESCAP DW 'PRESS ENTER TO NEW GAME,ESCAPE TO EXIT$'
NUMOFCELLSSEARCHFOR DW 4
LEFT  DW 0
RIGHT DW 0
IN_FRONT_ME DW 0
COMPUTER_DIFF DW 2                                                

COMPUTER_LABLE DW 0 
COMPUTER_SELECT DW 0,5,10,15,20,25,30,35,1,6,11,16,21,26,31,2,7,12,17,22,27,32,3,8,13,18,23,28,33,4,9,14,19,24,29,34

COMPUTER DW 0 ; 0 2 PLAYER , 1 1 PLAYER AND COMPUTER IS THE SECOND
INTRO DW 'CONNECT-4                                  BY$'
OUR_NAMES DW 'M.SALEH           M.WAGDY$'
OPTION DW 'press (0) to PLAYER Vs. PLAYER          press (1) to PLAYER Vs. COMPUTER$'
