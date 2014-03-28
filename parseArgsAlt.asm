; Methods:
;   parseArgs
;   printArgs
;   getArg
;       entry: DL = argument index
;       return: AL = argument offset
;   getArgLen
;       entry: DL = argument index
;       return: AL = argument length
;   getArgNum
;       return: AL = number of arguments
;   checkArgs
;       return: AL = 1 if arguments valid else 0
;   checkKey
;       return: AL = 1 if key valid else 0
;   checkVersion
;       return: AL = 1 if version valid else 0
;   printError
;       entry: DX = error offset

data segment
; Input
    args            db 255 dup(?)   ; stores command line arguments
    argPtr          db 128 dup(0)   ; array of argument offsets
    argNum          db 0            ; stores number of arguments
    runV            db 0            ; version of algorithm to run
    keyPtr          db 0            ; offset of SSH key 
    
; Error messages    
    argNumErr       db "Too many arguments! Usage: version key","$"
    argLenErr       db "Wrong arguments! Usage: version(0|1) 16-byte key","$"
    versionErr      db "Wrong version number! Usage version(0|1) key","$"
    keyCharErr      db "Invalid character in key! Use digits 0-9 and letters A-F","$"
data ends

assume ds:data, cs:code

code segment        
    
start:
    call init
    call parseArgs
    call printArgs
    call exit
    
    checkArgs proc
        push bx
        push cx
        push dx
        
        xor bx, bx
        
        ; check if there are 2 arguments
        cmp argNum, 2h
        jne wrongArgNum
        
        ; check if version number is 1 character long
        xor dl, dl
        call getArgLen
        cmp al, 1h
        jg wrongArgLen
        ; check if key is 32 characters long
        mov dl, 1h
        call getArgLen
        cmp al, 20h
        jne wrongArgLen
        
        call checkKey
        cmp al, 1h
        jne endCheckArgs
        
        wrongArgNum:
            mov dx, offset argNumErr
            call printError
            xor al, al
            jmp endCheckArgs
            
        wrongArgLen:
            mov dx, offset argLenErr
            call printError
            xor al, al
            jmp endCheckArgs
        
        wrongVersion:
            mov dx, offset versionErr
            call printError
            xor al, al
            jmp endCheckArgs
        
        ; return true    
        mov al, 1h    
        endCheckArgs:
        
        pop dx
        pop cx
        pop bx
        ret
    checkArgs endp
    
    checkKey proc
        push bx
        push dx
        
        xor bx, bx
        xor dx, dx
        
        ; get key offset
        mov dl, 1h
        call getArg
        mov bl, al
        mov dl, al
        add dx, 20h
        
        checkChar:
            mov al, ds:[bx]
            cmp al, 30h
            jl invalidChar
            cmp al, 5Ah
            jg invalidChar
            ; check if digit
            cmp al, 3Ah
            jl ccLoop
            ; check if letter 
            cmp al, 40h
            jg ccLoop
        
        invalidChar:
            mov dx, offset keyCharErr
            call printError
            ; return false
            xor al, al
            jmp endCheckKey
            
        ccLoop:
            inc bx
            cmp bx, dx
            jle checkChar
        
        ; return true 
        mov al, 1h
        endCheckKey:
        
        pop dx
        pop bx
        ret
    checkKey endp
    
    printError proc
        push ax
        mov ah, 9h
        int 21h
        pop ax
        ret
    printError endp
    
    checkVersion proc
        push ax
        push bx
        push dx
        
        ; check if version number is 0 or 1
        xor dl, dl
        call getArg
        mov bl, al
        mov al, ds:[bx]
        ; get numerical value from character
        sub al, 30h
        ; check if 0
        cmp al, 1h
        jle saveVersion 
        
        xor al, al
        jmp endCheckVersion
        
        saveVersion:
            mov runV, al
            mov al, 1h
        
        endCheckVersion:    
        pop dx
        pop bx
        pop ax
        ret
    checkVersion endp
    
    parseArgs proc
        push ax
        push bx
        push cx       
        
        mov si, 82h ; command line arguments offset
        mov di, offset args
        mov cl, byte ptr es:[80h] ; number of characters entered
        
        ; check if command line should be truncated 
        mov ax, ds
        mov bx, es
        ; calculate offset between data segment argument segment
        sub ax, bx
        shl ax, 4d
        ; add offset of arguments
        sub ax, 82h
        ; compare with number of characters entered
        cmp cl, al
        jle validLength
        
        ; read only characters that are available
        mov cl, al
        
        validLength:
        
        ; clear argument counter
        xor bx, bx
        
        call removeWhitespace
        ; check for empty command line
        cmp cx, 0
        je endParseArgs
        
        readLoop:
            call readArg
            call removeWhitespace
            
            cmp cx, 0
            jg readLoop
        
        ; save number of arguments
        endParseArgs:
        mov argNum, bl
        
        pop cx
        pop bx
        pop ax    
        ret
    parseArgs endp
    
    removeWhitespace proc
        rwLoop:
            ; read next character
            mov al, byte ptr es:[si]
            ; check if whitespace
            cmp al, 20h
            jg endRemoveWhitespace

            inc si
            dec cx
            cmp cx, 0
            jg rwLoop
            
        endRemoveWhitespace:
            ret
    removeWhitespace endp
    
    readArg proc
        
        call putAddress
        raLoop:
            ; read next character
            mov al, byte ptr es:[si]
            ; check if whitespace
            cmp al, 20h
            jle endReadArg
            
            cmp al, '$'
            je illegalChar
            ; save character
            mov ds:[di], al

            inc di
            
        illegalChar:    
            inc si
            
            dec cx
            cmp cx, 0
            jg raLoop
            
        endReadArg:
            ; end string with $
            mov ds:[di], '$'
            inc di
            ; increment argument counter
            inc bl
            
            ret
    endp        
    
    putAddress proc
        push bx
        
        ; bl contains number of arguments
        xor bh, bh        
        mov word ptr ds:[argPtr + bx], di
        
        pop bx
        ret
    readArg endp
    
    getArg proc
    ; entry: DL = argument index 
    ; return: AL = argument offset
     
        push bx
        xor bx, bx
        
        mov bl, dl 
        mov al, [argPtr + bx] 
        
        pop bx
        ret
    getArg endp
    
    getArgLen proc 
    ; entry : DL = argument index
    ; return: AL = arugment length
    
        push ax
        push bx   
        
        ; clear address storage
        xor bx, bx
        ; dl stores argument index
        call getArg
        ; save first offset
        mov al, 1
        mov bl, dl
        getChar:
            ; go to next character 
            inc bx
            ; check for end of argument
            cmp ds:[bx], 24h ; ASCII code for '$'
            
            ; return if encountered end of string
            je endGetArgLen
            ; else increment character counter and read next
            inc al
            jmp getChar
        
        endGetArgLen:
        
        pop bx
        pop ax
        ret
    getArgLen endp
    
    getArgNum proc
    ; return: AL = number of arguments
    
        mov al, argNum
        ret
    getArgNum endp
    
    printArgs proc ; prints command line arguments
        push ax
        push cx
        push dx
        
        xor cx, cx
        ; prints one argument
        printLoop:
            ; check for next argument
            cmp cl, argNum
            ; if no arguments left return
            je endPrintLoop
            
            ; get argument address
            mov dl, cl
            call getArg
            ; print argument to console
            mov dl, al
            mov ah, 9h
            int 21h
            ; print line break
            call crlf
            
            inc cl
            jmp printLoop
            
        endPrintLoop:
        
        pop dx
        pop cx
        pop ax
        ret
    printArgs endp
    
    crlf proc ; prints line break 
        push ax
        push dx
        
        ; prints new line
        mov dl, 0Ah
        mov ah, 2h
        int 21h
        ; prints carriage return
        mov dl, 0Dh
        int 21h 
        
        pop dx
        pop ax
        ret
    crlf endp
    
    init proc
        ; save data segment
        mov ax, seg args
        mov ds, ax
        
        ; initialize stack
        mov ax, seg top
        mov ss, ax
        mov sp, offset top
        
        ; clear arithmetic registers
        xor ax, ax
        xor bx, bx
        xor cx, cx
        xor dx, dx
        ret
    init endp
    
    exit proc ; returns control to system
        mov ax, 4C00h
        int 21h
    exit endp

code ends


st segment stack
        dw 200 dup(?)
    top dw ?
st ends

end start