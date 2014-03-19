data segment
    args    db 255 dup(?)
    argPtr  db 128 dup(0)
    argNum  db 0    
data ends

assume ds:data, cs:code

code segment        
    
start:
    call init
    call parseArgs
    call printArgs
    call exit
    
    printArgs proc ; print command line arguments
        push ax
        push cx
        push dx
        
        xor cx, cx
        ; prints one argument
        printLoop:
            ; get argument address
            call getArg
            ; print argument to console
            mov ah, 9h
            int 21h
            ; print line break
            call crlf
            
            inc cl
            ; check if there are arguments left
            cmp cl, argNum
            jl printLoop
        
        pop dx
        pop cx
        pop ax
        ret
    endp
    
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
    endp
    
    getArgLen proc ; returns length of argument with index in cl
        push ax
        push bx
        
        xor bx, bx
        ; cl stores argument index
        call getArg
        mov al, c
        mov bl, cl
        getChar:
            inc bx
            cmp ds:[bx], 24h
            je endGetArgLen
            
            inc al
            jmp getChar
        
        endGetArgLen:
            mov dl, al
        
        pop bx
        pop ax
        ret
    endp
    
    getArg proc ; return address of argument with index of cl
        push bx
        xor bx, bx
        
        mov bl, cl ; stores index of argument
        ; return address in dl
        mov dl, [argPtr + bx] 
        
        pop bx
        ret
    endp
    
    getArgNum proc ; return number of arguments in dl
        mov dl, byte ptr argNum
        ret
    
    parseArgs2 proc
        push ax
        push bx
        push cx
        
        ; clear argument counter
        xor bl, bl
        
        mov si, 82h ; command line arguments offset
        mov di, offset args
        mov cl, byte ptr es:[80h] ; number of characters entered
        
        call removeWhitespace
        ; check for empty command line
        cmp cx, 0
        je endParseArgs2
        
        readLoop:
            call readArg
            call removeWhitespace
            
            dec cx
            cmp cx, 0
            jg readLoop
        
        ; save number of arguments
        endParseArgs2:
        mov argNum, bl
        
        pop cx
        pop bx
        pop ax    
        ret
    endp
    
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
    endp
    
    readArg proc
        raLoop:
            ; read next character
            mov al, byte ptr es:[si]
            ; check if whitespace
            cmp al, 20h
            jle endReadArg
            
            ; save character
            mov ds:[di], al
            inc di
            inc si
            
            dec cx
            cmp cx, 0
            jg raLoop
            
        endReadArg:
            ; end string with $
            mov ds:[di], 24h
            inc di
            ; increment argument counter
            inc bl
            ret
    endp        
            
    parseArgs proc
        push ax
        push bx
        push cx
        
        xor bx, bx
         
        mov si, 82h ; command line arguments offset
        mov di, offset args
        mov cl, byte ptr es:[80h] ; number of characters entered
             
        readChar:
            ; read next character
            mov al, byte ptr es:[si]
            ; check if whitespace
            cmp al, 20h 
            jle skip
            jg save
            
            save:
                call saveChar
                jmp rcLoop
                
            skip:
                call skipChar
                
            rcLoop:
                dec cx
                cmp cx, 0
                jg readChar

            ; save number of arguments
            mov argNum, bl
            
            pop cx
            pop bx    
            pop ax
            ret        
    endp 
    
    skipChar proc
        ; go to next character
        inc si
        ; check if just saved argument
        cmp bh, 1
        jne endSkipChar
            
        call saveArg
        
        endSkipChar:
        ret
    endp
    
    saveArg proc
        ; end string
        mov ds:[di], 24h
        inc di
        ; increment argument counter
        inc bl
        ; clear argument flag
        xor bh, bh
        ret
    endp
    
    saveChar proc
        mov ds:[di], al
        ; check if beginning of argument
        cmp bh, 0
        jne contSaveChar
        
        call getAddress  
            
        contSaveChar: 
            ; increment index pointers
            inc si
            inc di       
        ret
    endp
    
    getAddress proc
        ; bl contains number of arguments
        xor bh, bh        
        mov word ptr ds:[argPtr + bx], di
        ; mark that an argument is being read
        mov bh, 1
        ret
    endp
        
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
    endp
    
    exit proc
        mov ax, 4C00h
        int 21h
    endp

code ends

st segment stack
        dw 200 dup(?)
    top dw ?
st ends

end start