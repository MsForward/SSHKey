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
    call exit
    
    getArgLen proc
        
    
    getArg proc
        xor bx, bx
        
        mov bx, offset argPtr
        add bx, dx ; stores index of argument
        mov bl, byte ptr ds:[bx]
        ret
    endp
    
    getArgNum proc
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
        
        push bx
        xor bh, bh
        add bx, offset argPtr        
        mov ds:[bx], di
        pop bx
        ; mark that an argument is being read
        mov bh, 1    
            
        contSaveChar: 
            ; increment index pointers
            inc si
            inc di
        
        pop dx       
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