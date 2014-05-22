; Methods:
;   drunkenBishop                                   Creates bishop movement map for given key
;   move
;       entry: DH = x position,                     Performs 4 moves for one key byte
;           DL = y position, 
;           AL = byte to analize
;   moveH                                           Performs one horizontal move
;       entry: DH = x position, 
;           DL = y position, 
;           AL = current byte
;       return: DH = new x position, 
;           DL = new y position
;   moveV                                           Performs one vertical move
;       entry: DH = x position, 
;           DL = y position, 
;           AL = current byte
;       return: DH = new x position, 
;           DL = new y position
;   getLastBit                                      Returns last bit of given byte
;       entry: DL = byte to convert
;       return: AL = last bit
;   getPosition                                     Returns array index for given x and y coordinates
;       entry: DH = x, DL = y
;       return: BX = index
;   movePossible                                    Checks if move is valid
;       entry: DH = x to check, DL = y to check
;       return: AL = 1 if move valid else 0
;   parseArgs                                       Saves command line arguments as array of string
;   putAddress                                      Saves arguments offset in pointer array                                       
;   printArgs                                       Prints all command line arguments
;   getArg                                          Returns offset for argument with given index
;       entry: DL = argument index
;       return: AL = argument offset
;   getArgLen                                       Returns length of argument
;       entry: DL = argument index
;       return: AL = argument length
;   getArgNum                                       Returns number of command line arguments
;       return: AL = number of arguments
;   checkArgs                                       Checks if arguments are valid
;       return: AL = 1 if arguments valid else 0
;   parseKey                                        Checks if SSH key is valid and convert to number array
;       return: AL = 1 if key valid else 0
;   checkVersion                                    Checks if version number is valid
;       return: AL = 1 if version valid else 0
;   println
;       entry: DX = string offset                   Prints string to console
;   printchar
;       entry: DL = character ASCII                 Prints one character to console
;   printError
;       entry: DX = error offset
;   crlf                                            Prints new line to console   

data segment
; Constants
    KEY_LEN         db 20h
    MAX_STEPS       db 0Eh

; Input
    args            db 255 dup(?)   ; stores command line arguments
    argPtr          db 128 dup(0)   ; array of argument offsets
    argNum          db 0            ; stores number of arguments
    runV            db 0            ; version of algorithm to run
    key             db 16 dup(?)    ; numerical key
    keyBytes        db 10h
    
; Drunken bishop
    array           db 153 dup(0)   ; szachownica
    sizeX           db 17d
    sizeY           db 9d
    frame           db "+-----------------+", "$"
    ASCII           db " ", ".", "o", "+", "=", "*", "B", "O", "X", "@", "%", "&", "#", "/", "^"
    startPos        db 76d
    endPos          db 0
    
; Error messages    
    argNumErr       db "Wrong argument number! Usage: version key","$"
    argLenErr       db "Wrong arguments! Usage: version(0|1) 16-byte key","$"
    versionErr      db "Wrong version number! Usage version(0|1) key","$"
    keyCharErr      db "Invalid character in key! Use digits 0-9 and letters A-F","$"
data ends

.286
assume ds:data, cs:code

code segment        
    
start:
    call init
    call parseArgs
    call checkArgs
    call drunkenBishop 
    call fingerprint    
    call exit
    
    drunkenBishop proc
        push ax
        push cx
        push dx
        
        ; start reading SSH key
        mov si, offset key
        ; set bishop at starting position
        mov dh, 8d
        mov dl, 4d
        ; read key length
        mov cl, keyBytes

        bishopLoop:
            ; read one byte from key
            mov al, byte ptr ds:[si]
            ; analize byte and mark bishops position
            call move
            ; DH = current x position
            ; DL = current y position
            ; go to next key byte
            inc si
            dec cx
            ; check for end of key
            cmp cx, 0
            jg bishopLoop
        
        ; calculate array index from x and y coordinates
        call getPosition
        ; save the final position
        mov endPos, bl    
        
        pop dx
        pop cx
        pop ax 
        ret
    drunkenBishop endp
    
    move proc
    ; entry: DH = x position, DL = y position, AL = byte to analize
        push ax
        push bx
        push cx

        ; analize 4 bit pairs
        mov cx, 4h
        moveOne:
            ; remember previous position
            mov bx, dx
            ; calculate new position
            call moveH
            shr al, 1
            cmp runV, 1
            jne continueMove
            call modify

            continueMove:
            call moveV   
            shr al, 1
            ; check if position has changed
            cmp bx, dx
            je mvLoop

            ; save new position
            call getPosition
            inc [array + bx]

            mvLoop:
                dec cx
                cmp cx, 0h
                jg moveOne
        
        pop cx
        pop bx
        pop ax
        ret
    move endp

    modify proc
    ; CX = loop number, DH = x position, DL = y position, BX = previous position
        push ax
        push bx
        push dx

        mov dl, cl
        call getLastBit
        cmp al, 1
        jne endModify
        cmp dx, bx
        je endModify
        call getPosition
        inc [array + bx]

        endModify:
        pop dx
        pop bx
        pop ax
        ret
    modify endp

    moveH proc
    ; entry: DH = x position, DL = y position, AL = current byte
    ; return: DH = new x position. DL = new y position
        push ax
        push bx
        push dx
        
        mov dl, al
        call getLastBit
        pop dx
        mov bx, dx
        ; 1 - move right, 0 - move left
        cmp al, 1h
        je moveRight
        moveLeft: 
            dec dh
            jmp checkMoveH
        moveRight: inc dh
        ; check if indices are not out of bounds
        checkMoveH:
            call movePossible
            cmp al, 1h
            je endMoveH
            ; if move not possible return to previous position
            mov dx, bx

        endMoveH:
        pop bx
        pop ax
        ret
    moveH endp
    
    moveV proc
    ; entry: DH = x position, DL = y position, AL = current byte
    ; return: DH = new x position. DL = new y position
        push ax
        push bx
        push dx
        
        mov dl, al
        call getLastBit
        pop dx
        mov bx, dx

        cmp al, 1h
        je moveDown
        ; 1 - move down, 0 - move up
        moveUp: 
            dec dl
            jmp checkMoveV
        moveDown: inc dl
        checkMoveV:
            call movePossible
            cmp al, 1h
            je endMoveV
            ; if move not possible return to previous position
            mov dx, bx

        endMoveV:
        pop bx
        pop ax
        ret
    moveV endp

    getLastBit proc
    ; entry: DL = byte to analize
    ; return: AL = 1 or 0
        mov al, dl
        and al, 00000001b
        ret
    getLastBit endp

    getPosition proc
    ; entry: DH = x position, DL = y position
    ; return: BX = array position
        push ax
        
        mov al, dl
        mov ah, sizeX
        mul ah
        add al, dh
        
        mov bx, ax
        
        pop ax
        ret
    getPosition endp
    
    movePossible proc
    ; entry: DH = x position to check, DL = y position to check
    ; return: AL = 1 if move is valid else 0  
        xor al, al

        cmp dh, 0h
        jl moveInvalid
        cmp dh, sizeX
        jge moveInvalid
        cmp dl, 0h
        jl moveInvalid
        cmp dl, sizeY
        jge moveInvalid

        mov al, 1h

        moveInvalid:
        ret        
    movePossible endp

    checkArgs proc
        push ax 
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
        cmp al, KEY_LEN
        jne wrongArgLen
        
        ; check if key contains valid characters and convert characters to values
        call parseKey
        call checkVersion
        jmp endCheckArgs
        
        wrongArgNum:
            mov dx, offset argNumErr
            call printError
            
        wrongArgLen:
            mov dx, offset argLenErr
            call printError
         
        endCheckArgs:
        
        pop dx
        pop cx
        pop bx
        pop ax 
        ret
    checkArgs endp
    
    parseKey proc
        push ax
        push cx
        push dx
        
        xor ah, ah
        ; get key offset
        mov dl, 1h
        call getArg
        mov si, ax
        mov di, offset key
        
        parseChar:
            ; read byte
            mov dl, byte ptr ds:[si]
            ; check for end of key
            cmp dl, '$'
            je endparseKey
            ; check if character is valid
            call checkChar
            call convertChar
            ; AL = numerical value of char
            mov ah, al
            shl ah, 4
            inc si
            mov dl, byte ptr ds:[si]
            call checkChar
            call convertChar
            ; AL = numerical value of char
            add al, ah
            
            ; save byte
            mov byte ptr ds:[di], al
            inc si
            inc di
            jmp parseChar
    
        endparseKey:

        pop dx
        pop cx
        pop ax
        ret
    parseKey endp

    checkChar proc
    ; entry: DL = char to check
        cmp dl, 30h
        jl invalidChar
        cmp dl, 7Ah
        jg invalidChar
        ; check if digit
        cmp dl, 3Ah
        jl endcheckChar
        ; check if lowercase letter 
        cmp dl, 60h
        jg endcheckChar

        invalidChar:
            mov dx, offset keyCharErr
            call printError

        endcheckChar:
        ret
    checkChar endp

    convertChar proc
    ; entry: DL = char to convert
    ; return: AL = numerical value
    
        mov al, dl
        sub al, 30h
        cmp al, 9h
        jle endConvertChar
        sub al, 27h
        
        endConvertChar:
        ret    
    convertChar endp
    
    checkVersion proc
        push ax
        push bx
        push dx
        
        ; check if version number is 0 or 1
        xor dl, dl
        call getArg
        ; AL = version number offset
        mov bl, al
        ; read version number
        mov al, byte ptr ds:[bx]
        ; get numerical value from character
        sub al, 30h
        ; check if 0
        cmp al, 1h
        jle saveVersion 

        mov dx, offset versionErr
        call printError
        
        saveVersion:
            mov runV, al

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
            mov byte ptr ds:[di], '$'
            inc di
            ; increment argument counter
            inc bl
            
            ret
    readArg endp        
    
    putAddress proc
        push bx
        
        ; bl contains number of arguments
        xor bh, bh        
        mov word ptr ds:[argPtr + bx], di
        
        pop bx
        ret
    putAddress endp
    
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
    ; return: AL = argument length
    
        push bx   
        
        ; clear address storage
        xor bx, bx
        ; dl stores argument index
        call getArg
        mov bl, al
        mov al, 1h
        getChar:
            ; go to next character 
            inc bx
            ; check for end of argument
            cmp byte ptr ds:[bx], '$'
            
            ; return if encountered end of string
            je endGetArgLen
            ; else increment character counter and read next
            inc al
            jmp getChar
        
        endGetArgLen:
        
        pop bx
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
            call println
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
    
    fingerprint proc
        push cx
        push dx
        call crlf
        mov dx, offset frame
        call println
        
        xor cl, cl
        rowLoop:
            mov dl, cl
            call printRow
            inc cx
            cmp cl, sizeY
            jl rowLoop
        
        call crlf
        mov dx, offset frame
        call println 
        pop cx
        pop dx
        ret
    fingerprint endp

    printRow proc
    ; entry: DL = row index
        push ax
        push bx
        mov al, dl
        push dx
        
        call crlf
        mov dl, '|'
        call printChar
        xor dh, dh
        mov dl, al
        call getPosition
        
        printASCII:
            mov dl, [array + bx]
            cmp dl, MAX_STEPS 
            jle convertASCII
            
            mov dl, MAX_STEPS
        
        convertASCII:
            cmp bl, startPos
            je startMarker
            cmp bl, endPos
            je endMarker
            push bx    
            xor bh, bh
            mov bl, dl
            mov dl, [ASCII + bx]
            pop bx
            jmp printMarker
        startMarker:
            mov dl, "S"
            jmp printMarker
        endMarker:
            mov dl, "E"    
        printMarker:        
            call printChar
            inc dh 
            inc bx
            cmp dh, sizeX
            jl printASCII
         
        mov dl, '|'
        call printChar 
         
        pop dx
        pop bx
        pop ax
        ret
    printRow endp
    
    println proc
    ; entry: DX = string offset
        push ax
        mov ah, 9h
        int 21h
        pop ax
        ret
    println endp
    
    printChar proc
    ; entry: DL = char to print
        push ax
        mov ah, 2h
        int 21h
        pop ax
        ret
    printChar endp

    printError proc
    ; entry: DX = error offset
        call println
        call exit
        ret
    printError endp
    
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


stack1 segment stack
        dw 200 dup(?)
    top dw ?
stack1 ends

end start