; Author: OneWayGondola
; Last Modified: 3/15/2020
; Course number/section: CS 271/400
; Project Number: 6
; Description: This program has procedures ReadVal and WriteVal, macros
;              getString and displayString. ReadVal invokes getString to get
;              user's string of digits, then converts the digit string to
;              numeric, while validating user's input. WriteVal converts numeric
;              to string of digits and invokes displayString to display results.
;              getString displays a prompt, then get's user's keyboard input in
;              to memory location. displayString prints the string stored in a
;              specified memory location.

INCLUDE Irvine32.inc
ARRAYSIZE = 10          ; number of elements in the array

;-------------------------------------------------------------------------------
; Macro getString displays a prompt, then stores user's keyboard input into a
; memory location
; Receives: offset of string prompt, offset of buffer, length of buffer
; Returns: the buffer will contain the input
; Preconditions: n/a
; Postconditions: n/a
; Registers changed: registers saved
;-------------------------------------------------------------------------------
getString MACRO aPrompt, bufferOffset, lenOfBuffer
    push    edx
    push    ecx
    
    mov     edx, aPrompt
    call    WriteString
    
    mov     edx, bufferOffset
    mov     ecx, lenOfBuffer
    call    ReadString
    
    pop     ecx
    pop     edx
ENDM

;-------------------------------------------------------------------------------
; Macro displayString prints string stored in specified memory location
; Receives: offset of string to be displayed
; Returns: n/a
; Preconditions: n/a
; Postconditions: n/a
; Registers changed: registers saved
;-------------------------------------------------------------------------------
displayString MACRO stringAddress
    push    edx
    
    mov     edx, stringAddress
    call    WriteString
    
    pop     edx
ENDM

.data
programTitle    BYTE    "PROGRAMMING ASSIGNMENT 6: Designing low-level I/O "
                BYTE    "procedures", 0
author          BYTE    "Written by Christopher Vu", 0
description     BYTE    "Please provide 10 signed decimal integers.", 0Dh, 0Ah
                BYTE    "Each number needs to be small enough to fit inside a "
                BYTE    "32 bit register.", 0Dh, 0Ah
                BYTE    "After you have finished inputting the raw numbers, I "
                BYTE    "will display a list", 0Dh, 0Ah
                BYTE    "of the integers, their sum, and their "
                BYTE    "average value.", 0
prompt          BYTE    "Please enter a signed number: ", 0
error           BYTE    "ERROR: You did not enter a signed number, or your "
                BYTE    "value was too big.", 0
reprompt        BYTE    "Please try again: ", 0
outputMsg       BYTE    "Entered Numbers: ", 0
outputSpace     BYTE    ", ", 0
displaySum      BYTE    "Sum: ", 0
displayAvg      BYTE    "Rounded Average: ", 0
goodbye         BYTE    "Until we meet again.", 0

; This array will store the valid numbers
array           SDWORD  ARRAYSIZE DUP(?)

.code
main PROC

    push    OFFSET programTitle
    push    OFFSET author
    push    OFFSET description
    call    introduction

    push    OFFSET array        ; array to be filled with valid signed ints
    push    LENGTHOF array
    push    OFFSET prompt
    push    OFFSET error        ; Error messge
    push    OFFSET reprompt     ; Prompt again if error
    call    getUserInput        ; Fills array with valid numbers

    push    OFFSET array
    push    LENGTHOF array
    push    OFFSET outputMsg    ; label for the array being displayed
    push    OFFSET outputSpace  ; spacing between terms displayed
    call    displayValues

    push    OFFSET array
    push    LENGTHOF array
    push    OFFSET displaySum
    push    OFFSET displayAvg
    call    displaySumAvg

    push    OFFSET goodbye
    call    farewell

    exit
main ENDP

;-------------------------------------------------------------------------------
; Procedure introduction introduces information about the program
; Receives: offset of DWORD array, lengthof array, prompt, error message,
;           error prompt
; Returns: n/a
; Preconditions: n/a
; Postconditions: n/a
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
introduction PROC USES eax
    enter 0, 0

    displayString [ebp + 20]
    call    Crlf

    displayString [ebp + 16]
    call    Crlf
    call    Crlf

    displayString [ebp + 12]
    call    Crlf
    call    Crlf
    
    leave
    ret     12
introduction ENDP

;-------------------------------------------------------------------------------
; Procedure getUserInput calls readVal which get input and validates it, fills
;           array
; Receives: offset of array, length of array
; Returns: fills the array with 10 signed integers
; Preconditions: dword arrays, 32 bit signed integers
; Postconditions: dword arrays, 32 bit signed integers
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
getUserInput PROC USES esi ecx
    enter 0, 0

    mov     esi, [ebp + 32]             ; array address
    mov     ecx, [ebp + 28]             ; loop for the length of the array

readInputArray:
    push    [ebp + 24]                  ; 16    prompt message
    push    [ebp + 20]                  ; 12    error message
    push    [ebp + 16]                  ; 8     error prompt
    call    readVal                     ; get's user keyboard input, validates
    
    pop     [esi]                       ; store valid value in array
    add     esi, 4                      ; increment array index
    loop    readInputArray

    leave
    ret     20
getUserInput ENDP

;-------------------------------------------------------------------------------
; Procedure readVal invokes getString macro to get the user's string of digits,
; then converts the digit string to numeric, while validating user's input
; Receives: offset of 3 strings
; Returns: one value on stack
; Preconditions: dword arrays, 32 bit signed integers
; Postconditions: dword arrays, 32 bit signed integers
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
readVal PROC USES eax ebx edx esi ecx
    LOCAL inputString[12]:BYTE, isValid:DWORD

    mov     eax, [ebp + 16]             ; prompt
readLoop:
    lea     ebx, inputString

    getString eax, ebx, LENGTHOF inputString
    
    push    [ebp + 12]                  ; 20    error message
    lea     eax, isValid
    push    eax                         ; 16    flag for input validity
    lea     eax, inputString
    push    eax                         ; 12    inputString address
    push    LENGTHOF inputString        ; 8     length of input
    call    validate

    pop     edx
    mov     [ebp + 16], edx
    mov     eax, isValid
    cmp     eax, 1
    je      exit_readVal                ; equal if valid

    mov     eax, [ebp + 8]              ; try again message
    jmp     readLoop
    
exit_readVal:
    ret     8
readVal ENDP

;-------------------------------------------------------------------------------
; Procedure validates that the input is a string of digits, then calls
; convertToInt to convert the string over to number and to check for 32 bit size
; If string is negative, sets isNegative flag to pass to converToInt. The signs
; are removed from the strings here.
; Receives: address of string, isValid from previous procedure, inputString
;           address, length of the input string
; Returns: sets or clears isValid flag from calling procedure, top of stack will
;          contain valid number or 0
; Preconditions: dword arrays, 32 bit signed integers
; Postconditions: dword arrays, 32 bit signed integers, signs removed
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
validate PROC USES esi ecx eax edx
    LOCAL valueAboveLimit:DWORD, isNegative:DWORD


; Initialize isNegative to 0
    lea     eax, isNegative
    mov     edx, 0
    mov     [eax], edx

    mov     esi, [ebp + 12]
    mov     ecx, [ebp + 8]
    cld

loadString:
    lodsb

                                    ; Checks if it is first character
    mov     edx, [ebp + 12]         ; (address of array) + 1
    add     edx, 1

    cmp     esi, edx                ; if esi is equal to (address of array) + 1
    je      checkForSign            ; it's the first element, check for sign.
;-------------------------------------------------------------------------------

checkForDigit:
    cmp     al, 0
    je      convertStrToInt
    cmp     al, 48
    jb      invalid
    cmp     al, 57
    ja      invalid
    loop    loadString


; Checks first character for sign, if negative, set isNegative, remove sign
; if positive: remove sign
; if not sign: return to check for Digit
checkForSign:
    cmp     al, 45
    je      setIsNegative
    cmp     al, 43
    je      removeSignFromString
    jmp     checkForDigit

setIsNegative:
    lea     eax, isNegative
    mov     edx, 1
    mov     [eax], edx

removeSignFromString:
    mov     al, 48
    mov     edi, [ebp + 12]
    stosb
    jmp     loadString
;-------------------------------------------------------------------------------

invalid:
    mov     edx, [ebp + 20]         ; invalid message
    displayString edx
    call    Crlf

    mov     edx, [ebp + 16]
    mov     eax, 0
    mov     [edx], eax              ; set isValid to 0
    jmp     finalValue

convertStrToInt:
    mov     edx, [ebp + 8]          ; length of input array
    cmp     ecx, edx                ; if ecx is equal, that means the string is empty
    je      invalid

    lea     eax, valueAboveLimit
    mov     edx, 0
    mov     [eax], edx              ; initialize valueAboveLimit to False

    lea     edx, isNegative

    push    edx                     ; 20    address of isNegative
    push    [ebp + 12]              ; 16    address of input string
    push    [ebp + 8]               ; 12    length of input array
    push    eax                     ; 8     valueAboveLimit
    call    convertToInt

    mov     edx, valueAboveLimit    ; check if valueAboveLimit flag set
    cmp     edx, 1
    je      invalid                 ; greater than 32 bit is invalid

    mov     edx, [ebp + 16]         ; set isValid to 1
    mov     eax, 1
    mov     [edx], eax

finalValue:
    pop     edx                     ; return value on stack
    mov     [ebp + 20], edx
    ret     12
validate ENDP

;-------------------------------------------------------------------------------
; Procedure convertToInt converts the number to integer while checking 32 bit
; limit. Checks if number is supposed to negative too as was determined from
; calling procedure
; Receives: isNegative DWORD flag from calling procedure, address of inputstring
;           length of input string, valueAboveLimit flag from call
; Returns: sets or clears valueAboveLimit, limit being must fit in 32 bit signed
;          integer, returns the value on stack
; Preconditions: dword arrays, 32 bit signed integers
; Postconditions: dword arrays, 32 bit signed integers, sets flag if value too
;                 great
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
convertToInt PROC USES esi ecx eax ebx edx
    LOCAL value:SDWORD

    mov     esi, [ebp + 16]     ; address of input string
    mov     ecx, [ebp + 12]     ; initialize counter to length of inputstring
    
    lea     eax, value
    xor     ebx, ebx            ; sets all bits to 0 in value
    mov     [eax], ebx

    xor     eax, eax            ; sets all bits to 0
    xor     edx, edx

    cld

insertDigits:
    lodsb

    cmp     eax, 0              ; reached null, check if the sign supposed to be
    je      isSignNegative      ; negative

    sub     eax, 48
    mov     ebx, eax            ; (str[k]-48)
    
    mov     eax, value          ; x
    mov     edx, 10             ; 10
    
    imul    edx                 ; (10*x)
    jo      overflow

    add     eax, ebx            ; (10*x)+(str[k]-48)
    jo      overflow

updateValue:
    mov     value, eax
    xor     eax, eax            ; set all eax bits to 0
    loop    insertDigits

overflow:
    cmp     ecx, 2              ; checks for minimum 32 bit value if at last digit
    jne     continue
    
    cmp     eax, 10000000000000000000000000000000b
    je      updateValue

continue:
    mov     ebx, [ebp + 8]      ; set valueAboveLimit
    mov     eax, 1
    mov     [ebx], eax

    xor     eax, eax            ; clear eax
    mov     [ebp + 20], eax     ; push 0 to stack
    jmp     exit_convertToInt

isSignNegative:
    ; This is for the case of minimum 32 bit signed integer
    mov     eax, value
    cmp     eax, 10000000000000000000000000000000b
    je      returnVal
    
    ; if this number is supposed to be negative
    mov     eax, [ebp + 20]
    mov     ebx, 1
    cmp     [eax], ebx
    je      itIsNegative

    mov     eax, value
    jmp     returnVal

; get two's comp if so
itIsNegative:
    mov     eax, value
    mov     ebx, 11111111111111111111111111111111b
    xor     eax, ebx
    add     eax, 1

returnVal:
    mov     [ebp + 20], eax     ; return value on stack

exit_convertToInt:
    ret     12
convertToInt ENDP

;-------------------------------------------------------------------------------
; Procedure displayValues prints value from array. Calls WriteVal to print
; individual values.
; Receives: array offset, length of array, string message for output, output
;           spacing as string
; Returns: prints the values in array
; Preconditions: dword arrays, 32 bit signed integers
; Postconditions: dword arrays, 32 bit signed integers
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
displayValues PROC USES esi ecx ebx edx
    enter 0, 0

    call    Crlf

    mov     edx, [ebp + 28]         ; display output label
    displayString edx
    call    Crlf

    mov     esi, [ebp + 36]         ; offset of array
    mov     ecx, [ebp + 32]         ; lengthof array
    mov     ebx, 1                  ; initialize counter

printValue:
    push    [esi]                   ; push array index
    call    WriteVal                ; prints the value
    add     esi, 4                  ; increment index

    cmp     ebx, [ebp + 32]         ; compare the counter to the array length
    jge     end_displayValues       ; if the counter is greater than or equal to
                                    ; the array length, exit

    mov     edx, [ebp + 24]         ; output spacing
    displayString edx

    inc     ebx
    loop    printValue              ; print next value

end_displayValues:
    call    Crlf

    leave
    ret     16
displayValues ENDP

;-------------------------------------------------------------------------------
; Procedure writeVal takes integer element from array, calls convertToString,
; prints resulting string from integer to string conversionl.
; Receives: Takes the integer elements from array.
; Returns: prints value from array
; Preconditions: dword arrays, 32 bit signed integers
; Postconditions: dword arrays, 32 bit signed integers
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
writeVal PROC USES eax
    LOCAL intString[12]:BYTE

    lea     eax, intString
    push    eax
    push    [ebp + 8]               ; array index
    call    convertToString

    lea     eax, intString          ; prints element
    displayString eax

    ret     4
writeVal ENDP

;-------------------------------------------------------------------------------
; Procedure convertToString takes an integer element from array then converts to
; string and passes back on stack.
; Receives: integer value from writeVal
; Returns: string on stack back to callign procedure
; Preconditions: dword arrays, 32 bit signed integers
; Postconditions: dword arrays, 32 bit signed integers
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
convertToString PROC USES eax ebx ecx
    LOCAL char:DWORD, isNeg:DWORD

    lea     eax, isNeg          ; initialize isNeg to False
    mov     ebx, 0
    mov     [eax], ebx

    mov     eax, [ebp + 8]      ; array element
    mov     ebx, 10
    mov     ecx, 0
    cld

    ; is the element from the array negative
    cmp     eax, 0
    jl      negativeNumber
    jmp     divideByTen

negativeNumber:
    pushad
    lea     eax, isNeg          ; the number is neg, so set isNeg
    mov     ebx, 1
    mov     [eax], ebx
    popad

    ; make it positive to be processed
    not     eax
    inc     eax

divideByTen:
    cdq
    idiv    ebx                     ; divide the element by 10
    
    push    edx                     ; put the remainder on the stack
    
    inc     ecx
    cmp     eax, 0                  ; compare the quotient to 0
    jne     divideByTen             ; keep pushing remainders on the stack until
                                    ; the quotient is 0

    pushad
    lea     eax, isNeg              ; check if supposed to negative post convert
    mov     ebx, 1
    cmp     [eax], ebx
    popad
    jne     notNeg
    ; put the negative into the string first
    mov     edi, [ebp + 12]
    mov     al, 45
    stosb
    jmp     insertChar

notNeg:
    mov     edi, [ebp + 12]         ; address of string to be filled if not neg

insertChar:
    pop     char
    mov     al, BYTE PTR char
    add     al, 48                  ; add 48 by ascii
    stosb                           ; put back in to intString from call
    loop    insertChar
    mov     al, 0                   ; null
    stosb

    ret     8
convertToString ENDP

;-------------------------------------------------------------------------------
; Procedure displaySumAvg calculates the sum and average of the array then
; then displays it
; Receives: array offset, lengthof array, two string labels for sum and avg
; Returns: prints sum and average.
; Preconditions: rounded average based on multiple of 10, dword arrays, 32 bit
;                signed integers
; Postconditions: dword arrays, 32 bit signed integers
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
displaySumAvg PROC USES esi ecx edx eax ebx
    enter 0, 0

    mov     edx, [ebp + 32]     ; sum label
    displayString edx
    
    mov     esi, [ebp + 40]     ; array
    mov     ecx, [ebp + 36]     ; loop length of array
    xor     eax, eax            ; 0 eax for accumulation

calculateSum:
    add     eax, [esi]          ; add element from array to eax
    add     esi, TYPE SDWORD    ; increment by dword
    loop    calculateSum
    
    push    eax                 ; print the sum
    call    writeVal
    call    Crlf

    mov     edx, [ebp + 28]     ; average label
    displayString edx

    ;average
    cdq
    mov     ebx, [ebp + 36]     ; elements in array
    idiv    ebx                 ; average value in eax

    mov     ebx, 5              ; if remainder 5 or greater, round up 1
    cmp     edx, ebx
    jge     roundUp
    jmp     ending

roundUp:
    add     eax, 1              ; round up

ending:
    push    eax
    call    writeVal            ;print the average
    call    Crlf

    leave
    ret     16
displaySumAvg ENDP

;-------------------------------------------------------------------------------
; Procedure farewell prints goodbye
; Receives: offset of goodbye string
; Returns: n/a
; Preconditions: n/a
; Postconditions: n/a
; Registers changed: registers saved and recovered
;-------------------------------------------------------------------------------
farewell PROC USES eax
    enter 0, 0
    
    call    Crlf
    mov     eax, [ebp + 12]
    displayString eax
    call    Crlf

    leave
    ret 4
farewell ENDP

END main
