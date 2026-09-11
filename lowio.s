# lowio.s — Designing low-level I/O procedures, x86-64 Linux edition (2026).
#
# Reads ten signed 32-bit decimal integers from standard input, one per line,
# validating each; prints them back, their sum and their rounded average.
# No C library, no Irvine32: the only outside calls are the read, write and
# exit system calls.  The 2020 original (low-level-IO.asm, MASM + Irvine32,
# 32-bit Windows) sits beside this file untouched.
#
#   build:  as -o lowio.o lowio.s && ld -o lowio lowio.o
#   test:   python3 test_lowio.py
#
# Register convention is System V AMD64, the one every Linux compiler uses:
# arguments in rdi, rsi, rdx, rcx; result in rax (rdx where noted); rbx, rbp,
# r12-r15 belong to the caller and are preserved; everything else is scratch.
# `syscall` itself clobbers rcx and r11.  Each procedure states what it
# clobbers, and the tests hold it to that.

        .intel_syntax noprefix

        .equ    SYS_READ,   0
        .equ    SYS_WRITE,  1
        .equ    SYS_EXIT,   60
        .equ    STDIN,      0
        .equ    STDOUT,     1
        .equ    STDERR,     2

        .equ    ARRAYSIZE,  10              # numbers to read
        .equ    LINEMAX,    64              # line buffer: 63 characters + NUL
        .equ    INT32_MAG,  0x80000000      # |INT32_MIN|, the magnitude cap
        .equ    NUMBUF,     24              # room for "-9223372036854775808"

# ---------------------------------------------------------------------------
        .section .rodata
title:      .asciz "PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures\n"
author:     .asciz "Written by Christopher Vu\n\n"
description:
            .ascii "Please provide 10 signed decimal integers.\n"
            .ascii "Each number needs to be small enough to fit inside a 32 bit register.\n"
            .ascii "After you have finished inputting the raw numbers, I will display a list\n"
            .asciz "of the integers, their sum, and their average value.\n\n"
prompt:     .asciz "Please enter a signed number: "
reprompt:   .asciz "Please try again: "
errmsg:     .asciz "ERROR: You did not enter a signed number, or your value was too big.\n"
listhdr:    .asciz "\nEntered Numbers: \n"
sep:        .asciz ", "
sumlbl:     .asciz "Sum: "
avglbl:     .asciz "Rounded Average: "
goodbye:    .asciz "\nUntil we meet again.\n"
newline:    .asciz "\n"
eofmsg:     .ascii "\nERROR: input ended before 10 numbers were read.\n"
        .equ    EOFMSG_LEN, . - eofmsg

        .section .bss
array:      .space  ARRAYSIZE * 4           # the validated values, int32 each
line:       .space  LINEMAX                 # one line of input
numbuf:     .space  NUMBUF                  # scratch for write_int

# ---------------------------------------------------------------------------
        .text
        .globl  _start
_start:
        lea     rdi, [rip + title]
        call    write_str
        lea     rdi, [rip + author]
        call    write_str
        lea     rdi, [rip + description]
        call    write_str

        # array[rbx] = read_val()  for rbx in 0 .. ARRAYSIZE-1
        xor     ebx, ebx
.Lfill:
        lea     rdi, [rip + prompt]
        lea     rsi, [rip + reprompt]
        lea     rdx, [rip + errmsg]
        call    read_val
        lea     rcx, [rip + array]
        mov     dword ptr [rcx + rbx*4], eax
        inc     ebx
        cmp     ebx, ARRAYSIZE
        jb      .Lfill

        # The values, comma-separated, on one line.
        lea     rdi, [rip + listhdr]
        call    write_str
        xor     ebx, ebx
.Lshow:
        lea     rcx, [rip + array]
        movsxd  rdi, dword ptr [rcx + rbx*4]
        call    write_int
        inc     ebx
        cmp     ebx, ARRAYSIZE
        jae     .Lshown
        lea     rdi, [rip + sep]
        call    write_str
        jmp     .Lshow
.Lshown:
        lea     rdi, [rip + newline]
        call    write_str

        # The sum, in 64 bits: ten int32 values need at most 35 of them.
        xor     r12d, r12d
        xor     ebx, ebx
.Lsum:
        lea     rcx, [rip + array]
        movsxd  rax, dword ptr [rcx + rbx*4]
        add     r12, rax
        inc     ebx
        cmp     ebx, ARRAYSIZE
        jb      .Lsum

        lea     rdi, [rip + sumlbl]
        call    write_str
        mov     rdi, r12
        call    write_int
        lea     rdi, [rip + newline]
        call    write_str

        # The average, rounded to the nearest integer, halves away from zero.
        lea     rdi, [rip + avglbl]
        call    write_str
        mov     rdi, r12
        mov     esi, ARRAYSIZE
        call    round_div
        mov     rdi, rax
        call    write_int
        lea     rdi, [rip + newline]
        call    write_str

        lea     rdi, [rip + goodbye]
        call    write_str

        mov     eax, SYS_EXIT
        xor     edi, edi
        syscall

# ---------------------------------------------------------------------------
# read_val(rdi = prompt, rsi = re-prompt, rdx = error message)
#   Prompt, read a line, parse it.  On a bad line: print the error, ask again
#   with the re-prompt.  Returns eax = the value.  End of input is fatal and
#   goes to eof_exit.  Clobbers rax, rcx, rdx, rsi, rdi, r8-r11.
read_val:
        push    r12
        push    r13
        push    r14
        mov     r12, rdi                    # the prompt in force
        mov     r13, rsi                    # the re-prompt
        mov     r14, rdx                    # the error message
.Lrv_again:
        mov     rdi, r12
        call    write_str
        lea     rdi, [rip + line]
        mov     esi, LINEMAX
        call    read_line
        cmp     rax, -1
        je      eof_exit
        cmp     rax, -2
        je      .Lrv_bad
        lea     rdi, [rip + line]
        call    parse_int
        test    edx, edx
        jz      .Lrv_ok
.Lrv_bad:
        mov     rdi, r14
        call    write_str
        mov     r12, r13                    # from here on, the re-prompt
        jmp     .Lrv_again
.Lrv_ok:
        pop     r14
        pop     r13
        pop     r12
        ret

# ---------------------------------------------------------------------------
# read_line(rdi = buffer, rsi = capacity)
#   One line from stdin, read a byte at a time, stored without its newline
#   and NUL-terminated; one trailing CR is dropped.  Returns rax = length;
#   -1 at end of input with nothing read; -2 when the line does not fit, in
#   which case the rest of that line is consumed so the next call starts
#   clean (the 2020 original truncated silently and accepted the stub).
#   Clobbers rax, rcx, rdx, rsi, rdi, r8-r11.
read_line:
        mov     r8, rdi                     # buffer
        mov     r9, rsi                     # capacity
        xor     r10d, r10d                  # bytes stored so far
        sub     rsp, 8                      # scratch for the byte just read
.Lrl_next:
        mov     eax, SYS_READ
        mov     edi, STDIN
        mov     rsi, rsp
        mov     edx, 1
        syscall
        test    rax, rax
        jle     .Lrl_eof                    # 0 = end of input; <0 = error, same
        movzx   eax, byte ptr [rsp]
        cmp     al, 10
        je      .Lrl_end
        lea     rcx, [r9 - 1]
        cmp     r10, rcx
        jae     .Lrl_drain                  # no room for this byte and a NUL
        mov     byte ptr [r8 + r10], al
        inc     r10
        jmp     .Lrl_next
.Lrl_drain:                                 # the line is void: eat the rest
        mov     eax, SYS_READ
        mov     edi, STDIN
        mov     rsi, rsp
        mov     edx, 1
        syscall
        test    rax, rax
        jle     .Lrl_toolong
        cmp     byte ptr [rsp], 10
        jne     .Lrl_drain
.Lrl_toolong:
        mov     byte ptr [r8], 0
        mov     rax, -2
        add     rsp, 8
        ret
.Lrl_eof:
        test    r10, r10
        jnz     .Lrl_end                    # a last line with no newline counts
        mov     byte ptr [r8], 0
        mov     rax, -1
        add     rsp, 8
        ret
.Lrl_end:
        test    r10, r10
        jz      .Lrl_term
        cmp     byte ptr [r8 + r10 - 1], 13
        jne     .Lrl_term
        dec     r10                         # Windows line ending: drop the CR
.Lrl_term:
        mov     byte ptr [r8 + r10], 0
        mov     rax, r10
        add     rsp, 8
        ret

# ---------------------------------------------------------------------------
# parse_int(rdi = NUL-terminated string)
#   The grammar is  [+-]?[0-9]+  and nothing else; the value must fit a
#   signed 32-bit integer.  Returns rax = value (sign-extended to 64 bits)
#   and rdx = 0; or rdx = 1 and rax = 0 when the string is not such a number.
#   The magnitude is accumulated in 64 bits and capped at 2^31 after every
#   digit, so a long string of digits can neither overflow nor sneak through.
#   INT32_MIN needs no special case: 2147483648 is under the cap, then the
#   sign decides whether it is in range.  Clobbers rax, rcx, rdx, rsi, rdi,
#   r8, r9.
parse_int:
        xor     r8d, r8d                    # 1 when a '-' was seen
        xor     eax, eax                    # the magnitude
        xor     ecx, ecx                    # digits seen
        mov     r9d, INT32_MAG              # 64-bit compare needs it in a register:
                                            # an imm32 would sign-extend to negative
        movzx   esi, byte ptr [rdi]
        cmp     sil, '+'
        je      .Lpi_sign
        cmp     sil, '-'
        jne     .Lpi_digit
        mov     r8d, 1
.Lpi_sign:
        inc     rdi
        movzx   esi, byte ptr [rdi]
.Lpi_digit:
        test    sil, sil
        jz      .Lpi_end
        sub     sil, '0'
        cmp     sil, 9
        ja      .Lpi_bad                    # unsigned: anything below '0' wraps high
        imul    rax, rax, 10
        add     rax, rsi
        cmp     rax, r9
        ja      .Lpi_bad                    # past |INT32_MIN|: cannot recover
        inc     ecx
        inc     rdi
        movzx   esi, byte ptr [rdi]
        jmp     .Lpi_digit
.Lpi_end:
        test    ecx, ecx
        jz      .Lpi_bad                    # "", "+", "-": no digits
        test    r8d, r8d
        jnz     .Lpi_neg
        cmp     rax, r9
        jae     .Lpi_bad                    # +2147483648 is one too many
        xor     edx, edx
        ret
.Lpi_neg:
        neg     rax                         # magnitude <= 2^31: exact
        xor     edx, edx
        ret
.Lpi_bad:
        xor     eax, eax
        mov     edx, 1
        ret

# ---------------------------------------------------------------------------
# round_div(rdi = dividend, rsi = divisor > 0)
#   The quotient rounded to the nearest integer, halves away from zero:
#   15/10 -> 2, -15/10 -> -2, 14/10 -> 1, -14/10 -> -1.
#   Clobbers rax, rcx, rdx.
round_div:
        mov     rax, rdi
        cqo
        idiv    rsi                         # rax toward zero; rdx has the dividend's sign
        mov     rcx, rdx
        neg     rcx
        cmovl   rcx, rdx                    # rcx = |remainder|
        add     rcx, rcx                    # 2|r|
        cmp     rcx, rsi
        jb      .Lrd_done                   # 2|r| < divisor: keep the truncated quotient
        test    rdi, rdi
        js      .Lrd_neg
        inc     rax
        ret
.Lrd_neg:
        dec     rax
.Lrd_done:
        ret

# ---------------------------------------------------------------------------
# write_int(rdi = signed 64-bit value)
#   Print it in decimal.  Digits are produced backwards into numbuf with an
#   unsigned divide, so INT32_MIN (and even INT64_MIN) print correctly — the
#   2020 original printed INT32_MIN as punctuation.  Clobbers rax, rcx, rdx,
#   rsi, rdi, r8-r11.
write_int:
        lea     rsi, [rip + numbuf + NUMBUF]    # one past the end
        mov     rax, rdi
        xor     r8d, r8d                    # 1 when negative
        test    rax, rax
        jns     .Lwi_digits
        neg     rax
        mov     r8d, 1
.Lwi_digits:
        mov     ecx, 10
.Lwi_loop:
        xor     edx, edx
        div     rcx                         # rax /= 10, rdx = the digit
        add     dl, '0'
        dec     rsi
        mov     byte ptr [rsi], dl
        test    rax, rax
        jnz     .Lwi_loop
        test    r8d, r8d
        jz      .Lwi_out
        dec     rsi
        mov     byte ptr [rsi], '-'
.Lwi_out:
        lea     rdi, [rip + numbuf + NUMBUF]
        sub     rdi, rsi                    # length
        xchg    rdi, rsi                    # rdi = first char, rsi = length
        jmp     write_buf

# ---------------------------------------------------------------------------
# write_str(rdi = NUL-terminated string)
#   Print it on stdout.  Clobbers what write_buf does.
write_str:
        xor     esi, esi
.Lws_len:
        cmp     byte ptr [rdi + rsi], 0
        je      write_buf                   # tail call with rdi = start, rsi = length
        inc     rsi
        jmp     .Lws_len

# ---------------------------------------------------------------------------
# write_buf(rdi = bytes, rsi = count)
#   Write them all to stdout, continuing after a short write.  A write that
#   returns an error or zero ends the attempt; there is nothing better to do.
#   Clobbers rax, rcx, rdx, rsi, rdi, r8, r9, r11.
write_buf:
        mov     r8, rdi                     # next byte to write
        mov     r9, rsi                     # bytes remaining
.Lwb_loop:
        test    r9, r9
        jz      .Lwb_done
        mov     eax, SYS_WRITE
        mov     edi, STDOUT
        mov     rsi, r8
        mov     rdx, r9
        syscall
        test    rax, rax
        jle     .Lwb_done
        add     r8, rax
        sub     r9, rax
        jmp     .Lwb_loop
.Lwb_done:
        ret

# ---------------------------------------------------------------------------
# eof_exit
#   Standard input ran out before the array was full.  Say so on standard
#   error and exit 1, so a caller piping input in can tell this from a run
#   that finished.
eof_exit:
        mov     eax, SYS_WRITE
        mov     edi, STDERR
        lea     rsi, [rip + eofmsg]
        mov     edx, EOFMSG_LEN
        syscall
        mov     eax, SYS_EXIT
        mov     edi, 1
        syscall

        .section .note.GNU-stack,"",@progbits
