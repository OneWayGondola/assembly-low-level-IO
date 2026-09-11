# assembly-low-level-IO

One program, two editions. It reads ten signed 32-bit decimal integers from
the keyboard, one per line, refusing anything that is not one; then prints
them back, their sum and their rounded average. Every character in and out
goes through code in this repository — no `printf`, no `scanf`.

| | `low-level-IO.asm` (2020) | `lowio.s` (2026) |
|---|---|---|
| Target | 32-bit Windows, MASM, Irvine32 library | x86-64 Linux, GNU `as`/`ld`, no library at all |
| I/O | Irvine32 `ReadString` / `WriteString` (which call the Win32 console API) | `read`, `write`, `exit` system calls |
| Verified | Hand-traced only (see the audit below) | `python3 test_lowio.py`: byte-exact against a model, 0 failures |
| Origin | Oregon State CS 271, Project 6, March 2020 | Rewrite, September 2026 |

The 2020 file is unchanged. It is the assignment as submitted.

## 2026 edition

```
make          # as --fatal-warnings + ld  ->  ./lowio (static, ~10 KB)
make run
make test     # builds, then runs the harness
```

Needs GNU binutils and Python 3 on x86-64 Linux. Nothing else.

What it does that the original does not:

- **Grammar is exact**: `[+-]?[0-9]+`, value in `[-2147483648, 2147483647]`.
  `+2147483648`, `02147483648`, `-2147483649`, `1 2`, `1.0`, `""`, `+`, `-`
  are all refused.
- **INT32_MIN is not a special case.** The magnitude accumulates in 64 bits,
  capped at 2^31 after each digit; the sign then decides whether it is in
  range. Twenty-digit input cannot overflow the accumulator.
- **A line longer than the buffer (63 characters) is refused and drained**,
  so the next prompt starts on the next line of input. The original
  truncated silently and accepted what was left.
- **Sum in 64 bits**; ten int32 values cannot overflow it.
- **Average rounds half away from zero, negatives included**: `-19/10 → -2`.
- **End of input is an exit code**: message on stderr, exit 1. A script that
  pipes input in can tell a finished run from a starved one.
- **`write_int` prints INT32_MIN correctly** (unsigned divide on the negated
  64-bit value).
- **Every procedure states what it clobbers** and uses the System V AMD64
  convention, so the reader knows the rules without reading the body.

### What the tests hold it to

`test_lowio.py` builds the binary, checks the ELF has no `INTERP`, `NEEDED`
or `DYNAMIC` entry (no loader, no libc), then drives it through a pipe and
compares stdout, stderr and exit code byte-for-byte against a Python model
of the specification:

1. ten plain values, output layout exact;
2. every int32 boundary, both signs, with and without `+`;
3. 22 rejection cases, each followed by a recovery;
4. leading zeros up to the 63-character line limit;
5. a 63-character line fits, a 64-character line does not;
6. 14 rounding cases for the average, including `INT_MAX*10` and `INT_MIN*10`;
7. sum overflow and underflow of 32 bits;
8. end of input: empty, partial, after a rejection;
9. CRLF line endings; a final line with no newline;
10. 300 seeded random runs across the whole range with junk lines mixed in.

Result, 2026-09-11, binutils 2.42, Python 3.12, Linux x86-64: **0 failures**.
The oracle was wrong twice while writing it (prompt-before-EOF order; a test
line one byte too long) and the binary zero times; both fixes are in the
history.

## Audit of the 2020 original

Read in full on 2026-09-11. It could not be executed here — MASM and
Irvine32 are Windows-only — so every finding below is a hand trace with
line numbers into `low-level-IO.asm`, not an observed run.

### What it does well

- Every procedure has a header: receives, returns, pre/postconditions,
  registers changed. Most student assembly has none.
- Parameters go on the stack, procedures use `USES` and `LOCAL`; the only
  global written is the array, and it is passed by address. The code is
  re-entrant in shape.
- Overflow is detected with the hardware flag (`jo`, lines 361 and 364), not
  a magnitude compare. That is the right instinct.
- INT32_MIN was thought about at all (lines 371–376, 387–391).
- Sign removal in place (lines 272–276) makes the digit loop uniform.
- Macros only wrap I/O; logic lives in procedures. That is what the
  assignment asked for.

### What is wrong

1. **`+2147483648` and `02147483648` are accepted and stored as -2147483648.**
   `validate` replaces a leading sign with `'0'`
   (272–276), so both become the 11-character string `02147483648`. In
   `convertToInt` the last digit overflows (364); the check at 371–376 asks
   only "is this the last character, and is the result 0x80000000?" — it
   never asks whether a minus sign was seen. `isSignNegative` (387–391) then
   returns 0x80000000 unconditionally. Any 11-character digit string whose
   value is exactly 2^31 becomes INT32_MIN regardless of sign. A bare
   `2147483648` (10 characters) is correctly refused only because `ecx` is 3,
   not 2, at the overflowing digit.

2. **INT32_MIN prints as punctuation.** `convertToString` negates with
   `not`/`inc` (518–519), which leaves 0x80000000 unchanged and negative;
   `cdq`/`idiv` (522–523) then yields negative remainders, and `add al, 48`
   (550) lands below `'0'`. Trace: `-./,),(-*,(`. The input side goes to
   lengths to accept INT32_MIN; the output side cannot print it.

3. **Long input is truncated and the stub accepted.** `ReadString` is called
   with `ecx = 12` (187); the library keeps at most 11 characters and
   ignores the rest (Irvine32.asm, `ReadString` header: "If the user types
   more characters than (ECX-1), the excess characters are ignored"). So
   `000000000001` reads as `00000000000` → 0, valid, no error.

4. **Negative averages truncate instead of rounding.** `cmp edx, 5; jge
   roundUp` (596–598): after `idiv` the remainder carries the dividend's
   sign, so a negative remainder never rounds. Sum -19 prints -1. The
   constant 5 is also half of 10 hard-coded; the header admits it
   ("rounded average based on multiple of 10", 564).

5. **An unbalanced `pop` on the error paths.** `validate` ends every path at
   `finalValue: pop edx` (315). On the valid path that pops the slot
   `convertToInt` left behind; on the non-digit path (279) and the
   empty-string path (292) nothing was left, so it pops the first
   `USES`-saved register. The epilogue's four `pop`s are then off by one —
   `esi`, `ecx`, `eax`, `edx` return holding each other's values — and only
   the `leave` MASM emits for `LOCAL` puts `esp` right. It is masked because
   `readVal` reloads `eax` and never reads `esi`/`ecx` before its own
   epilogue restores them. Harmless today, and a trap for the next edit.

6. **"Registers changed: registers saved and recovered" is not true.**
   `edi` is written in `validate` (274–275) and `convertToString` (539,
   545, and the `stosb`s that follow) and saved by neither. `edx` is
   destroyed by `cdq`/`idiv` in `convertToString`, whose `USES` list omits
   it (493), so `writeVal` (`USES eax`, 470) returns with `edx` changed. `getString` (26) claims the
   same while `ReadString` returns its count in `eax`. Every caller happens
   to reload before use; the headers say otherwise.

7. **Return values travel in a parameter slot.** `readVal` takes 12 bytes of
   arguments, does `ret 8` (207), and leaves its result in what was the
   prompt slot for the caller to `pop` (163). `validate` and `convertToInt`
   do the same (317, 413). It works; it is documented nowhere but at the
   call sites, and it is the reason for finding 5 — the stack shape differs
   by path, so a single `pop` cannot be right on all of them.

8. **The 32-bit sum has no overflow check** (580). Ten values of 2147483647
   print as -10. Whether the assignment required the sum to fit is not in
   the repository.

9. **Two loop counters in `displayValues`** (`ebx` 437–451 and `loop` on
   `ecx` 452); `ebx` does the work, `loop` never terminates the loop.

10. **`introduction`'s header (117–118) describes `getUserInput`'s
    parameters.** Copy-paste.

11. **Magic numbers for characters** — `45`, `43`, `48`, `57` (250–273,
    550) where `'-'`, `'+'`, `'0'`, `'9'` would read.

Findings 1–4 change what the user sees. Findings 5–7 are the same fault
seen three ways: the calling convention is improvised per procedure, the
headers describe an ideal rather than the code, and correctness rests on
callers happening not to depend on what was clobbered. The 2026 edition
removes that class of fault by using one published convention and stating
clobbers per procedure, then proves the visible behaviour with the oracle.
