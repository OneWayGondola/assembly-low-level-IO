#!/usr/bin/env python3
"""Build lowio and hold it to a Python model of the specification.

The model is the oracle: it says, for any sequence of input lines, exactly
what the program must print, which values it must accept, and how it must
exit.  Every test drives the real binary through a pipe and compares bytes.
Run:  python3 test_lowio.py        (exit 0 = every check passed)
"""
import os
import pathlib
import random
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
BIN = HERE / "lowio"
N = 10
INT_MIN, INT_MAX = -2**31, 2**31 - 1
LINEMAX = 64  # must match lowio.s

INTRO = (
    "PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures\n"
    "Written by Christopher Vu\n\n"
    "Please provide 10 signed decimal integers.\n"
    "Each number needs to be small enough to fit inside a 32 bit register.\n"
    "After you have finished inputting the raw numbers, I will display a list\n"
    "of the integers, their sum, and their average value.\n\n"
)
PROMPT = "Please enter a signed number: "
REPROMPT = "Please try again: "
ERR = "ERROR: You did not enter a signed number, or your value was too big.\n"
EOFMSG = "\nERROR: input ended before 10 numbers were read.\n"


# --- the oracle ------------------------------------------------------------

def parse(line: bytes):
    """[+-]?[0-9]+ within int32, or None.  Mirrors read_line + parse_int."""
    if len(line) > LINEMAX - 1:
        return None                      # read_line: too long, drained, rejected
    if line.endswith(b"\r"):
        line = line[:-1]
    body = line[1:] if line[:1] in (b"+", b"-") else line
    if not body or any(c not in b"0123456789" for c in body):
        return None
    v = int(line)
    return v if INT_MIN <= v <= INT_MAX else None


def round_half_away(a: int, b: int) -> int:
    q, r = divmod(abs(a), b)
    if 2 * r >= b:
        q += 1
    return -q if a < 0 else q


def expected(lines):
    """(stdout, stderr, exit code, accepted values) for these input lines."""
    out = [INTRO]
    vals = []
    prompt = PROMPT
    i = 0
    while len(vals) < N:
        out.append(prompt)               # the prompt goes out before the read
        if i >= len(lines):
            return "".join(out), EOFMSG, 1, vals
        v = parse(lines[i])
        i += 1
        if v is None:
            out.append(ERR)
            prompt = REPROMPT
        else:
            vals.append(v)
            prompt = PROMPT
    total = sum(vals)
    out.append("\nEntered Numbers: \n" + ", ".join(map(str, vals)) + "\n")
    out.append(f"Sum: {total}\n")
    out.append(f"Rounded Average: {round_half_away(total, N)}\n")
    out.append("\nUntil we meet again.\n")
    return "".join(out), "", 0, vals


# --- the harness -----------------------------------------------------------

def build():
    subprocess.run(["as", "--fatal-warnings", "-o", "lowio.o", "lowio.s"],
                   cwd=HERE, check=True)
    subprocess.run(["ld", "-o", "lowio", "lowio.o"], cwd=HERE, check=True)


def run(stdin: bytes):
    p = subprocess.run([str(BIN)], input=stdin, capture_output=True, timeout=10)
    return p.stdout.decode(), p.stderr.decode(), p.returncode


failures = []


def check(name, lines, stdin=None):
    """Compare the binary against the oracle for these lines."""
    if stdin is None:
        stdin = b"".join(l + b"\n" for l in lines)
    want = expected(lines)
    got = run(stdin)
    if got != want[:3]:
        failures.append((name, want[:3], got))
    return want[3]


def diff(a, b):
    for i, (x, y) in enumerate(zip(a, b)):
        if x != y:
            return f"first difference at byte {i}: want {a[i:i+40]!r}, got {b[i:i+40]!r}"
    return f"lengths differ: want {len(a)}, got {len(b)}"


def main():
    build()

    # 1. The binary is what it claims: static, no interpreter, no libc.
    rl = subprocess.run(["readelf", "-l", "-d", str(BIN)], capture_output=True,
                        text=True, check=True).stdout
    for bad in ("INTERP", "NEEDED", "DYNAMIC"):
        if bad in rl:
            failures.append(("static-binary", f"no {bad}", rl))

    # 2. Ten plain values, output layout exactly as specified.
    check("plain", [b"1", b"2", b"3", b"4", b"5", b"6", b"7", b"8", b"9", b"10"])

    # 3. Every boundary of int32, both signs, with and without '+'.
    edge = [b"2147483647", b"+2147483647", b"-2147483648", b"-2147483647",
            b"0", b"-0", b"+0", b"-1", b"1", b"-2147483648"]
    vals = check("int32-edges", edge)
    assert vals == [INT_MAX, INT_MAX, INT_MIN, INT_MIN + 1, 0, 0, 0, -1, 1, INT_MIN], vals

    # 4. Rejections, then recovery.  Each of these must be refused and the
    #    program must then accept the next good line.  The 2020 original
    #    accepted the ones marked (*) — see README.
    bad = [b"", b"+", b"-", b"abc", b"12a", b"a12", b" 12", b"12 ", b"1 2",
           b"--1", b"+-1", b"1.0", b"1e3", b"0x10",
           b"2147483648",            # INT_MAX + 1
           b"+2147483648",           # (*) original stored this as INT_MIN
           b"02147483648",           # (*) original stored this as INT_MIN
           b"-2147483649",           # INT_MIN - 1
           b"99999999999999999999",  # far past 64 bits of magnitude
           b"-99999999999999999999",
           b"1" * 200,               # (*) longer than the buffer: original truncated
           b"0" * 62 + b"5" * 20]    # (*) valid prefix, too long as a whole
    lines = []
    for b in bad:
        lines += [b, b"7"]
    lines += [b"7"] * (N - len(bad)) if len(bad) < N else []
    vals = check("rejections", lines[: 2 * len(bad)] + [b"7"] * max(0, N - len(bad)))
    assert all(v == 7 for v in vals) and len(vals) == N, vals

    # 5. Leading zeros are digits: the value is what counts, up to 63 chars.
    vals = check("leading-zeros", [b"0" * 53 + b"2147483647", b"-" + b"0" * 52 + b"2147483648",
                                   b"000", b"+" + b"0" * 61 + b"1",
                                   b"0" * 63, b"1", b"2", b"3", b"4", b"5"])
    assert vals == [INT_MAX, INT_MIN, 0, 1, 0, 1, 2, 3, 4, 5], vals

    # 6. Exactly 63 characters fits; 64 does not.
    vals = check("buffer-edge", [b"0" * 62 + b"9", b"0" * 63 + b"9", b"1"] + [b"0"] * 8)
    assert vals[:2] == [9, 1], vals

    # 7. The rounded average: halves away from zero, negatives included.
    #    The original truncated negatives toward zero (-19 -> -1).
    for total, want in [(15, 2), (14, 1), (5, 1), (4, 0), (0, 0), (-4, 0), (-5, -1),
                        (-14, -1), (-15, -2), (-19, -2), (-21, -2), (-25, -3),
                        (INT_MAX * 10, INT_MAX), (INT_MIN * 10, INT_MIN)]:
        base = total // N
        rest = total - base * N
        lines = [str(base).encode()] * (N - 1) + [str(base + rest).encode()]
        assert sum(int(x) for x in lines) == total
        vals = check(f"average-{total}", lines)
        assert round_half_away(total, N) == want, (total, want)

    # 8. The sum needs more than 32 bits.  The original wrapped silently.
    check("sum-overflow", [b"2147483647"] * N)
    check("sum-underflow", [b"-2147483648"] * N)

    # 9. End of input before ten values: message on stderr, exit 1.
    check("eof-empty", [])
    check("eof-partial", [b"1", b"2", b"3"])
    check("eof-after-bad", [b"x"])

    # 10. Line-ending variants: CRLF is accepted; a last line with no newline counts.
    check("crlf", [b"1\r", b"-2\r", b"3\r", b"4", b"5", b"6", b"7", b"8", b"9", b"10"])
    check("no-final-newline", [b"1"] * N, stdin=b"\n".join([b"1"] * N))

    # 11. Random: values across the whole range with junk lines mixed in,
    #     byte-exact against the oracle.
    rng = random.Random(20260911)
    junk = [b"", b"x", b"1x", b"-", b"+", b"2147483648", b"-2147483649", b"9" * 30,
            b"1" * 100, b" 1", b"1 "]
    for round_ in range(300):
        lines = []
        good = 0
        while good < N:
            r = rng.random()
            if r < 0.15:
                lines.append(rng.choice(junk))
                continue
            if r < 0.35:
                v = rng.choice([INT_MIN, INT_MAX, 0, -1, 1, INT_MIN + 1, INT_MAX - 1])
            elif r < 0.6:
                v = rng.randint(-1000, 1000)
            else:
                v = rng.randint(INT_MIN, INT_MAX)
            s = str(v).encode()
            if v >= 0 and rng.random() < 0.2:
                s = b"+" + s
            lines.append(s)
            good += 1
        check(f"random-{round_}", lines)

    if failures:
        for name, want, got in failures:
            print(f"FAIL {name}")
            for label, w, g in zip(("stdout", "stderr", "exit"), want, got):
                if w != g:
                    print(f"  {label}: {diff(str(w), str(g))}")
        print(f"\n{len(failures)} failure(s)")
        return 1
    print(f"ok — {BIN.stat().st_size} bytes, static; oracle model, "
          f"{11} test groups, 300 random runs, 0 failures")
    return 0


if __name__ == "__main__":
    sys.exit(main())
