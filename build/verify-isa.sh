#!/bin/bash
# Verify a RISC-V binary contains no instructions outside the SpaceMiT K1 ISA.
#
# K1 (BPI-F3) supports RVA22 + V 1.0 + Zicbom/Zicbop/Zicboz + Zihintpause.
# Instructions from RVA23-only extensions (Zicond, Zfa, Zacas, Zfh, Zcmop,
# Zimop, vector crypto) SIGILL on this CPU.
set -e

BINARY="${1:?usage: verify-isa.sh <binary>}"

if [ ! -f "$BINARY" ]; then
    echo "ERROR: $BINARY not found" >&2
    exit 1
fi

OBJDUMP=""
for candidate in \
    "${RISCV_OBJDUMP:-}" \
    riscv64-linux-gnu-objdump \
    riscv64-openwrt-linux-musl-objdump \
    riscv64-unknown-linux-musl-objdump; do
    if [ -n "$candidate" ] && command -v "$candidate" >/dev/null 2>&1; then
        OBJDUMP="$candidate"
        break
    fi
done
if [ -z "$OBJDUMP" ]; then
    for f in openwrt/staging_dir/toolchain-riscv64*/bin/*-objdump; do
        if [ -x "$f" ]; then
            OBJDUMP="$f"
            break
        fi
    done
fi
if [ -z "$OBJDUMP" ]; then
    echo "ERROR: no riscv64 objdump found (install binutils-riscv64-linux-gnu or set RISCV_OBJDUMP)" >&2
    exit 1
fi

# RVA23-only instructions that SIGILL on K1:
#   Zicond:  czero.eqz, czero.nez
#   Zfa:     fli.*, fround(nx)?.*, fmaxm.*, fminm.*, fleq.*, fltq.*,
#            fcvtmod.w.d, fmvh.x.d, fmvp.d.x
#   Zacas:   amocas.w/d/q
#   Zfh:     flh, fsh, f*.h (half-precision FP)
#   Zcmop:   c.mop.N
#   Zimop:   mop.r.N, mop.rr.N
#   Zcb:     c.lbu, c.lhu, c.lh, c.sb, c.sh, c.zext.b, c.sext.b,
#            c.zext.h, c.sext.h, c.zext.w, c.not, c.mul
BANNED_RE='\b(czero\.(eqz|nez)|fli\.[sdhq]|fround(nx)?\.[sdhq]|fmaxm\.[sdhq]|fminm\.[sdhq]|fleq\.[sdhq]|fltq\.[sdhq]|fcvtmod\.w\.d|fmvh\.x\.d|fmvp\.d\.x|amocas\.[wdq]|c\.mop\.[0-9]+|mop\.r\.[0-9]+|mop\.rr\.[0-9]+|f(add|sub|mul|div|sqrt|min|max|madd|msub|nmadd|nmsub|sgnj|sgnjn|sgnjx|eq|lt|le|class|mv|cvt)\.h|c\.(lbu|lhu|lh|sb|sh|zext\.[bhw]|sext\.[bh]|not|mul))\b'

echo "==> Verifying $(basename "$BINARY") against K1 ISA (via $(basename "$OBJDUMP"))..."
FOUND=$("$OBJDUMP" -d -M no-aliases "$BINARY" 2>/dev/null | grep -oE "$BANNED_RE" | sort -u || true)

if [ -n "$FOUND" ]; then
    echo "ERROR: $BINARY contains RVA23-only instructions not supported by SpaceMiT K1:" >&2
    echo "$FOUND" | sed 's/^/    /' >&2
    echo "" >&2
    echo "Check RUSTFLAGS in build/build-rust.sh and the cargo-zigbuild image default." >&2
    exit 1
fi

echo "    OK — no RVA23-only instructions"
