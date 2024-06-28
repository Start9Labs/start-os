const msb = 0x80
const dropMsb = 0x7f
const maxSize = Math.floor((8 * 8 + 7) / 7)

export class VarIntProcessor {
  private buf: Uint8Array
  private i: number
  constructor() {
    this.buf = new Uint8Array(maxSize)
    this.i = 0
  }
  push(b: number) {
    if (this.i >= maxSize) {
      throw new Error("Unterminated varint")
    }
    this.buf[this.i] = b
    this.i += 1
  }
  finished(): boolean {
    return this.i > 0 && (this.buf[this.i - 1] & msb) === 0
  }
  decode(): number | null {
    let result = 0
    let shift = 0
    let success = false
    for (let i = 0; i < this.i; i++) {
      const b = this.buf[i]
      const msbDropped = b & dropMsb
      result |= msbDropped << shift
      shift += 7

      if ((b & msb) == 0 || shift > 9 * 7) {
        success = (b & msb) === 0
        break
      }
    }

    if (success) {
      return result
    } else {
      console.error(this.buf)
      return null
    }
  }
}

export function serializeVarint(int: number): ArrayBuffer {
  const buf = new Uint8Array(maxSize)
  let n = int
  let i = 0

  while (n >= msb) {
    buf[i] = msb | n
    i += 1
    n >>= 7
  }

  buf[i] = n
  i += 1

  return buf.slice(0, i).buffer
}
