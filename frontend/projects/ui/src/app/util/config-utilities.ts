import { DefaultString } from 'start-sdk/lib/config/config-types'

export class Range {
  min?: number
  max?: number
  minInclusive!: boolean
  maxInclusive!: boolean

  static from(s: string): Range {
    const r = new Range()
    r.minInclusive = s.startsWith('[')
    r.maxInclusive = s.endsWith(']')
    const [minStr, maxStr] = s.split(',').map(a => a.trim())
    r.min = minStr === '(*' ? undefined : Number(minStr.slice(1))
    r.max = maxStr === '*)' ? undefined : Number(maxStr.slice(0, -1))
    return r
  }

  checkIncludes(n: number) {
    if (
      this.hasMin() &&
      (this.min > n || (!this.minInclusive && this.min == n))
    ) {
      throw new Error(this.minMessage())
    }
    if (
      this.hasMax() &&
      (this.max < n || (!this.maxInclusive && this.max == n))
    ) {
      throw new Error(this.maxMessage())
    }
  }

  private hasMin(): this is Range & { min: number } {
    return this.min !== undefined
  }

  private hasMax(): this is Range & { max: number } {
    return this.max !== undefined
  }

  private minMessage(): string {
    return `greater than${this.minInclusive ? ' or equal to' : ''} ${this.min}`
  }

  private maxMessage(): string {
    return `less than${this.maxInclusive ? ' or equal to' : ''} ${this.max}`
  }
}

export function getDefaultString(defaultSpec: DefaultString): string {
  if (typeof defaultSpec === 'string') {
    return defaultSpec
  } else {
    let s = ''
    for (let i = 0; i < defaultSpec.len; i++) {
      s = s + getRandomCharInSet(defaultSpec.charset)
    }

    return s
  }
}

// a,g,h,A-Z,,,,-
function getRandomCharInSet(charset: string): string {
  const set = stringToCharSet(charset)
  let charIdx = Math.floor(Math.random() * set.len)
  for (let range of set.ranges) {
    if (range.len > charIdx) {
      return String.fromCharCode(range.start.charCodeAt(0) + charIdx)
    }
    charIdx -= range.len
  }
  throw new Error('unreachable')
}

function stringToCharSet(charset: string): CharSet {
  let set: CharSet = { ranges: [], len: 0 }
  let start: string | null = null
  let end: string | null = null
  let in_range = false
  for (let char of charset) {
    switch (char) {
      case ',':
        if (start !== null && end !== null) {
          if (start!.charCodeAt(0) > end!.charCodeAt(0)) {
            throw new Error('start > end of charset')
          }
          const len = end.charCodeAt(0) - start.charCodeAt(0) + 1
          set.ranges.push({
            start,
            end,
            len,
          })
          set.len += len
          start = null
          end = null
          in_range = false
        } else if (start !== null && !in_range) {
          set.len += 1
          set.ranges.push({ start, end: start, len: 1 })
          start = null
        } else if (start !== null && in_range) {
          end = ','
        } else if (start === null && end === null && !in_range) {
          start = ','
        } else {
          throw new Error('unexpected ","')
        }
        break
      case '-':
        if (start === null) {
          start = '-'
        } else if (!in_range) {
          in_range = true
        } else if (in_range && end === null) {
          end = '-'
        } else {
          throw new Error('unexpected "-"')
        }
        break
      default:
        if (start === null) {
          start = char
        } else if (in_range && end === null) {
          end = char
        } else {
          throw new Error(`unexpected "${char}"`)
        }
    }
  }
  if (start !== null && end !== null) {
    if (start!.charCodeAt(0) > end!.charCodeAt(0)) {
      throw new Error('start > end of charset')
    }
    const len = end.charCodeAt(0) - start.charCodeAt(0) + 1
    set.ranges.push({
      start,
      end,
      len,
    })
    set.len += len
  } else if (start !== null) {
    set.len += 1
    set.ranges.push({
      start,
      end: start,
      len: 1,
    })
  }
  return set
}

interface CharSet {
  ranges: {
    start: string
    end: string
    len: number
  }[]
  len: number
}
