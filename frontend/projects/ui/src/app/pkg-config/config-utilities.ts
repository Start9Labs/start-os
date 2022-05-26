import { ValueSpec, DefaultString } from './config-types'

export class Range {
  min?: number
  max?: number
  minInclusive: boolean
  maxInclusive: boolean

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

  hasMin(): this is Range & { min: number } {
    return this.min !== undefined
  }

  hasMax(): this is Range & { max: number } {
    return this.max !== undefined
  }

  minMessage(): string {
    return `greater than${this.minInclusive ? ' or equal to' : ''} ${this.min}`
  }

  maxMessage(): string {
    return `less than${this.maxInclusive ? ' or equal to' : ''} ${this.max}`
  }

  description(): string {
    let message = 'Value can be any number.'

    if (this.hasMin() || this.hasMax()) {
      message = 'Value must be'
    }

    if (this.hasMin() && this.hasMax()) {
      message = `${message} ${this.minMessage()} AND ${this.maxMessage()}.`
    } else if (this.hasMin() && !this.hasMax()) {
      message = `${message} ${this.minMessage()}.`
    } else if (!this.hasMin() && this.hasMax()) {
      message = `${message} ${this.maxMessage()}.`
    }

    return message
  }

  integralMin(): number | undefined {
    if (this.min) {
      const ceil = Math.ceil(this.min)
      if (this.minInclusive) {
        return ceil
      } else {
        if (ceil === this.min) {
          return ceil + 1
        } else {
          return ceil
        }
      }
    }
  }

  integralMax(): number | undefined {
    if (this.max) {
      const floor = Math.floor(this.max)
      if (this.maxInclusive) {
        return floor
      } else {
        if (floor === this.max) {
          return floor - 1
        } else {
          return floor
        }
      }
    }
  }
}

export function getDefaultDescription(spec: ValueSpec): string {
  let toReturn: string | undefined
  switch (spec.type) {
    case 'string':
      if (typeof spec.default === 'string') {
        toReturn = spec.default
      } else if (typeof spec.default === 'object') {
        toReturn = 'random'
      }
      break
    case 'number':
      if (typeof spec.default === 'number') {
        toReturn = String(spec.default)
      }
      break
    case 'boolean':
      toReturn = spec.default === true ? 'True' : 'False'
      break
    case 'enum':
      toReturn = spec['value-names'][spec.default]
      break
  }

  return toReturn || ''
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
export function getRandomCharInSet(charset: string): string {
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
