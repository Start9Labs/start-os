import {
  ValueSpec, ValueSpecList, DefaultString, ValueSpecUnion, ConfigSpec,
  ValueSpecObject, ValueSpecString, ValueSpecEnum, ValueSpecNumber,
  ValueSpecBoolean, ValueSpecPointer, ValueSpecOf, ListValueSpecType
} from './config-types'

export interface Annotation {
  invalid: string | null
  edited: boolean
  added: boolean
}

export type Annotations<T extends string> =
  T extends 'object' | 'union' ? { self: Annotation, members: { [key: string]: Annotation } } :
  T extends 'list' ?             { self: Annotation, members: Annotation[] } :
  Annotation

export class Range {
  min?: number
  max?: number
  minInclusive: boolean
  maxInclusive: boolean


  static from (s: string): Range {
    const r = new Range()
    r.minInclusive = s.startsWith('[')
    r.maxInclusive = s.endsWith(']')
    const [minStr, maxStr] = s.split(',').map(a => a.trim())
    r.min = minStr === '(*' ? undefined : Number(minStr.slice(1))
    r.max = maxStr === '*)' ? undefined : Number(maxStr.slice(0, -1))
    return r
  }

  checkIncludes (n: number) {
    if (this.hasMin() !== undefined && ((!this.minInclusive && this.min == n || (this.min > n)))) {
      throw new Error(`Value must be ${this.minMessage()}.`)
    }
    if (this.hasMax() && ((!this.maxInclusive && this.max == n || (this.max < n)))) {
      throw new Error(`Value must be ${this.maxMessage()}.`)
    }
  }

  hasMin (): boolean {
    return this.min !== undefined
  }

  hasMax (): boolean {
    return this.max !== undefined
  }

  minMessage (): string {
    return `greater than${this.minInclusive ? ' or equal to' : ''} ${this.min}`
  }

  maxMessage (): string {
    return `less than${this.maxInclusive ? ' or equal to' : ''} ${this.max}`
  }

  description (): string {
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

  integralMin (): number | undefined {
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

  integralMax (): number | undefined {
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

// converts a ValueSpecList, i.e. a spec for a list, to its inner ListValueSpec, i.e., a spec for the values within the list.
// We then augment it with the defalt values (e.g. nullable: false) to make a
export function listInnerSpec (listSpec: ValueSpecList): ValueSpecOf<ListValueSpecType> {
  return {
    type: listSpec.subtype,
    nullable: false,
    name: listSpec.name,
    description: listSpec.description,
    changeWarning: listSpec['change-warning'],
    ...listSpec.spec as any, //listSpec.spec is a ListValueSpecOf listSpec.subtype
  }
}

export function mapSpecToConfigValue (spec: ValueSpec, value: any): any {
  if (value === undefined) return undefined
  switch (spec.type) {
    case 'string': return mapStringSpec(value)
    case 'number': return mapNumberSpec(value)
    case 'boolean': return mapBooleanSpec(spec, value)
    case 'enum': return mapEnumSpec(spec, value)
    case 'list': return mapListSpec(spec, value)
    case 'object': return mapObjectSpec(spec, value)
    case 'union': return mapUnionSpec(spec, value)
    case 'pointer': return value
  }
}

export function mapConfigSpec (configSpec: ConfigSpec, value: any): object {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    Object.entries(configSpec).map(([key, val]) => {
      value[key] = mapSpecToConfigValue(val, value[key])
      if (value[key] === undefined) {
        value[key] = getDefaultConfigValue(val)
      }
    })
    return value
  } else {
    return getDefaultObject(configSpec)
  }
}

export function mapObjectSpec (spec: ValueSpecObject, value: any): object {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    return mapConfigSpec(spec.spec, value)
  } else {
    return null
  }
}

export function mapUnionSpec (spec: ValueSpecUnion, value: any): object {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    const variant = mapEnumSpec({
      ...spec.tag,
      type: 'enum',
      default: spec.default,
      values: Object.keys(spec.variants),
      'value-names': spec.tag['variant-names'],
    }, value[spec.tag.id])
    value = mapConfigSpec(spec.variants[variant], value)
    value[spec.tag.id] = variant
    return value
  } else {
    return getDefaultUnion(spec)
  }
}

export function mapStringSpec (value: any): string {
  if (typeof value === 'string') {
    return value
  } else {
    return null
  }
}

export function mapNumberSpec (value: any): number {
  if (typeof value === 'number') {
    return value
  } else {
    return null
  }
}

export function mapEnumSpec (spec: ValueSpecEnum, value: any): string {
  if (typeof value === 'string' && spec.values.includes(value)) {
    return value
  } else {
    return spec.default
  }
}

export function mapListSpec (spec: ValueSpecList, value: any): string[] | number[] | object[] {
  if (Array.isArray(value)) {
    const innerSpec = listInnerSpec(spec)
    return value.map(item => mapSpecToConfigValue(innerSpec, item))
  } else {
    return getDefaultList(spec)
  }
}

export function mapBooleanSpec (spec: ValueSpecBoolean, value: any): boolean {
  if (typeof value === 'boolean') {
    return value
  } else {
    return spec.default
  }
}

export function getDefaultConfigValue (spec: ValueSpec): string | number | object | string[] | number[] | object[] | boolean | null {
  switch (spec.type) {
    case 'object':
      return getDefaultObject(spec.spec)
    case 'union':
      return getDefaultUnion(spec)
    case 'string':
      return spec.default ? getDefaultString(spec.default) : null
    case 'number':
      return spec.default || null
    case 'list':
      return getDefaultList(spec)
    case 'enum':
    case 'boolean':
      return spec.default
    case 'pointer':
      return null
  }
}

export function getDefaultObject (spec: ConfigSpec): object {
  const obj = { }
  Object.entries(spec).map(([key, val]) => {
    obj[key] = getDefaultConfigValue(val)
  })

  return obj
}

export function getDefaultList (spec: ValueSpecList): string[] | number[] | object[] {
  if (spec.subtype === 'object') {
    const l = (spec.default as any[])
    const range = Range.from(spec.range)
    while (l.length < range.integralMin()) {
      l.push(getDefaultConfigValue(listInnerSpec(spec)))
    }
    return l as string[] | number[] | object[]
  } else {
    const l = (spec.default as any[]).map(d => getDefaultConfigValue({ ...listInnerSpec(spec), default: d }))
    return l as string[] | number[] | object[]
  }
}

export function getDefaultUnion (spec: ValueSpecUnion): object {
  return { [spec.tag.id]: spec.default, ...getDefaultObject(spec.variants[spec.default]) }
}

export function getDefaultMapTagKey (defaultSpec: DefaultString = '', value: object): string {
  const keySrc = getDefaultString(defaultSpec)

  const keys = Object.keys(value)

  let key = keySrc
  let idx = 1
  while (keys.includes(key)) {
    key = `${keySrc}-${idx++}`
  }

  return key
}

export function getDefaultString (defaultSpec: DefaultString): string {
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

export function getDefaultDescription (spec: ValueSpec): string {
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

// a,g,h,A-Z,,,,-
export function getRandomCharInSet (charset: string): string {
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

function stringToCharSet (charset: string): CharSet {
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