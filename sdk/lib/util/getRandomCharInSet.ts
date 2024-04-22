// a,g,h,A-Z,,,,-

import * as crypto from "crypto"
export function getRandomCharInSet(charset: string): string {
  const set = stringToCharSet(charset)
  let charIdx = crypto.randomInt(0, set.len)
  for (let range of set.ranges) {
    if (range.len > charIdx) {
      return String.fromCharCode(range.start.charCodeAt(0) + charIdx)
    }
    charIdx -= range.len
  }
  throw new Error("unreachable")
}
function stringToCharSet(charset: string): CharSet {
  let set: CharSet = { ranges: [], len: 0 }
  let start: string | null = null
  let end: string | null = null
  let in_range = false
  for (let char of charset) {
    switch (char) {
      case ",":
        if (start !== null && end !== null) {
          if (start!.charCodeAt(0) > end!.charCodeAt(0)) {
            throw new Error("start > end of charset")
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
          end = ","
        } else if (start === null && end === null && !in_range) {
          start = ","
        } else {
          throw new Error('unexpected ","')
        }
        break
      case "-":
        if (start === null) {
          start = "-"
        } else if (!in_range) {
          in_range = true
        } else if (in_range && end === null) {
          end = "-"
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
      throw new Error("start > end of charset")
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
type CharSet = {
  ranges: {
    start: string
    end: string
    len: number
  }[]
  len: number
}
