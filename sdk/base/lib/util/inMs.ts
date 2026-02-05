/**
 * @module inMs
 *
 * Parses human-readable time strings into milliseconds.
 */

/** @internal Regex for parsing time strings */
const matchTimeRegex = /^\s*(\d+)?(\.\d+)?\s*(ms|s|m|h|d)/

/**
 * Gets the millisecond multiplier for a time unit.
 * @internal
 */
const unitMultiplier = (unit?: string) => {
  if (!unit) return 1
  if (unit === "ms") return 1
  if (unit === "s") return 1000
  if (unit === "m") return 1000 * 60
  if (unit === "h") return 1000 * 60 * 60
  if (unit === "d") return 1000 * 60 * 60 * 24
  throw new Error(`Invalid unit: ${unit}`)
}

/**
 * Converts decimal digits to milliseconds.
 * @internal
 */
const digitsMs = (digits: string | null, multiplier: number) => {
  if (!digits) return 0
  const value = parseInt(digits.slice(1))
  const divideBy = multiplier / Math.pow(10, digits.length - 1)
  return Math.round(value * divideBy)
}

/**
 * Parses a time value (string or number) into milliseconds.
 *
 * Accepts human-readable time strings with units:
 * - `ms` - milliseconds
 * - `s` - seconds
 * - `m` - minutes
 * - `h` - hours
 * - `d` - days
 *
 * Numbers are returned unchanged (assumed to be in milliseconds).
 *
 * @param time - Time value as string with unit or number in milliseconds
 * @returns Time in milliseconds, or undefined if input is undefined
 * @throws Error if string format is invalid
 *
 * @example
 * ```typescript
 * inMs('5s')      // 5000
 * inMs('1.5m')    // 90000
 * inMs('2h')      // 7200000
 * inMs('1d')      // 86400000
 * inMs(3000)      // 3000
 * inMs('500ms')   // 500
 * ```
 */
export const inMs = (time?: string | number) => {
  if (typeof time === "number") return time
  if (!time) return undefined
  const matches = time.match(matchTimeRegex)
  if (!matches) throw new Error(`Invalid time format: ${time}`)
  const [_, leftHandSide, digits, unit] = matches
  const multiplier = unitMultiplier(unit)
  const firstValue = parseInt(leftHandSide || "0") * multiplier
  const secondValue = digitsMs(digits, multiplier)

  return firstValue + secondValue
}
