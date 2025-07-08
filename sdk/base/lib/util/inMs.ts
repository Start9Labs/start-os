const matchTimeRegex = /^\s*(\d+)?(\.\d+)?\s*(ms|s|m|h|d)/

const unitMultiplier = (unit?: string) => {
  if (!unit) return 1
  if (unit === "ms") return 1
  if (unit === "s") return 1000
  if (unit === "m") return 1000 * 60
  if (unit === "h") return 1000 * 60 * 60
  if (unit === "d") return 1000 * 60 * 60 * 24
  throw new Error(`Invalid unit: ${unit}`)
}
const digitsMs = (digits: string | null, multiplier: number) => {
  if (!digits) return 0
  const value = parseInt(digits.slice(1))
  const divideBy = multiplier / Math.pow(10, digits.length - 1)
  return Math.round(value * divideBy)
}
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
