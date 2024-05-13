export type TimeUnit = "d" | "h" | "s" | "ms"
export type Duration = `${number}${TimeUnit}`

export function duration(timeValue: number, timeUnit: TimeUnit = "s") {
  return `${timeValue > 0 ? timeValue : 0}${timeUnit}` as Duration
}
