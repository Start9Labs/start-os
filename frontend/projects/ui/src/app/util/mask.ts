export function mask(val: string, max: number = Infinity): string {
  return '●'.repeat(Math.min(max, val.length))
}
