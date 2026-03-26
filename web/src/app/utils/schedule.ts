/** Convert quarter-hour index (0-96) to "HH:MM" string */
export function quarterHourToTime(quarterHours: number): string {
  return `${Math.floor(quarterHours / 4)
    .toString()
    .padStart(2, '0')}:${((quarterHours % 4) * 15).toString().padStart(2, '0')}`
}

/** Convert "HH:MM" string to quarter-hour index (0-96) */
export function timeToQuarterHour(formatted: string): number {
  const [h, m] = formatted.split(':').map(Number)

  return h * 4 + Math.round(m / 15)
}

/** Format "HH:MM" (24h) to "h:MMam/pm" (12h) */
export function formatTime12h(time: string): string {
  const [h, m] = time.split(':').map(Number)
  const period = h >= 12 ? 'pm' : 'am'
  const hour = h % 12 || 12

  return h === 24
    ? `11:59pm`
    : `${hour}:${m.toString().padStart(2, '0')}${period}`
}
