/**
 * TS port of the Rust `normalize()` function from core/src/hostname.rs.
 * Converts a free-text name into a valid hostname.
 */
export function normalizeHostname(name: string): string {
  let prevWasDash = true
  let normalized = ''

  for (const c of name) {
    if (/[a-zA-Z0-9]/.test(c)) {
      prevWasDash = false
      normalized += c.toLowerCase()
    } else if ((c === '-' || /\s/.test(c)) && !prevWasDash) {
      prevWasDash = true
      normalized += '-'
    }
  }

  while (normalized.endsWith('-')) {
    normalized = normalized.slice(0, -1)
  }

  return normalized || 'start9'
}
