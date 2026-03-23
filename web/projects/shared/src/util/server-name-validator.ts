import { AbstractControl, ValidationErrors } from '@angular/forms'
import { normalizeHostnameRaw } from './hostname'

// Matches backend normalize() threshold (core/src/hostname.rs:109)
const HOSTNAME_MIN_LENGTH = 4
// DNS label limit
const HOSTNAME_MAX_LENGTH = 63

export function serverNameValidator(
  control: AbstractControl,
): ValidationErrors | null {
  const name = (control.value || '').trim()
  if (!name) return null

  const hostname = normalizeHostnameRaw(name)

  if (hostname.length < HOSTNAME_MIN_LENGTH) {
    return { hostnameMinLength: true }
  }

  if (hostname.length > HOSTNAME_MAX_LENGTH) {
    return { hostnameMaxLength: true }
  }

  return null
}
