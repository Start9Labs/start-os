import { Pattern } from '../actions/input/inputSpecTypes'
import * as regexes from './regexes'

/** Pattern for validating IPv6 addresses. */
export const ipv6: Pattern = {
  regex: regexes.ipv6.matches(),
  description: 'Must be a valid IPv6 address',
}

/** Pattern for validating IPv4 addresses. */
export const ipv4: Pattern = {
  regex: regexes.ipv4.matches(),
  description: 'Must be a valid IPv4 address',
}

/** Pattern for validating hostnames (RFC-compliant). */
export const hostname: Pattern = {
  regex: regexes.hostname.matches(),
  description: 'Must be a valid hostname',
}

/** Pattern for validating `.local` mDNS hostnames. */
export const localHostname: Pattern = {
  regex: regexes.localHostname.matches(),
  description: 'Must be a valid ".local" hostname',
}

/** Pattern for validating HTTP/HTTPS URLs. */
export const url: Pattern = {
  regex: regexes.url.matches(),
  description: 'Must be a valid URL',
}

/** Pattern for validating `.local` URLs (mDNS/LAN). */
export const localUrl: Pattern = {
  regex: regexes.localUrl.matches(),
  description: 'Must be a valid ".local" URL',
}

/** Pattern for validating ASCII-only strings (printable characters). */
export const ascii: Pattern = {
  regex: regexes.ascii.matches(),
  description:
    'May only contain ASCII characters. See https://www.w3schools.com/charsets/ref_html_ascii.asp',
}

/** Pattern for validating fully qualified domain names (FQDNs). */
export const domain: Pattern = {
  regex: regexes.domain.matches(),
  description: 'Must be a valid Fully Qualified Domain Name',
}

/** Pattern for validating email addresses. */
export const email: Pattern = {
  regex: regexes.email.matches(),
  description: 'Must be a valid email address',
}

/** Pattern for validating email addresses, optionally with a display name (e.g. `"John Doe <john@example.com>"`). */
export const emailWithName: Pattern = {
  regex: regexes.emailWithName.matches(),
  description: 'Must be a valid email address, optionally with a name',
}

/** Pattern for validating base64-encoded strings. */
export const base64: Pattern = {
  regex: regexes.base64.matches(),
  description:
    'May only contain base64 characters. See https://base64.guru/learn/base64-characters',
}
