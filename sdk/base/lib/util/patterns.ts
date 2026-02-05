/**
 * @module patterns
 *
 * Pre-built validation patterns for common input types.
 * Use these with text inputs to validate user-entered values.
 *
 * Each pattern includes a regex and a human-readable description
 * that's shown when validation fails.
 *
 * @example
 * ```typescript
 * import { Patterns } from '@start9labs/sdk'
 *
 * // Validate an email field
 * Value.text({
 *   name: 'Email',
 *   patterns: [Patterns.email]
 * })
 *
 * // Validate a Tor hostname
 * Value.text({
 *   name: 'Onion Address',
 *   patterns: [Patterns.torHostname]
 * })
 * ```
 */
import { Pattern } from "../actions/input/inputSpecTypes"
import * as regexes from "./regexes"

/** Validates IPv6 addresses */
export const ipv6: Pattern = {
  regex: regexes.ipv6.matches(),
  description: "Must be a valid IPv6 address",
}

/** Validates IPv4 addresses (e.g., "192.168.1.1") */
export const ipv4: Pattern = {
  regex: regexes.ipv4.matches(),
  description: "Must be a valid IPv4 address",
}

/** Validates general hostnames */
export const hostname: Pattern = {
  regex: regexes.hostname.matches(),
  description: "Must be a valid hostname",
}

/** Validates .local mDNS hostnames (e.g., "mydevice.local") */
export const localHostname: Pattern = {
  regex: regexes.localHostname.matches(),
  description: 'Must be a valid ".local" hostname',
}

/** Validates Tor .onion hostnames */
export const torHostname: Pattern = {
  regex: regexes.torHostname.matches(),
  description: 'Must be a valid Tor (".onion") hostname',
}

/** Validates general URLs */
export const url: Pattern = {
  regex: regexes.url.matches(),
  description: "Must be a valid URL",
}

/** Validates .local mDNS URLs */
export const localUrl: Pattern = {
  regex: regexes.localUrl.matches(),
  description: 'Must be a valid ".local" URL',
}

/** Validates Tor .onion URLs */
export const torUrl: Pattern = {
  regex: regexes.torUrl.matches(),
  description: 'Must be a valid Tor (".onion") URL',
}

/** Validates ASCII-only text (printable characters) */
export const ascii: Pattern = {
  regex: regexes.ascii.matches(),
  description:
    "May only contain ASCII characters. See https://www.w3schools.com/charsets/ref_html_ascii.asp",
}

/** Validates fully qualified domain names (FQDNs) */
export const domain: Pattern = {
  regex: regexes.domain.matches(),
  description: "Must be a valid Fully Qualified Domain Name",
}

/** Validates email addresses (e.g., "user@example.com") */
export const email: Pattern = {
  regex: regexes.email.matches(),
  description: "Must be a valid email address",
}

/** Validates email addresses with optional display name (e.g., "John Doe <john@example.com>") */
export const emailWithName: Pattern = {
  regex: regexes.emailWithName.matches(),
  description: "Must be a valid email address, optionally with a name",
}

/** Validates base64-encoded strings */
export const base64: Pattern = {
  regex: regexes.base64.matches(),
  description:
    "May only contain base64 characters. See https://base64.guru/learn/base64-characters",
}
