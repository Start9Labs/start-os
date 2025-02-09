import { Pattern } from "../actions/input/inputSpecTypes"
import * as regexes from "./regexes"

export const ipv6: Pattern = {
  regex: regexes.ipv6.matches(),
  description: "Must be a valid IPv6 address",
}

export const ipv4: Pattern = {
  regex: regexes.ipv4.matches(),
  description: "Must be a valid IPv4 address",
}

export const hostname: Pattern = {
  regex: regexes.hostname.matches(),
  description: "Must be a valid hostname",
}

export const localHostname: Pattern = {
  regex: regexes.localHostname.matches(),
  description: 'Must be a valid ".local" hostname',
}

export const torHostname: Pattern = {
  regex: regexes.torHostname.matches(),
  description: 'Must be a valid Tor (".onion") hostname',
}

export const url: Pattern = {
  regex: regexes.url.matches(),
  description: "Must be a valid URL",
}

export const localUrl: Pattern = {
  regex: regexes.localUrl.matches(),
  description: 'Must be a valid ".local" URL',
}

export const torUrl: Pattern = {
  regex: regexes.torUrl.matches(),
  description: 'Must be a valid Tor (".onion") URL',
}

export const ascii: Pattern = {
  regex: regexes.ascii.matches(),
  description:
    "May only contain ASCII characters. See https://www.w3schools.com/charsets/ref_html_ascii.asp",
}

export const domain: Pattern = {
  regex: regexes.domain.matches(),
  description: "Must be a valid Fully Qualified Domain Name",
}

export const email: Pattern = {
  regex: regexes.email.matches(),
  description: "Must be a valid email address",
}

export const emailWithName: Pattern = {
  regex: regexes.emailWithName.matches(),
  description: "Must be a valid email address, optionally with a name",
}

export const base64: Pattern = {
  regex: regexes.base64.matches(),
  description:
    "May only contain base64 characters. See https://base64.guru/learn/base64-characters",
}
