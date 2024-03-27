import { Pattern } from "../config/configTypes"
import * as regexes from "./regexes"

export const ipv6: Pattern = {
  regex: regexes.ipv6.toString(),
  description: "Must be a valid IPv6 address",
}

export const ipv4: Pattern = {
  regex: regexes.ipv4.toString(),
  description: "Must be a valid IPv4 address",
}

export const hostname: Pattern = {
  regex: regexes.hostname.toString(),
  description: "Must be a valid hostname",
}

export const localHostname: Pattern = {
  regex: regexes.localHostname.toString(),
  description: 'Must be a valid ".local" hostname',
}

export const torHostname: Pattern = {
  regex: regexes.torHostname.toString(),
  description: 'Must be a valid Tor (".onion") hostname',
}

export const url: Pattern = {
  regex: regexes.url.toString(),
  description: "Must be a valid URL",
}

export const localUrl: Pattern = {
  regex: regexes.localUrl.toString(),
  description: 'Must be a valid ".local" URL',
}

export const torUrl: Pattern = {
  regex: regexes.torUrl.toString(),
  description: 'Must be a valid Tor (".onion") URL',
}

export const ascii: Pattern = {
  regex: regexes.ascii.toString(),
  description:
    "May only contain ASCII characters. See https://www.w3schools.com/charsets/ref_html_ascii.asp",
}

export const email: Pattern = {
  regex: regexes.email.toString(),
  description: "Must be a valid email address",
}

export const base64: Pattern = {
  regex: regexes.base64.toString(),
  description:
    "May only contain base64 characters. See https://base64.guru/learn/base64-characters",
}
