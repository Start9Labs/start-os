import * as Base64 from 'base64-js'

export function encodeBase64(text: string) {
  return Base64.fromByteArray(new TextEncoder().encode(text))
}

export function decodeBase64(encoded: string) {
  return new TextDecoder().decode(Base64.toByteArray(encoded))
}
