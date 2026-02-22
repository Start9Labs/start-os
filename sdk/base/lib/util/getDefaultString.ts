import { DefaultString } from '../actions/input/inputSpecTypes'
import { getRandomString } from './getRandomString'

/**
 * Resolves a DefaultString spec into a concrete string value.
 * If the spec is a plain string, returns it directly.
 * If it is a random-string specification, generates a random string accordingly.
 *
 * @param defaultSpec - A string literal or a random-string generation spec
 * @returns The resolved default string value
 */
export function getDefaultString(defaultSpec: DefaultString): string {
  if (typeof defaultSpec === 'string') {
    return defaultSpec
  } else {
    return getRandomString(defaultSpec)
  }
}
