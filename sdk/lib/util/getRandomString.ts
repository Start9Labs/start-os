import { RandomString } from "../config/configTypes"
import { getRandomCharInSet } from "./getRandomCharInSet"

export function getRandomString(generator: RandomString): string {
  let s = ""
  for (let i = 0; i < generator.len; i++) {
    s = s + getRandomCharInSet(generator.charset)
  }

  return s
}
