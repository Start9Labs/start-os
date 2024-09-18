import { DefaultString } from "../actions/input/inputSpecTypes"
import { getRandomString } from "./getRandomString"

export function getDefaultString(defaultSpec: DefaultString): string {
  if (typeof defaultSpec === "string") {
    return defaultSpec
  } else {
    return getRandomString(defaultSpec)
  }
}
