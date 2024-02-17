import { DefaultString } from "../config/configTypes"
import { getRandomString } from "./getRandomString"

export function getDefaultString(defaultSpec: DefaultString): string {
  if (typeof defaultSpec === "string") {
    return defaultSpec
  } else {
    return getRandomString(defaultSpec)
  }
}
