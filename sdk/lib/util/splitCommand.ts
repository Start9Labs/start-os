import { arrayOf, string } from "ts-matches"
import { ValidIfNoStupidEscape } from "../types"

export const splitCommand = <A>(
  command: ValidIfNoStupidEscape<A> | [string, ...string[]],
): string[] => {
  if (arrayOf(string).test(command)) return command
  return String(command)
    .split('"')
    .flatMap((x, i) =>
      i % 2 !== 0
        ? [x]
        : x.split("'").flatMap((x, i) => (i % 2 !== 0 ? [x] : x.split(" "))),
    )
    .map((x) => x.trim())
    .filter(Boolean)
}
