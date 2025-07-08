import { arrayOf, string } from "ts-matches"

export const splitCommand = (
  command: string | [string, ...string[]],
): string[] => {
  if (arrayOf(string).test(command)) return command
  return ["sh", "-c", command]
}
