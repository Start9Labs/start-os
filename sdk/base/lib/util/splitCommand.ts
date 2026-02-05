import { arrayOf, string } from "ts-matches"

/**
 * Normalizes a command into an array format suitable for execution.
 *
 * If the command is already an array, it's returned as-is.
 * If it's a string, it's wrapped in `sh -c` for shell interpretation.
 *
 * @param command - Command as string or array of strings
 * @returns Command as array suitable for process execution
 *
 * @example
 * ```typescript
 * splitCommand(['nginx', '-g', 'daemon off;'])
 * // Returns: ['nginx', '-g', 'daemon off;']
 *
 * splitCommand('nginx -g "daemon off;"')
 * // Returns: ['sh', '-c', 'nginx -g "daemon off;"']
 * ```
 */
export const splitCommand = (
  command: string | [string, ...string[]],
): string[] => {
  if (arrayOf(string).test(command)) return command
  return ["sh", "-c", command]
}
