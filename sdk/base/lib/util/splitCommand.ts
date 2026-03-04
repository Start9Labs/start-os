import { AllowReadonly } from '../types'

/**
 * Normalizes a command into an argv-style string array.
 * If given a string, wraps it as `["sh", "-c", command]`.
 * If given a tuple, returns it as-is.
 *
 * @param command - A shell command string or a pre-split argv tuple
 * @returns An argv-style string array suitable for process execution
 *
 * @example
 * ```ts
 * splitCommand("echo hello")           // ["sh", "-c", "echo hello"]
 * splitCommand(["node", "index.js"])   // ["node", "index.js"]
 * ```
 */
export const splitCommand = (
  command: string | AllowReadonly<[string, ...string[]]>,
): string[] => {
  if (Array.isArray(command)) return command
  return ['sh', '-c', command as string]
}
