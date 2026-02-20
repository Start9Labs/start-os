export const splitCommand = (
  command: string | [string, ...string[]],
): string[] => {
  if (Array.isArray(command)) return command
  return ['sh', '-c', command]
}
