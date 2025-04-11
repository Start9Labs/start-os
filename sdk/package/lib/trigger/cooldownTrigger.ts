export function cooldownTrigger(timeMs: number) {
  return async function* () {
    while (true) {
      await new Promise((resolve) => setTimeout(resolve, timeMs))
      yield
    }
  }
}
