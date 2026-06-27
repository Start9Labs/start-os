const loggedErrors = new WeakSet<object>()

export function logErrorOnce(err: unknown) {
  if (typeof err === 'object' && err !== null) {
    if (loggedErrors.has(err)) return
    loggedErrors.add(err)
  }
  console.error(err)
}
