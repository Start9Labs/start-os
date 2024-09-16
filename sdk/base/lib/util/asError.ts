export const asError = (e: unknown) => {
  if (e instanceof Error) {
    return new Error(e as any)
  }
  return new Error(`${e}`)
}
