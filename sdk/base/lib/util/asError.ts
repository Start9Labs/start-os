export const asError = (e: unknown) => {
  if (e instanceof Error) {
    return new Error(e as any)
  }
  if (typeof e === "string") {
    return new Error(`${e}`)
  }
  return new Error(`${JSON.stringify(e)}`)
}
