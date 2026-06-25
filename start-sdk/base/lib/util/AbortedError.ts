export class AbortedError extends Error {
  readonly muteUnhandled = true as const
  declare cause?: unknown

  constructor(message?: string, options?: { cause?: unknown }) {
    super(message)
    this.name = 'AbortedError'
    if (options?.cause !== undefined) this.cause = options.cause
  }
}
