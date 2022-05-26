import { RpcErrorDetails } from '../types/rpc-error-details'

export class RpcError<T> {
  readonly code = this.error.code
  readonly message = this.getMessage()
  readonly revision = this.getRevision()

  constructor(private readonly error: RpcErrorDetails<T>) {}

  private getMessage(): string {
    if (typeof this.error.data === 'string') {
      return `${this.error.message}\n\n${this.error.data}`
    }

    return this.error.data?.details
      ? `${this.error.message}\n\n${this.error.data.details}`
      : this.error.message
  }

  private getRevision(): T | null {
    return typeof this.error.data === 'string'
      ? null
      : this.error.data?.revision || null
  }
}
