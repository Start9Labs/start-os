import { RPCErrorDetails } from '../types/rpc.types'

export class RpcError {
  readonly code = this.error.code
  readonly message = this.getMessage()

  constructor(private readonly error: RPCErrorDetails) {}

  private getMessage(): string {
    if (typeof this.error.data === 'string') {
      return `${this.error.message}\n\n${this.error.data}`
    }

    return this.error.data?.details
      ? `${this.error.message}\n\n${this.error.data.details}`
      : this.error.message
  }
}
