import { RPCErrorDetails } from '../types/rpc.types'

export class RpcError {
  readonly code = this.error.code
  readonly message = this.getMessage()

  constructor(private readonly error: RPCErrorDetails) {}

  private getMessage(): string {
    let message: string

    if (typeof this.error.data === 'string') {
      message = `${this.error.message}\n\n${this.error.data}`
    } else {
      message = this.error.data?.details
        ? `${this.error.message}\n\n${this.error.data.details}`
        : this.error.message
    }

    return `RPC ERROR: ${message}`
  }
}
