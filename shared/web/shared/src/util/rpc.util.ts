import { RPCErrorDetails } from '../types/rpc.types'

export function getRpcErrorMessage(error: RPCErrorDetails): string {
  if (typeof error.data === 'string') {
    return `${error.message}\n\n${error.data}`
  }

  return error.data?.details
    ? `${error.message}\n\n${error.data.details}`
    : error.message
}
