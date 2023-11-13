// ** RPC types **

interface RPCBase {
  jsonrpc: '2.0'
  id: string
}

export interface RPCRequest<T> extends RPCBase {
  method: string
  params?: T
}

export interface RPCSuccessRes<T> extends RPCBase {
  result: T
}

export interface RPCErrorRes extends RPCBase {
  error: RPCErrorDetails
}

export interface RPCErrorDetails {
  code: number
  message: string
  data?:
    | {
        details: string
      }
    | string
}

export type RPCResponse<T> = RPCSuccessRes<T> | RPCErrorRes

export interface RPCOptions {
  method: string
  headers?: Record<string, string | string[]>
  params: Record<string, any>
  timeout?: number
}

export function isRpcError<Error, Result>(
  arg: { error: Error } | { result: Result },
): arg is { error: Error } {
  return (arg as any).error !== undefined
}
