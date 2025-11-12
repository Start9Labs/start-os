import { Injectable, inject } from '@angular/core'
import { HttpService } from './http.service'
import { AuthService } from './auth.service'

@Injectable({
  providedIn: 'root',
})
export class RpcService {
  private readonly http = inject(HttpService)
  private readonly auth = inject(AuthService)

  async request<T>(options: RPCOptions): Promise<T> {
    const { method, headers, params } = options

    const res = await this.http.request<RPCResponse<T>>({
      headers,
      body: { method, params },
    })

    const body = res.body as RPCResponse<T>

    if (isRpcError(body)) {
      if (body.error.code === 34) {
        console.error('Unauthenticated, logging out')
        this.auth.authenticated.set(false)
      }
      throw new RpcError(body.error)
    }

    return body.result
  }
}

export class RpcError {
  readonly code: number
  readonly message: string

  constructor(private readonly error: RPCErrorDetails) {
    this.code = this.error.code
    this.message = this.getMessage()
  }

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
