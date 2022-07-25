import { Injectable } from '@angular/core'
import { HttpClient } from '@angular/common/http'
import { HttpError, RpcError } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  constructor(private readonly http: HttpClient) {}

  async rpcRequest<T>(options: RPCOptions): Promise<T> {
    const res = await this.httpRequest<RPCResponse<T>>(options)
    if (isRpcError(res)) throw new RpcError(res.error)
    return res.result
  }

  async httpRequest<T>(body: RPCOptions): Promise<T> {
    const url = `${window.location.protocol}//${window.location.hostname}:${window.location.port}/rpc/v1`
    return this.http
      .post(url, body)
      .toPromise()
      .then(a => a as T)
      .catch(e => {
        throw new HttpError(e)
      })
  }
}

function isRpcError<Error, Result>(
  arg: { error: Error } | { result: Result },
): arg is { error: Error } {
  return (arg as any).error !== undefined
}

export interface RPCOptions {
  method: string
  params: { [param: string]: Params }
}

export interface RequestError {
  code: number
  message: string
  details: string
}

export type Params = string | number | boolean | object | string[] | number[]

interface RPCBase {
  jsonrpc: '2.0'
  id: string
}

export interface RPCRequest<T> extends RPCBase {
  method: string
  params?: T
}

export interface RPCSuccess<T> extends RPCBase {
  result: T
}

export interface RPCError extends RPCBase {
  error: {
    code: number
    message: string
    data?:
      | {
          details: string
        }
      | string
  }
}

export type RPCResponse<T> = RPCSuccess<T> | RPCError
