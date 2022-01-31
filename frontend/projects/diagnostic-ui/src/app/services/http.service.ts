import { Injectable } from '@angular/core'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'

@Injectable({
  providedIn: 'root',
})
export class HttpService {

  constructor (
    private readonly http: HttpClient,
  ) { }

  async rpcRequest<T> (options: RPCOptions): Promise<T> {
    const res = await this.httpRequest<RPCResponse<T>>(options)
    if (isRpcError(res)) throw new RpcError(res.error)
    if (isRpcSuccess(res)) return res.result
  }

  async httpRequest<T> (body: RPCOptions): Promise<T> {
    const url = `${window.location.protocol}//${window.location.hostname}:${window.location.port}/rpc/v1`
    return this.http.post(url, body)
      .toPromise().then(a => a as T)
      .catch(e => { throw new HttpError(e) })
  }
}

function RpcError (e: RPCError['error']): void {
  const { code, message, data } = e

  this.code = code
  this.message = message

  if (typeof data === 'string') {
    this.details = e.data
    this.revision = null
  } else {
    this.details = data.details
  }
}

function HttpError (e: HttpErrorResponse): void {
  const { status, statusText } = e

  this.code = status
  this.message = statusText
  this.details = null
  this.revision = null
}

function isRpcError<Error, Result> (arg: { error: Error } | { result: Result}): arg is { error: Error } {
  return !!(arg as any).error
}

function isRpcSuccess<Error, Result> (arg: { error: Error } | { result: Result}): arg is { result: Result } {
  return !!(arg as any).result
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
    code: number,
    message: string
    data?: {
      details: string
    } | string
  }
}

export type RPCResponse<T> = RPCSuccess<T> | RPCError

type HttpError = HttpErrorResponse & { error: { code: string, message: string } }
