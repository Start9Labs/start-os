import { Injectable } from '@angular/core'
import {
  HttpClient,
  HttpErrorResponse,
  HttpHeaders,
  HttpParams,
} from '@angular/common/http'
import { Observable, from, interval, race } from 'rxjs'
import { map, take } from 'rxjs/operators'
import { ConfigService } from './config.service'
import { Revision } from 'patch-db-client'
import { AuthService } from './auth.service'
import { HttpError, RpcError } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  fullUrl: string

  constructor(
    private readonly http: HttpClient,
    private readonly config: ConfigService,
    private readonly auth: AuthService,
  ) {
    const port = window.location.port
    this.fullUrl = `${window.location.protocol}//${window.location.hostname}:${port}`
  }

  async rpcRequest<T>(rpcOpts: RPCOptions): Promise<T> {
    const { url, version } = this.config.api
    rpcOpts.params = rpcOpts.params || {}
    const httpOpts: HttpOptions = {
      method: Method.POST,
      body: rpcOpts,
      url: `/${url}/${version}`,
    }
    if (rpcOpts.timeout) httpOpts.timeout = rpcOpts.timeout

    const res = await this.httpRequest<RPCResponse<T>>(httpOpts)
    if (isRpcError(res)) {
      if (res.error.code === 34) this.auth.setUnverified()
      throw new RpcError(res.error)
    }

    if (isRpcSuccess(res)) return res.result
  }

  async httpRequest<T>(httpOpts: HttpOptions): Promise<T> {
    if (httpOpts.withCredentials !== false) {
      httpOpts.withCredentials = true
    }

    const urlIsRelative = httpOpts.url.startsWith('/')
    const url = urlIsRelative ? this.fullUrl + httpOpts.url : httpOpts.url

    Object.keys(httpOpts.params || {}).forEach(key => {
      if (httpOpts.params[key] === undefined) {
        delete httpOpts.params[key]
      }
    })

    const options = {
      responseType: httpOpts.responseType || 'json',
      body: httpOpts.body,
      observe: 'events',
      reportProgress: false,
      withCredentials: httpOpts.withCredentials,
      headers: httpOpts.headers,
      params: httpOpts.params,
      timeout: httpOpts.timeout,
    } as any

    let req: Observable<{ body: T }>
    switch (httpOpts.method) {
      case Method.GET:
        req = this.http.get(url, options) as any
        break
      case Method.POST:
        req = this.http.post(url, httpOpts.body, options) as any
        break
      case Method.PUT:
        req = this.http.put(url, httpOpts.body, options) as any
        break
      case Method.PATCH:
        req = this.http.patch(url, httpOpts.body, options) as any
        break
      case Method.DELETE:
        req = this.http.delete(url, options) as any
        break
    }

    return (httpOpts.timeout ? withTimeout(req, httpOpts.timeout) : req)
      .toPromise()
      .then(res => res.body)
      .catch(e => {
        throw new HttpError(e)
      })
  }
}

function isRpcError<Error, Result>(
  arg: { error: Error } | { result: Result },
): arg is { error: Error } {
  return !!(arg as any).error
}

function isRpcSuccess<Error, Result>(
  arg: { error: Error } | { result: Result },
): arg is { result: Result } {
  return !!(arg as any).result
}

export interface RequestError {
  code: number
  message: string
  details: string
  revision: Revision | null
}

export enum Method {
  GET = 'GET',
  POST = 'POST',
  PUT = 'PUT',
  PATCH = 'PATCH',
  DELETE = 'DELETE',
}

export interface RPCOptions {
  method: string
  params?: object
  timeout?: number
}

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
          revision: Revision | null
          debug: string | null
        }
      | string
  }
}

export type RPCResponse<T> = RPCSuccess<T> | RPCError

export interface HttpOptions {
  method: Method
  url: string
  headers?:
    | HttpHeaders
    | {
        [header: string]: string | string[]
      }
  params?:
    | HttpParams
    | {
        [param: string]: string | string[]
      }
  responseType?: 'json' | 'text'
  withCredentials?: boolean
  body?: any
  timeout?: number
}

function withTimeout<U>(req: Observable<U>, timeout: number): Observable<U> {
  return race(
    from(req.toPromise()), // this guarantees it only emits on completion, intermediary emissions are suppressed.
    interval(timeout).pipe(
      take(1),
      map(() => {
        throw new Error('timeout')
      }),
    ),
  )
}
