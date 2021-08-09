import { Injectable } from '@angular/core'
import { HttpClient, HttpErrorResponse, HttpHeaders, HttpParams } from '@angular/common/http'
import { Observable, from, interval, race, Subject } from 'rxjs'
import { map, take } from 'rxjs/operators'
import { ConfigService } from './config.service'
import { Revision } from 'patch-db-client'

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  private unauthorizedApiResponse$ = new Subject()
  fullUrl: string

  constructor (
    private readonly http: HttpClient,
    private readonly config: ConfigService,
  ) {
    const port = config.mocks.enabled ? this.config.mocks.rpcPort : window.location.port
    this.fullUrl = `${window.location.protocol}//${window.location.hostname}:${port}`
  }

  watchUnauth$ (): Observable<{ }> {
    return this.unauthorizedApiResponse$.asObservable()
  }

  async rpcRequest<T> (rpcOpts: RPCOptions): Promise<T> {
    const { url, version } = this.config.api
    rpcOpts.params = rpcOpts.params || { }
    const httpOpts = {
      method: Method.POST,
      data: rpcOpts,
      url: `/${url}/${version}`,
    }

    const res = await this.httpRequest<RPCResponse<T>>(httpOpts)

    if (isRpcError(res)) {
      if (res.error.code === 34) this.unauthorizedApiResponse$.next(true)
      throw new RpcError(res.error)
    }

    if (isRpcSuccess(res)) return res.result
  }

  async httpRequest<T> (httpOpts: HttpOptions): Promise<T> {
    let { body, timeout, url, ...rest} = this.translateOptions(httpOpts)
    let req: Observable<{ body: T }>
    switch (httpOpts.method){
      case Method.GET:    req = this.http.get(url, rest) as any;          break
      case Method.POST:   req = this.http.post(url, body, rest) as any;   break
      case Method.PUT:    req = this.http.put(url, body, rest) as any;    break
      case Method.PATCH:  req = this.http.patch(url, body, rest) as any;  break
      case Method.DELETE: req = this.http.delete(url, rest) as any;       break
    }

    return (timeout ? withTimeout(req, timeout) : req)
      .toPromise()
      .then(res => res.body)
      .catch(e => { throw new HttpError(e) })
  }

  private translateOptions (httpOpts: HttpOptions): HttpJsonOptions {
    if (httpOpts.withCredentials !== false) {
      httpOpts.withCredentials = this.config.mocks.enabled ? false : true
    }

    const urlIsRelative = !httpOpts.url || httpOpts.url.startsWith('/')
    const url = urlIsRelative ?
      this.fullUrl + httpOpts.url :
      httpOpts.url

    Object.keys(httpOpts.params || { }).forEach(key => {
      if (httpOpts.params[key] === undefined) {
        delete httpOpts.params[key]
      }
    })

    return {
      observe: 'events',
      responseType: 'json',
      reportProgress: false,
      withCredentials: httpOpts.withCredentials,
      headers: httpOpts.headers,
      params: httpOpts.params,
      body: httpOpts.data || { },
      url,
      timeout: httpOpts.readTimeout,
    }
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
    this.revision = data.revision
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
  // @TODO what are valid params? object, bool?
  params?: {
    [param: string]: string | number | boolean | object | string[] | number[];
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

export interface RPCSuccess<T> extends RPCBase {
  result: T
}

export interface RPCError extends RPCBase {
  error: {
    code: number,
    message: string
    data?: {
      details: string
      revision: Revision | null
    } | string
  }
}

export type RPCResponse<T> = RPCSuccess<T> | RPCError

type HttpError = HttpErrorResponse & { error: { code: string, message: string } }

export interface HttpOptions {
  withCredentials?: boolean
  method: Method
  params?: {
    [param: string]: string | string[]
  }
  data?: any
  headers?: {
    [key: string]: string;
  }
  url: string
  readTimeout?: number
}

export interface HttpJsonOptions {
  headers?: HttpHeaders | {
      [header: string]: string | string[]
  }
  observe: 'events'
  params?: HttpParams | {
      [param: string]: string | string[]
  }
  reportProgress?: boolean
  responseType?: 'json'
  withCredentials?: boolean
  body?: any
  url: string
  timeout: number
}

function withTimeout<U> (req: Observable<U>, timeout: number): Observable<U> {
  return race(
    from(req.toPromise()), // this guarantees it only emits on completion, intermediary emissions are suppressed.
    interval(timeout).pipe(take(1), map(() => { throw new Error('timeout') })),
  )
}
