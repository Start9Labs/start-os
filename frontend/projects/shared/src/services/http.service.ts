import { Inject, Injectable } from '@angular/core'
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http'
import { HttpError, RpcError, WorkspaceConfig } from '@start9labs/shared'
import {
  firstValueFrom,
  from,
  interval,
  lastValueFrom,
  map,
  Observable,
  race,
  take,
} from 'rxjs'
import { DOCUMENT } from '@angular/common'

const {
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  relativeUrl = `/${api.url}/${api.version}`
  private fullUrl: string

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly http: HttpClient,
  ) {
    const { protocol, hostname, port } = this.document.location
    this.fullUrl = `${protocol}//${hostname}:${port}`
  }

  async rpcRequest<T>(opts: RPCOptions): Promise<T> {
    const { method, params, timeout } = opts

    const res = await this.httpRequest<RPCResponse<T>>({
      method: Method.POST,
      url: this.relativeUrl,
      body: { method, params },
      timeout,
    })
    if (isRpcError(res)) throw new RpcError(res.error)
    return res.result
  }

  async httpRequest<T>(opts: HttpOptions): Promise<T> {
    let { method, url, headers, body, responseType, timeout } = opts

    url = opts.url.startsWith('/') ? this.fullUrl + url : url

    const { params } = opts
    if (hasParams(params)) {
      Object.keys(params).forEach(key => {
        if (params[key] === undefined) {
          delete params[key]
        }
      })
    }

    const options: HttpAngularOptions = {
      observe: 'response',
      withCredentials: true,
      headers,
      params,
      responseType: responseType || 'json',
    }

    let req: Observable<{ body: T }>
    if (method === Method.GET) {
      req = this.http.get(url, options as any) as any
    } else {
      req = this.http.post(url, body, options as any) as any
    }

    return firstValueFrom(timeout ? withTimeout(req, timeout) : req)
      .then(res => res.body)
      .catch(e => {
        throw new HttpError(e)
      })
  }
}

// ** RPC types **

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

export interface RPCOptions {
  method: string
  params: {
    [param: string]:
      | string
      | number
      | boolean
      | object
      | string[]
      | number[]
      | null
  }
  timeout?: number
}

export function isRpcError<Error, Result>(
  arg: { error: Error } | { result: Result },
): arg is { error: Error } {
  return (arg as any).error !== undefined
}

// ** HTTP types **

export enum Method {
  GET = 'GET',
  POST = 'POST',
}

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
  responseType?: 'json' | 'text' | 'arrayBuffer'
  body?: any
  timeout?: number
}

interface HttpAngularOptions {
  observe: 'response'
  withCredentials: true
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
  responseType?: 'json' | 'text' | 'arrayBuffer'
}

function hasParams(
  params?: HttpOptions['params'],
): params is Record<string, string | string[]> {
  return !!params
}

function withTimeout<U>(req: Observable<U>, timeout: number): Observable<U> {
  return race(
    from(lastValueFrom(req)), // this guarantees it only emits on completion, intermediary emissions are suppressed.
    interval(timeout).pipe(
      take(1),
      map(() => {
        throw new Error('timeout')
      }),
    ),
  )
}

export interface RequestError {
  code: number
  message: string
  details: string
}
