import { Inject, Injectable } from '@angular/core'
import { HttpClient } from '@angular/common/http'
import { HttpError } from '../classes/http-error'
import {
  HttpAngularOptions,
  HttpOptions,
  LocalHttpResponse,
  Method,
} from '../types/http.types'
import { RPCResponse, RPCOptions } from '../types/rpc.types'
import { WorkspaceConfig } from '../types/workspace-config'
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

  async rpcRequest<T>(
    opts: RPCOptions,
  ): Promise<LocalHttpResponse<RPCResponse<T>>> {
    const { method, headers, params, timeout } = opts

    return this.httpRequest<RPCResponse<T>>({
      method: Method.POST,
      url: this.relativeUrl,
      headers,
      body: { method, params },
      timeout,
    })
  }

  async httpRequest<T>(opts: HttpOptions): Promise<LocalHttpResponse<T>> {
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

    let req: Observable<LocalHttpResponse<T>>
    if (method === Method.GET) {
      req = this.http.get(url, options as any) as any
    } else {
      req = this.http.post(url, body, options as any) as any
    }

    return firstValueFrom(timeout ? withTimeout(req, timeout) : req).catch(
      e => {
        throw new HttpError(e)
      },
    )
  }
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
