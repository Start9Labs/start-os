import { Injectable } from '@angular/core'
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http'
import { Observable, from, interval, race } from 'rxjs'
import { map, take } from 'rxjs/operators'
import { ConfigService } from './config.service'

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  constructor (
    private readonly http: HttpClient,
    private readonly config: ConfigService,
  ) { }

  get raw () : HttpClient {
    return this.http
  }

  async serverRequest<T> (options: HttpOptions, overrides: Partial<{ version: string }> = { }): Promise<T> {
    options.url = leadingSlash(`${this.config.api.url}${exists(overrides.version) ? overrides.version : this.config.api.version}${options.url}`)
    if ( this.config.api.root && this.config.api.root !== '' ) {
      options.url = `${this.config.api.root}${options.url}`
    }
    return this.request<T>(options)
  }

  async request<T> (httpOpts: HttpOptions): Promise<T> {
    const { url, body, timeout, ...rest} = translateOptions(httpOpts)
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
              .catch(e => { console.error(e); throw humanReadableErrorMessage(e)})
  }
}

function humanReadableErrorMessage (e: any): Error {
  // server up, custom backend error
  if (e.error && e.error.message) return { ...e, message: e.error.message }
  if (e.message) return { ...e, message: e.message }
  if (e.status && e.statusText) return { ...e, message: `${e.status} ${e.statusText}` }
  return { ...e, message: `Unidentifiable HTTP exception` }
}

function leadingSlash (url: string): string {
  let toReturn = url
  toReturn = toReturn.startsWith('/') ? toReturn : '/' + toReturn
  toReturn = !toReturn.endsWith('/')  ? toReturn : toReturn.slice(0, -1)
  return toReturn
}

export enum Method {
  GET = 'GET',
  POST = 'POST',
  PUT = 'PUT',
  PATCH = 'PATCH',
  DELETE = 'DELETE',
}

export interface HttpOptions {
  withCredentials?: boolean
  url: string
  method: Method
  params?: {
    [param: string]: string | string[];
  }
  data?: any
  headers?: {
    [key: string]: string;
  }
  readTimeout?: number
}

export interface HttpJsonOptions {
  headers?: HttpHeaders | {
      [header: string]: string | string[];
  }
  observe: 'events'
  params?: HttpParams | {
      [param: string]: string | string[];
  }
  reportProgress?: boolean
  responseType?: 'json'
  withCredentials?: boolean
  body?: any
  url: string
  timeout: number
}

function translateOptions (httpOpts: HttpOptions): HttpJsonOptions {
  return {
    observe: 'events',
    responseType: 'json',
    reportProgress: false,
    withCredentials: true,
    headers: httpOpts.headers,
    params: httpOpts.params,
    body: httpOpts.data || { },
    url: httpOpts.url,
    timeout: httpOpts.readTimeout,
  }
}

function withTimeout<U> (req: Observable<U>, timeout: number): Observable<U> {
  return race(
    from(req.toPromise()), // this guarantees it only emits on completion, intermediary emissions are suppressed.
    interval(timeout).pipe(take(1), map(() => { throw new Error('timeout') })),
  )
}

function exists (str?: string): boolean {
  return !!str || str === ''
}