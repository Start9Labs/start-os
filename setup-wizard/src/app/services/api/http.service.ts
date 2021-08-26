import { Injectable } from '@angular/core'
import { HttpClient, HttpErrorResponse, HttpHeaders, HttpParams } from '@angular/common/http'
import { Observable, from, interval, race, Subject } from 'rxjs'
import { map, take } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  private unauthorizedApiResponse$ = new Subject()
  fullUrl: string

  constructor (
    private readonly http: HttpClient,
  ) {
    const port = window.location.port
    this.fullUrl = `${window.location.protocol}//${window.location.hostname}:${port}`
  }

  watchUnauth$ (): Observable<{ }> {
    return this.unauthorizedApiResponse$.asObservable()
  }

  async rpcRequest<T> (rpcOpts: RPCOptions): Promise<T> {
    rpcOpts.params = rpcOpts.params || { }
    const httpOpts = {
      method: Method.POST,
      body: rpcOpts,
      url: `this.fullUrl`,
    }

    const res = await this.httpRequest<RPCResponse<T>>(httpOpts)

    if (isRpcError(res)) {
      if (res.error.code === 34) this.unauthorizedApiResponse$.next(true)
      throw new RpcError(res.error)
    }

    if (isRpcSuccess(res)) return res.result
  }

  async httpRequest<T> (httpOpts: HttpOptions): Promise<T> {
    if (httpOpts.withCredentials !== false) {
      httpOpts.withCredentials = true
    }

    const urlIsRelative = httpOpts.url.startsWith('/')
    const url = urlIsRelative ?
      this.fullUrl + httpOpts.url :
      httpOpts.url

    Object.keys(httpOpts.params || { }).forEach(key => {
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
      case Method.GET:    req = this.http.get(url, options) as any; break
      case Method.POST:   req = this.http.post(url, httpOpts.body, options) as any; break
      case Method.PUT:    req = this.http.put(url, httpOpts.body, options) as any; break
      case Method.PATCH:  req = this.http.patch(url, httpOpts.body, options) as any; break
      case Method.DELETE: req = this.http.delete(url, options) as any; break
    }

    return (httpOpts.timeout ? withTimeout(req, httpOpts.timeout) : req)
      .toPromise()
      .then(res => res.body)
      .catch(e => { throw new HttpError(e) })
  }
}

function RpcError (e: RPCError['error']): void {
  const { code, message, data } = e

  this.code = code
  this.message = message

  if (typeof data === 'string') {
    this.details = e.data
  } else {
    this.details = data.details
  }
}

function HttpError (e: HttpErrorResponse): void {
  const { status, statusText } = e

  this.code = status
  this.message = statusText
  this.details = null
}

function isRpcError<Error, Result> (arg: { error: Error } | { result: Result}): arg is { error: Error } {
  return !!(arg as any).error
}

function isRpcSuccess<Error, Result> (arg: { error: Error } | { result: Result}): arg is { result: Result } {
  return !!(arg as any).result
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
    } | string
  }
}

export type RPCResponse<T> = RPCSuccess<T> | RPCError

type HttpError = HttpErrorResponse & { error: { code: string, message: string } }

export interface HttpOptions {
  method: Method
  url: string
  headers?: HttpHeaders | {
    [header: string]: string | string[]
  }
  params?: HttpParams | {
    [param: string]: string | string[]
  }
  responseType?: 'json' | 'text'
  withCredentials?: boolean
  body?: any
  timeout?: number
}

function withTimeout<U> (req: Observable<U>, timeout: number): Observable<U> {
  return race(
    from(req.toPromise()), // this guarantees it only emits on completion, intermediary emissions are suppressed.
    interval(timeout).pipe(take(1), map(() => { throw new Error('timeout') })),
  )
}

type AES_CTR = {
  encryptPbkdf2: (secretKey: string, messageBuffer: Uint8Array) => Promise<{ cipher: Uint8Array, counter: Uint8Array, salt: Uint8Array }>
  decryptPbkdf2: (secretKey, a: { cipher: Uint8Array, counter: Uint8Array, salt: Uint8Array }) => Promise<Uint8Array>
}

export const AES_CTR: AES_CTR = {
  encryptPbkdf2: async (secretKey: string, messageBuffer: Uint8Array) =>  {

    const { key, salt } = await pbkdf2(secretKey, { name: 'AES-CTR', length: 256 })
    const counter = window.crypto.getRandomValues(new Uint8Array(16))
    const algorithm = { name: 'AES-CTR', counter, length: 64 }

    return window.crypto.subtle.encrypt(algorithm, key, messageBuffer)
      .then(encrypted => new Uint8Array(encrypted))
      .then(cipher => ({ cipher, counter, salt }))
  },
  decryptPbkdf2: async (secretKey: string, a: { cipher: Uint8Array, counter: Uint8Array, salt: Uint8Array }) =>  {
    const { cipher, counter, salt } = a
    const { key } = await pbkdf2(secretKey, { name: 'AES-CTR', length: 256 }, salt)
    const algorithm = { name: 'AES-CTR', counter, length: 64 };
    
    (window as any).stuff = { algorithm, key, cipher }
    return window.crypto.subtle.decrypt(algorithm, key, cipher)
      .then(decrypted => new Uint8Array(decrypted))
  },
}

async function pbkdf2 (secretKey: string, algorithm: AesKeyAlgorithm | HmacKeyGenParams, salt = window.crypto.getRandomValues(new Uint8Array(16))): Promise<{ salt: Uint8Array, key: CryptoKey, rawKey: Uint8Array }> {
  const usages: KeyUsage[] = algorithm.name === 'AES-CTR' ? [ 'encrypt', 'decrypt' ] : [ 'sign' ]
  const keyMaterial = await window.crypto.subtle.importKey(
    'raw',
    encodeUtf8(secretKey),
    'PBKDF2',
    false,
    ['deriveBits', 'deriveKey'],
  )

  const key = await window.crypto.subtle.deriveKey(
    {
      name: 'PBKDF2',
      salt,
      iterations: 100000,
      hash: 'SHA-256',
    },
    keyMaterial,
    algorithm,
    true,
    usages,
  )

  const rawKey = await window.crypto.subtle.exportKey('raw', key).then(r => new Uint8Array(r))
  return { salt, key, rawKey }
}

export const encode16 = (buffer: Uint8Array) => buffer.reduce((str, byte) => str + byte.toString(16).padStart(2, '0'), '')
export const decode16 = hexString => new Uint8Array(hexString.match(/.{1,2}/g).map(byte => parseInt(byte, 16)))

export function encodeUtf8 (str: string): Uint8Array {
  const encoder = new TextEncoder()
  return encoder.encode(str)
}

export function decodeUtf8 (arr: Uint8Array): string {
  return new TextDecoder().decode(arr);
}