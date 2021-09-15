import { Injectable } from '@angular/core'
import { HttpClient, HttpErrorResponse, HttpHeaders, HttpParams } from '@angular/common/http'
import { Observable, from, interval, race, Subject } from 'rxjs'
import { map, take } from 'rxjs/operators'
import * as aesjs from 'aes-js'
import * as pbkdf2 from 'pbkdf2'

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  fullUrl: string
  productKey: string

  constructor (
    private readonly http: HttpClient,
  ) {
    const port = window.location.port
    this.fullUrl = `${window.location.protocol}//${window.location.hostname}:${port}`
  }

  async rpcRequest<T> (body: RPCOptions): Promise<T> {

    const httpOpts = {
      method: Method.POST,
      body,
      url: this.fullUrl,
    }

    const res = await this.httpRequest<RPCResponse<T>>(httpOpts)

    if (isRpcError(res)) throw new RpcError(res.error)

    if (isRpcSuccess(res)) return res.result
  }

  async httpRequest<T> (httpOpts: {
    body: RPCOptions;
    url: string;
  }): Promise<T> {

    const urlIsRelative = httpOpts.url.startsWith('/')
    const url = urlIsRelative ?
      this.fullUrl + httpOpts.url :
      httpOpts.url

    const options = {
      responseType: 'arraybuffer',
      body: await AES_CTR.encryptPbkdf2(this.productKey, encodeUtf8( JSON.stringify(httpOpts.body))),
      observe: 'events',
      reportProgress: false,
      headers: {
        'Content-Encoding': 'aesctr256',
        'Content-Type': 'application/json'
      },
    } as any

    const req = this.http.post(url, httpOpts.body, options)

    return (withTimeout(req, 60000))
      .toPromise()
      .then(res => AES_CTR.decryptPbkdf2(this.productKey, new Uint8Array(res)))
      .then(res => JSON.parse(decodeUtf8(res)))
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
  responseType?: 'json' | 'text' | 'arrayBuffer'
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
  encryptPbkdf2: (secretKey: string, messageBuffer: Uint8Array) => Promise<Uint8Array>
  decryptPbkdf2: (secretKey, arr: Uint8Array) => Promise<Uint8Array>
}

export const AES_CTR: AES_CTR = {
  encryptPbkdf2: async (secretKey: string, messageBuffer: Uint8Array) =>  {
    const salt = window.crypto.getRandomValues(new Uint8Array(16))
    const counter = window.crypto.getRandomValues(new Uint8Array(16))
    const key = pbkdf2.pbkdf2Sync(secretKey, salt, 1, 256 / 8, 'sha256');
    var aesCtr = new aesjs.ModeOfOperation.ctr(key, new aesjs.Counter(counter));
    var encryptedBytes = aesCtr.encrypt(messageBuffer);
    return new Uint8Array([...counter,...salt,...encryptedBytes])
  },
  decryptPbkdf2: async (secretKey: string, arr: Uint8Array) =>  {
    const counter = arr.slice(0, 16)
    const salt = arr.slice(16, 32)
    const cipher = arr.slice(32)
    const key = pbkdf2.pbkdf2Sync(secretKey, salt, 1, 256 / 8, 'sha256');

    var aesCtr = new aesjs.ModeOfOperation.ctr(key, new aesjs.Counter(counter));
    return aesCtr.decrypt(cipher);
  },
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