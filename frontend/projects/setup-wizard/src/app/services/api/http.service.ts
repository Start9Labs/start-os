import { Injectable } from '@angular/core'
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http'
import { firstValueFrom, Observable } from 'rxjs'
import * as aesjs from 'aes-js'
import * as pbkdf2 from 'pbkdf2'
import { HttpError, RpcError } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  fullUrl: string
  productKey?: string

  constructor(private readonly http: HttpClient) {
    const port = window.location.port
    this.fullUrl = `${window.location.protocol}//${window.location.hostname}:${port}/rpc/v1`
  }

  async rpcRequest<T>(body: RPCOptions, encrypted = true): Promise<T> {
    const httpOpts = {
      method: Method.POST,
      body,
      url: this.fullUrl,
    }

    let res: RPCResponse<T>

    if (encrypted) {
      res = await this.encryptedHttpRequest<RPCResponse<T>>(httpOpts)
    } else {
      res = await this.httpRequest<RPCResponse<T>>(httpOpts)
    }

    if (isRpcError(res)) {
      console.error('RPC ERROR: ', res)
      throw new RpcError(res.error)
    }

    return res.result
  }

  async encryptedHttpRequest<T>(httpOpts: {
    body: RPCOptions
    url: string
  }): Promise<T> {
    const urlIsRelative = httpOpts.url.startsWith('/')
    const url = urlIsRelative ? this.fullUrl + httpOpts.url : httpOpts.url

    const encryptedBody = await AES_CTR.encryptPbkdf2(
      this.productKey || '',
      encodeUtf8(JSON.stringify(httpOpts.body)),
    )
    const options = {
      responseType: 'arraybuffer',
      body: encryptedBody.buffer,
      observe: 'response',
      headers: {
        'Content-Encoding': 'aesctr256',
        'Content-Type': 'application/json',
      },
    } as any

    const req = this.http.post(url, options.body, options)

    return firstValueFrom(req)
      .then(res =>
        AES_CTR.decryptPbkdf2(
          this.productKey || '',
          (res as any).body as ArrayBuffer,
        ),
      )
      .then(res => JSON.parse(res))
      .catch(e => {
        if (!e.status && !e.statusText) {
          throw new EncryptionError()
        } else {
          throw new HttpError(e)
        }
      })
  }

  async httpRequest<T>(httpOpts: {
    body: RPCOptions
    url: string
  }): Promise<T> {
    const urlIsRelative = httpOpts.url.startsWith('/')
    const url = urlIsRelative ? this.fullUrl + httpOpts.url : httpOpts.url

    const options = {
      responseType: 'json',
      body: httpOpts.body,
      observe: 'response',
      headers: {
        'content-type': 'application/json',
        accept: 'application/json',
      },
    } as any

    const req: Observable<{ body: T }> = this.http.post(
      url,
      httpOpts.body,
      options,
    ) as any

    return firstValueFrom(req)
      .then(res => res.body)
      .catch(e => {
        throw new HttpError(e)
      })
  }
}

class EncryptionError {
  readonly code = null
  readonly message = 'Invalid Key'
  readonly details = null
}

function isRpcError<Error, Result>(
  arg: { error: Error } | { result: Result },
): arg is { error: Error } {
  return (arg as any).error !== undefined
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
  params?: {
    [param: string]: string | number | boolean | object | string[] | number[]
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
  withCredentials?: boolean
  body?: any
  timeout?: number
}

type AES_CTR = {
  encryptPbkdf2: (
    secretKey: string,
    messageBuffer: Uint8Array,
  ) => Promise<Uint8Array>
  decryptPbkdf2: (secretKey: string, arr: ArrayBuffer) => Promise<string>
}

export const AES_CTR: AES_CTR = {
  encryptPbkdf2: async (secretKey: string, messageBuffer: Uint8Array) => {
    const salt = window.crypto.getRandomValues(new Uint8Array(16))
    const counter = window.crypto.getRandomValues(new Uint8Array(16))

    const key = pbkdf2.pbkdf2Sync(secretKey, salt, 1000, 256 / 8, 'sha256')

    const aesCtr = new aesjs.ModeOfOperation.ctr(
      key,
      new aesjs.Counter(counter),
    )
    const encryptedBytes = aesCtr.encrypt(messageBuffer)
    return new Uint8Array([...counter, ...salt, ...encryptedBytes])
  },
  decryptPbkdf2: async (secretKey: string, arr: ArrayBuffer) => {
    const buff = new Uint8Array(arr)
    const counter = buff.slice(0, 16)
    const salt = buff.slice(16, 32)

    const cipher = buff.slice(32)
    const key = pbkdf2.pbkdf2Sync(secretKey, salt, 1000, 256 / 8, 'sha256')

    const aesCtr = new aesjs.ModeOfOperation.ctr(
      key,
      new aesjs.Counter(counter),
    )
    const decryptedBytes = aesCtr.decrypt(cipher)

    return aesjs.utils.utf8.fromBytes(decryptedBytes)
  },
}

export const encode16 = (buffer: Uint8Array) =>
  buffer.reduce((str, byte) => str + byte.toString(16).padStart(2, '0'), '')
export const decode16 = (hexString: string) =>
  new Uint8Array(
    hexString.match(/.{1,2}/g)?.map(byte => parseInt(byte, 16)) || [],
  )

export function encodeUtf8(str: string): Uint8Array {
  const encoder = new TextEncoder()
  return encoder.encode(str)
}

export function decodeUtf8(arr: Uint8Array): string {
  return new TextDecoder().decode(arr)
}
