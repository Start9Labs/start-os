import { HttpHeaders, HttpResponse } from '@angular/common/http'

export enum Method {
  GET = 'GET',
  POST = 'POST',
}

export interface HttpOptions {
  method: Method
  url: string
  headers?: {
    [header: string]: string | string[]
  }
  params?: {
    [param: string]: string | string[]
  }
  responseType?: 'json' | 'text' | 'arrayBuffer'
  body?: any
  timeout?: number
}

export interface HttpAngularOptions {
  observe: 'response'
  withCredentials: true
  headers?:
    | HttpHeaders
    | {
        [header: string]: string | string[]
      }
  params?: {
    [param: string]: string | string[]
  }
  responseType?: 'json' | 'text' | 'arrayBuffer'
}

export interface LocalHttpResponse<T> extends HttpResponse<T> {
  body: T
}

export interface RequestError {
  code: number
  message: string
  details: string
}
