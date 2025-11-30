import { Injectable, DOCUMENT, inject } from '@angular/core'
import { HttpClient, HttpResponse } from '@angular/common/http'
import { firstValueFrom } from 'rxjs'
import { HttpErrorResponse } from '@angular/common/http'
import { InjectionToken } from '@angular/core'

export const RELATIVE_URL = new InjectionToken<string>(
  'Relative URL for requests',
)

@Injectable({
  providedIn: 'root',
})
export class HttpService {
  private readonly url = inject(DOCUMENT).location.origin + inject(RELATIVE_URL)
  private readonly http = inject(HttpClient)

  async request<T>(options: HttpOptions): Promise<HttpResponse<T>> {
    const { params, body, headers } = options

    if (hasParams(params)) {
      Object.keys(params).forEach(key => {
        if (params[key] === undefined) {
          delete params[key]
        }
      })
    }

    try {
      return firstValueFrom(
        this.http.post<T>(this.url, body, {
          observe: 'response',
          withCredentials: true,
          params,
          headers,
          responseType: 'json',
        }),
      )
    } catch (e: any) {
      throw new HttpError(e)
    }
  }
}

function hasParams(
  params?: HttpOptions['params'],
): params is Record<string, string | string[]> {
  return !!params
}

export class HttpError {
  readonly code: number
  readonly message: string

  constructor(private readonly error: HttpErrorResponse) {
    this.code = this.error.status
    this.message = this.error.statusText
  }
}

type ParamPrimitive = string | number | boolean

export type HttpOptions = {
  headers?: {
    [header: string]: string | string[]
  }
  params?: {
    [param: string]: ParamPrimitive | ParamPrimitive[]
  }
  body?: any
}
