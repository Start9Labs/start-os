import { inject, Injectable } from '@angular/core'
import {
  ApiService,
  ExecReq,
  ExecRes,
  GetFileReq,
  GetFileRes,
  GetUciReq,
  GetUciRes,
  LoginReq,
  SetUciReq,
} from './api.service'
import { pauseFor } from '../../utils/pauseFor'
import { AuthService } from '../auth.service'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  private readonly auth = inject(AuthService)

  constructor() {
    super()
  }

  async login(params: LoginReq): Promise<null> {
    await pauseFor(1000)
    return null
  }

  async logout(): Promise<null> {
    await pauseFor(1000)
    return null
  }

  // exec
  async exec(params: ExecReq): Promise<ExecRes> {
    await pauseFor(1000)
    return {
      stdout: 'Success',
      stderr: '',
      exitCode: 0,
    }
  }

  // file
  async getFile(params: GetFileReq): Promise<GetFileRes> {
    await pauseFor(1000)
    return {
      contents: 'file contents',
      modified: new Date().toISOString(),
    }
  }

  async setFile(params: GetFileRes): Promise<null> {
    await pauseFor(1000)
    return null
  }

  // uci
  async getUci(params: GetUciReq): Promise<GetUciRes> {
    await pauseFor(1000)
    return {
      sections: [
        {
          type: 'rule',
          name: null,
          options: {},
          lists: {},
        },
      ],
      modified: new Date().toISOString(),
    }
  }

  async setUci(params: SetUciReq): Promise<null> {
    await pauseFor(1000)
    return null
  }
}
