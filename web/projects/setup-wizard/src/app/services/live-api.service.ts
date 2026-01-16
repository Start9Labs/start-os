import { Inject, Injectable, DOCUMENT } from '@angular/core'
import {
  DiskInfo,
  encodeBase64,
  FollowLogsRes,
  FullKeyboard,
  HttpService,
  isRpcError,
  RpcError,
  RPCOptions,
  SetLanguageParams,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import * as jose from 'node-jose'
import { Observable } from 'rxjs'
import { webSocket } from 'rxjs/webSocket'
import { ApiService } from './api.service'
import {
  SetupStatusRes,
  InstallOsParams,
  InstallOsRes,
  AttachParams,
  SetupExecuteParams,
  SetupCompleteRes,
  EchoReq,
} from '../types'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  constructor(
    private readonly http: HttpService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {
    super()
  }

  openWebsocket$<T>(guid: string): Observable<T> {
    const { location } = this.document.defaultView!
    const protocol = location.protocol === 'http:' ? 'ws' : 'wss'
    const host = location.host

    return webSocket({
      url: `${protocol}://${host}/ws/rpc/${guid}`,
    })
  }

  async echo(params: EchoReq, url: string): Promise<string> {
    return this.rpcRequest({ method: 'echo', params }, url)
  }

  async getStatus() {
    return this.rpcRequest<SetupStatusRes>({
      method: 'setup.status',
      params: {},
    })
  }

  async getPubKey() {
    const response: jose.JWK.Key = await this.rpcRequest({
      method: 'setup.get-pubkey',
      params: {},
    })
    this.pubkey = response
  }

  async setKeyboard(params: FullKeyboard): Promise<null> {
    return this.rpcRequest({
      method: 'setup.set-keyboard',
      params,
    })
  }

  async setLanguage(params: SetLanguageParams): Promise<null> {
    return this.rpcRequest({
      method: 'setup.set-language',
      params,
    })
  }

  async getDisks() {
    return this.rpcRequest<DiskInfo[]>({
      method: 'setup.disk.list',
      params: {},
    })
  }

  async installOs(params: InstallOsParams) {
    return this.rpcRequest<InstallOsRes>({
      method: 'setup.install-os',
      params,
    })
  }

  async verifyCifs(source: T.VerifyCifsParams) {
    source.path = source.path.replace('/\\/g', '/')
    return this.rpcRequest<Record<string, StartOSDiskInfo>>({
      method: 'setup.cifs.verify',
      params: source,
    })
  }

  async attach(params: AttachParams) {
    return this.rpcRequest<T.SetupProgress>({
      method: 'setup.attach',
      params,
    })
  }

  async execute(params: SetupExecuteParams) {
    if (params.recoverySource?.type === 'backup') {
      const target = params.recoverySource.target
      if (target.type === 'cifs') {
        target.path = target.path.replace('/\\/g', '/')
      }
    }

    return this.rpcRequest<T.SetupProgress>({
      method: 'setup.execute',
      params,
    })
  }

  async initFollowLogs() {
    return this.rpcRequest<FollowLogsRes>({
      method: 'setup.logs.follow',
      params: {},
    })
  }

  async complete() {
    const res = await this.rpcRequest<SetupCompleteRes>({
      method: 'setup.complete',
      params: {},
    })

    return {
      ...res,
      rootCa: encodeBase64(res.rootCa),
    }
  }

  async exit() {
    await this.rpcRequest<void>({
      method: 'setup.exit',
      params: {},
    })
  }

  async restart() {
    await this.rpcRequest<void>({
      method: 'setup.restart',
      params: {},
    })
  }

  private async rpcRequest<T>(opts: RPCOptions, url?: string): Promise<T> {
    const res = await this.http.rpcRequest<T>(opts, url)
    const rpcRes = res.body

    if (isRpcError(rpcRes)) {
      throw new RpcError(rpcRes.error)
    }

    return rpcRes.result
  }
}
