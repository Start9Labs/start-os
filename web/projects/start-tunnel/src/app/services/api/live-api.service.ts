import { Injectable, DOCUMENT, inject } from '@angular/core'
import {
  HttpService,
  RPCOptions,
  isRpcError,
  RpcError,
  ErrorService,
} from '@start9labs/shared'
import { filter, firstValueFrom, Observable } from 'rxjs'
import { webSocket } from 'rxjs/webSocket'
import {
  AddForwardReq,
  ApiService,
  DeleteDeviceReq,
  DeleteForwardReq,
  DeleteSubnetReq,
  LoginReq,
  SubscribeRes,
  UpsertDeviceReq,
  UpsertSubnetReq,
} from './api.service'
import { AuthService } from '../auth.service'
import { PATCH_CACHE } from '../patch-db/patch-db-source'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  private readonly http = inject(HttpService)
  private readonly document = inject(DOCUMENT)
  private readonly auth = inject(AuthService)
  private readonly cache$ = inject(PATCH_CACHE)
  private readonly errorService = inject(ErrorService)

  constructor() {
    super()
  }

  openWebsocket$<T>(guid: string): Observable<T> {
    const { location } = this.document.defaultView!
    const host = location.host

    return webSocket({
      url: `wss://${host}/ws/rpc/${guid}`,
    })
  }

  async subscribe(): Promise<SubscribeRes> {
    return this.rpcRequest({ method: 'db.subscribe', params: {} })
  }

  // auth

  async login(params: LoginReq): Promise<null> {
    return this.rpcRequest({ method: 'auth.login', params })
  }

  async logout(): Promise<null> {
    return this.rpcRequest({ method: 'auth.logout', params: {} })
  }

  async setPassword(params: LoginReq): Promise<null> {
    return this.rpcRequest({ method: 'auth.set-password', params })
  }

  async addSubnet(params: UpsertSubnetReq): Promise<null> {
    return this.upsertSubnet(params)
  }

  async editSubnet(params: UpsertSubnetReq): Promise<null> {
    return this.upsertSubnet(params)
  }

  async deleteSubnet(params: DeleteSubnetReq): Promise<null> {
    return this.rpcRequest({ method: 'subnet.remove', params })
  }

  // devices

  async addDevice(params: UpsertDeviceReq): Promise<null> {
    return this.upsertDevice(params)
  }

  async editDevice(params: UpsertDeviceReq): Promise<null> {
    return this.upsertDevice(params)
  }

  async deleteDevice(params: DeleteDeviceReq): Promise<null> {
    return this.rpcRequest({ method: 'device.remove', params })
  }

  async showDeviceConfig(params: DeleteDeviceReq): Promise<string> {
    return this.rpcRequest({ method: 'device.show-config', params })
  }

  // forwards

  async addForward(params: AddForwardReq): Promise<null> {
    return this.rpcRequest({ method: 'port-forward.add', params })
  }

  async deleteForward(params: DeleteForwardReq): Promise<null> {
    return this.rpcRequest({ method: 'port-forward.remove', params })
  }

  // private

  private async upsertSubnet(params: UpsertSubnetReq): Promise<null> {
    return this.rpcRequest({ method: 'subnet.add', params })
  }

  private async upsertDevice(params: UpsertDeviceReq): Promise<null> {
    return this.rpcRequest({ method: 'device.add', params })
  }

  private async rpcRequest<T>(
    options: RPCOptions,
    urlOverride?: string,
  ): Promise<T> {
    const res = await this.http.rpcRequest<T>(options, urlOverride)
    const body = res.body

    if (isRpcError(body)) {
      if (body.error.code === 34) {
        console.error('Unauthenticated, logging out')
        this.auth.authenticated.set(false)
      }
      throw new RpcError(body.error)
    }

    const patchSequence = res.headers.get('x-patch-sequence')
    if (patchSequence)
      await firstValueFrom(
        this.cache$.pipe(filter(({ id }) => id >= Number(patchSequence))),
      )

    return body.result
  }
}
