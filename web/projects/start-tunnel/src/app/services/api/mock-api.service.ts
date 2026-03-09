import { inject, Injectable } from '@angular/core'
import { shareReplay, Subject, tap } from 'rxjs'
import { WebSocketSubject } from 'rxjs/webSocket'
import {
  AddForwardReq,
  ApiService,
  DeleteDeviceReq,
  DeleteForwardReq,
  DeleteSubnetReq,
  LoginReq,
  SubscribeRes,
  TunnelUpdateResult,
  UpdateForwardLabelReq,
  UpsertDeviceReq,
  UpsertSubnetReq,
} from './api.service'
import { pauseFor } from '@start9labs/shared'
import { AuthService } from '../auth.service'
import {
  AddOperation,
  Operation,
  PatchOp,
  RemoveOperation,
  ReplaceOperation,
  Revision,
} from 'patch-db-client'
import { toObservable } from '@angular/core/rxjs-interop'
import {
  mockTunnelData,
  PortForwardEntry,
  WgClient,
  WgSubnet,
} from '../patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  private readonly auth = inject(AuthService)
  readonly mockWsSource$ = new Subject<Revision>()
  sequence = 1

  constructor() {
    super()
    toObservable(this.auth.authenticated)
      .pipe(
        tap(() => {
          this.sequence = 1
        }),
      )
      .subscribe()
  }

  openWebsocket$<T>(guid: string): WebSocketSubject<T> {
    return this.mockWsSource$.pipe(
      shareReplay({ bufferSize: 1, refCount: true }),
    ) as WebSocketSubject<T>
  }

  async subscribe(): Promise<SubscribeRes> {
    await pauseFor(1000)
    return {
      dump: { id: 1, value: mockTunnelData },
      guid: 'patch-db-guid',
    }
  }

  async login(params: LoginReq): Promise<null> {
    await pauseFor(1000)
    return null
  }

  async logout(): Promise<null> {
    await pauseFor(1000)
    return null
  }

  async setPassword(params: LoginReq): Promise<null> {
    await pauseFor(1000)
    return null
  }

  async addSubnet(params: UpsertSubnetReq): Promise<null> {
    await pauseFor(1000)

    const patch: AddOperation<WgSubnet>[] = [
      {
        op: PatchOp.ADD,
        path: `/wg/subnets/${replaceSlashes(params.subnet)}`,
        value: { name: params.name, clients: {} },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async editSubnet(params: UpsertSubnetReq): Promise<null> {
    await pauseFor(1000)

    const patch: ReplaceOperation<string>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/wg/subnets/${replaceSlashes(params.subnet)}/name`,
        value: params.name,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async deleteSubnet(params: DeleteSubnetReq): Promise<null> {
    await pauseFor(1000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/wg/subnets/${replaceSlashes(params.subnet)}`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async addDevice(params: UpsertDeviceReq): Promise<null> {
    await pauseFor(1000)

    const patch: AddOperation<WgClient>[] = [
      {
        op: PatchOp.ADD,
        path: `/wg/subnets/${replaceSlashes(params.subnet)}/clients/${params.ip}`,
        value: { name: params.name },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async editDevice(params: UpsertDeviceReq): Promise<null> {
    await pauseFor(1000)

    const patch: ReplaceOperation<string>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/wg/subnets/${replaceSlashes(params.subnet)}/clients/${params.ip}/name`,
        value: params.name,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async deleteDevice(params: DeleteDeviceReq): Promise<null> {
    await pauseFor(1000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/wg/subnets/${replaceSlashes(params.subnet)}/clients/${params.ip}`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async showDeviceConfig(params: DeleteDeviceReq): Promise<string> {
    await pauseFor(1000)

    return MOCK_CONFIG
  }

  async addForward(params: AddForwardReq): Promise<null> {
    await pauseFor(1000)

    const patch: AddOperation<PortForwardEntry>[] = [
      {
        op: PatchOp.ADD,
        path: `/portForwards/${params.source}`,
        value: { target: params.target, label: params.label || '' },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async updateForwardLabel(params: UpdateForwardLabelReq): Promise<null> {
    await pauseFor(1000)

    const patch: ReplaceOperation<string>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/portForwards/${params.source}/label`,
        value: params.label,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async deleteForward(params: DeleteForwardReq): Promise<null> {
    await pauseFor(1000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/portForwards/${params.source}`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async checkUpdate(): Promise<TunnelUpdateResult> {
    await pauseFor(1000)
    return {
      status: 'update-available',
      installed: '0.4.0-alpha.19',
      candidate: '0.4.0-alpha.20',
    }
  }

  async applyUpdate(): Promise<TunnelUpdateResult> {
    await pauseFor(2000)
    return {
      status: 'updating',
      installed: '0.4.0-alpha.19',
      candidate: '0.4.0-alpha.20',
    }
  }

  private async mockRevision<T>(patch: Operation<T>[]): Promise<void> {
    const revision = {
      id: ++this.sequence,
      patch,
    }
    this.mockWsSource$.next(revision)
  }
}

function replaceSlashes(val: string) {
  return val.replace(new RegExp('/', 'g'), '~1')
}

const MOCK_CONFIG = `[Interface]
# Server's private IP address for the WireGuard VPN subnet
Address = 10.20.10.1/24
# UDP port WireGuard listens on
ListenPort = 33333
# Server private key (generated)
PrivateKey = 4K68mdpQWdEz/FpdVuRoZYgWpQgpW63J9GFzn+iOulQ=

# Commands to run after starting/stopping WireGuard tunnel to enable forwarding and NAT (example)
PostUp = iptables -A FORWARD -i %i -j ACCEPT; iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE
PostDown = iptables -D FORWARD -i %i -j ACCEPT; iptables -t nat -D POSTROUTING -o eth0 -j MASQUERADE

# Add client peers below with their public keys and allowed IPs
[Peer]
# Client public key
PublicKey = MQBiYHxAj7u8paj3L4w4uav3P/9YBPbaN4gkWn90SSs=
# Allowed client IP address within VPN subnet`
