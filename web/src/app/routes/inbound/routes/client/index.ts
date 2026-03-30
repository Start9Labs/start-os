import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { ActivatedRoute, RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiLink,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiBadge, TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Placeholder } from 'src/app/components/placeholder'
import { DevicesApiService } from 'src/app/routes/devices/service'
import { InboundService, VpnServerPeer } from '../../service'

import { CLIENT_CONFIG } from './dialog-config'
import { ADD_CLIENT, ClientDialogData } from './dialog-add'
import { RENAME_CLIENT } from './dialog-rename'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>
          <a
            tuiLink
            routerLink=".."
            appearance=""
            iconStart="@tui.chevron-left"
            [style.font]="'inherit'"
            [style.text-decoration]="'none'"
            [tuiSkeleton]="!server()"
          >
            {{ server()?.label }}
            (client devices)
          </a>
        </h2>
      </hgroup>
      <aside tuiAccessories>
        <button
          tuiButton
          iconStart="@tui.plus"
          [disabled]="!deviceIps()"
          (click)="add()"
        >
          Add
        </button>
      </aside>
    </header>
    <table tuiTable class="g-table" [tuiSkeleton]="!server()">
      <thead>
        <tr>
          <th tuiTh [sorter]="'name' | tuiSorter">Name</th>
          <th tuiTh [sorter]="'ip' | tuiSorter">LAN IP Address</th>
          <th tuiTh>Routing</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of server()?.peers | tuiTableSort; track item.public_key) {
          <tr>
            <td tuiTd>{{ item.name }}</td>
            <td tuiTd>{{ item.ip }}</td>
            <td tuiTd>
              <span tuiBadge [appearance]="item.route_all ? 'info' : 'neutral'">
                {{ item.route_all ? 'All traffic' : 'LAN only' }}
              </span>
            </td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.ellipsis-vertical"
                appearance="icon"
                tuiDropdownAlign="end"
                tuiDropdownAuto
                tuiDropdown
              >
                Actions
                <tui-data-list
                  *tuiDropdown="let close"
                  size="s"
                  (click)="close()"
                >
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    (click)="rename(item)"
                  >
                    Rename
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.repeat"
                    (click)="changeRouting(item)"
                  >
                    {{
                      item.route_all
                        ? 'Switch to LAN only'
                        : 'Switch to all traffic'
                    }}
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.file-text"
                    (click)="viewConfig(item)"
                  >
                    View Config
                  </button>
                  <button
                    tuiOption
                    class="g-negative"
                    iconStart="@tui.trash"
                    (click)="delete(item)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="4">
              <app-placeholder icon="@tui.monitor-smartphone">
                No clients configured
              </app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      max-width: 36rem;
    }

    span {
      display: inline-block;
      min-width: 4rem;
      border-radius: var(--tui-radius-xs);
    }

    td:last-child {
      text-align: end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    RouterLink,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiButton,
    TuiTable,
    TuiSkeleton,
    TuiDataList,
    TuiDropdown,
    TuiBadge,
    Placeholder,
  ],
})
export default class InboundClients {
  private readonly service = inject(InboundService)
  private readonly devices = inject(DevicesApiService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly listenPort = Number(
    inject(ActivatedRoute).snapshot.queryParams['port'],
  )

  protected readonly deviceIps = signal<string[] | null>(null)
  protected readonly server = computed(() =>
    this.service.data()?.find(item => item.listen_port === this.listenPort),
  )

  constructor() {
    this.devices.get().then(devices => {
      this.deviceIps.set(devices.map(d => d.ipv4).filter(Boolean) as string[])
    })
  }

  add() {
    const server = this.server()
    if (!server) return

    const peerIps = server.peers.map(p => p.ip).filter(Boolean) as string[]
    const usedIps = [
      ...new Set([
        server.server_address,
        ...(this.deviceIps() ?? []),
        ...peerIps,
      ]),
    ]

    this.dialogs
      .open<VpnServerPeer>(ADD_CLIENT, {
        label: 'Add client device',
        data: {
          serverAddress: server.server_address,
          usedIps,
        } satisfies ClientDialogData,
      })
      .subscribe(async peer => {
        const response = await this.service.addPeer(server.profile, peer)
        if (response?.client_config) {
          this.dialogs
            .open(CLIENT_CONFIG, {
              data: {
                name: peer.name,
                config: response.client_config,
              },
            })
            .subscribe()
        }
      })
  }

  rename(peer: VpnServerPeer) {
    const profile = this.server()?.profile
    if (!profile) return

    this.dialogs
      .open<string>(RENAME_CLIENT, {
        label: 'Rename Device',
        data: peer.name,
      })
      .subscribe(newName => {
        this.service.addPeer(profile, { ...peer, name: newName })
      })
  }

  viewConfig(peer: VpnServerPeer) {
    const server = this.server()
    if (!server) return

    const subnet = server.server_address.replace(/\.\d+$/, '')
    const gateway = `${subnet}.1`
    const allowedIPs = peer.route_all ? '0.0.0.0/0, ::/0' : `${subnet}.0/24`

    const config = [
      '[Interface]',
      'PrivateKey = <PRIVATE_KEY>',
      `Address = ${peer.ip}/32`,
      `DNS = ${gateway}`,
      '',
      '[Peer]',
      `PublicKey = ${server.public_key}`,
      `Endpoint = ${server.endpoint}:${server.listen_port}`,
      `AllowedIPs = ${allowedIPs}`,
    ].join('\n')

    this.dialogs
      .open(CLIENT_CONFIG, {
        data: {
          name: peer.name,
          config,
        },
      })
      .subscribe()
  }

  changeRouting(peer: VpnServerPeer) {
    const server = this.server()
    if (!server || !peer.public_key) return

    const newRouteAll = !peer.route_all

    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Change routing mode?',
        data: {
          content: `This will delete the existing peer and create a new one with ${newRouteAll ? 'all traffic' : 'LAN only'} routing. You will need to reconfigure your device with the new config.`,
          yes: 'Continue',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        await this.service.deletePeer(server.profile, peer.public_key!)

        // Refresh device IPs so the deleted peer's IP is no longer reserved
        const devices = await this.devices.get()
        this.deviceIps.set(devices.map(d => d.ipv4).filter(Boolean) as string[])

        const peerIps = server.peers
          .filter(p => p.public_key !== peer.public_key)
          .map(p => p.ip)
          .filter(Boolean) as string[]
        const usedIps = [
          ...new Set([
            server.server_address,
            ...(this.deviceIps() ?? []),
            ...peerIps,
          ]),
        ]

        this.dialogs
          .open<VpnServerPeer>(ADD_CLIENT, {
            label: 'Reconfigure client device',
            data: {
              serverAddress: server.server_address,
              usedIps,
              defaults: {
                name: peer.name,
                ip: peer.ip,
                route_all: newRouteAll,
              },
            } satisfies ClientDialogData,
          })
          .subscribe(async newPeer => {
            const response = await this.service.addPeer(server.profile, newPeer)
            if (response?.client_config) {
              this.dialogs
                .open(CLIENT_CONFIG, {
                  data: {
                    name: newPeer.name,
                    config: response.client_config,
                  },
                })
                .subscribe()
            }
          })
      })
  }

  delete(peer: VpnServerPeer) {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(() => {
        const profile = this.server()?.profile
        if (profile && peer.public_key) {
          this.service.deletePeer(profile, peer.public_key)
        }
      })
  }
}
