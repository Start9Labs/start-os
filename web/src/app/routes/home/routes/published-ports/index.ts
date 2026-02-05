import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { Help } from 'src/app/directives/help'
import { LanIpv6UciService } from 'src/app/routes/home/routes/lan/routes/ipv6/uci/service'
import { DdnsUciService } from 'src/app/routes/home/routes/wan/routes/ddns/uci/service'
import { WanIpv4UciService } from 'src/app/routes/home/routes/wan/routes/ipv4/uci/service'
import { WanIpv6UciService } from 'src/app/routes/home/routes/wan/routes/ipv6/uci/service'
import { provideFormService } from 'src/app/services/form.service'

import { PublishedPortsAside } from './aside'
import { PublishPortDialog } from './dialog'
import { PublishedPortsService } from './service'
import { PublishedPortsTable } from './table'
import { PublishedPortDialogResult, PublishedPortDisplay } from './types'

@Component({
  template: `
    <published-ports-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Published Ports</h2></hgroup>
      <aside tuiAccessories>
        @if (!loading()) {
          <button tuiButton iconStart="@tui.plus" (click)="add()">Add</button>
        }
      </aside>
    </header>
    <table
      [style.margin-block.rem]="1"
      [style.min-height.rem]="loading() ? 10 : 0"
      [publishedPorts]="loading() ? [] : service.data() || []"
      [ipv4EndpointHost]="ipv4EndpointHost()"
      [tuiSkeleton]="loading()"
      (edit)="edit($event)"
    ></table>
  `,
  providers: [provideFormService(PublishedPortsService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiHeader,
    TuiTitle,
    TuiButton,
    Help,
    PublishedPortsAside,
    PublishedPortsTable,
    TuiSkeleton,
  ],
})
export default class PublishedPorts {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(PublishedPortsService)
  protected readonly lanIpv6Uci = inject(LanIpv6UciService)
  protected readonly wanIpv6Uci = inject(WanIpv6UciService)
  protected readonly wanIpv4Uci = inject(WanIpv4UciService)
  protected readonly ddnsUci = inject(DdnsUciService)
  protected readonly loading = computed(() => !this.service.data())

  protected readonly ipv6Available = signal(true)

  // IPv4 endpoint host: DDNS hostname or WAN IP
  protected readonly ipv4EndpointHost = signal<string | null>(null)

  constructor() {
    this.loadDependencies()
  }

  private async loadDependencies() {
    const [wanEnabled, lanEnabled, ddnsHostname, wanIp] = await Promise.all([
      this.wanIpv6Uci.isEnabled(),
      this.lanIpv6Uci.isEnabled(),
      this.ddnsUci.getHostname(),
      this.wanIpv4Uci.getWanIp(),
    ])
    // IPv6 port forwarding requires WAN IPv6 + LAN IPv6
    this.ipv6Available.set(wanEnabled && lanEnabled)

    // Use DDNS hostname if available, otherwise WAN IP
    this.ipv4EndpointHost.set(ddnsHostname || wanIp)
  }

  add() {
    const devices = this.service.getDevices()

    this.dialogs
      .open<PublishedPortDialogResult>(
        new PolymorpheusComponent(PublishPortDialog),
        {
          label: 'Publish Ports',
          data: { devices, ipv6Available: this.ipv6Available() },
        },
      )
      .subscribe(result => {
        const { port: value, reserveIpv4 } = result

        // Handle IPv4 reservation if needed
        if (reserveIpv4) {
          this.service.reserveDeviceIps(value.deviceMac, true, false)
        }

        // Enrich the new port with display data
        const device = this.service.getDevice(value.deviceMac)
        const newPort: PublishedPortDisplay = {
          ...value,
          status: 'active',
          deviceName: device?.name || device?.hostname,
          deviceIpv4: device?.ipv4,
          deviceIpv6: device?.ipv6,
        }

        this.service.save([...(this.service.data() || []), newPort])
      })
  }

  edit(item: PublishedPortDisplay) {
    const devices = this.service.getDevices()

    this.dialogs
      .open<PublishedPortDialogResult>(
        new PolymorpheusComponent(PublishPortDialog),
        {
          label: 'Edit Published Port',
          data: {
            devices,
            existing: item,
            ipv6Available: this.ipv6Available(),
          },
        },
      )
      .subscribe(result => {
        const { port: value, reserveIpv4 } = result

        // Handle IPv4 reservation if needed
        if (reserveIpv4) {
          this.service.reserveDeviceIps(value.deviceMac, true, false)
        }

        // Update the port with new values
        const device = this.service.getDevice(value.deviceMac)
        const updatedPort: PublishedPortDisplay = {
          ...value,
          status: value.enabled ? 'active' : 'disabled',
          deviceName: device?.name || device?.hostname,
          deviceIpv4: device?.ipv4,
          deviceIpv6: device?.ipv6,
        }

        // Replace the existing item
        const items = (this.service.data() || []).map(p =>
          p.id === value.id ? updatedPort : p,
        )
        this.service.save(items)
      })
  }
}
