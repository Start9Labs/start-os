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
import { ApiService } from 'src/app/services/api/api.service'
import { provideFormService } from 'src/app/services/form.service'
import { PublishPortDialog } from './dialog'
import { PublishedPortsService } from './service'
import { PublishedPortsTable } from './table'
import { PublishedPortDialogResult, PublishedPortDisplay } from './types'
import { i18nPipe } from 'src/app/i18n/i18n.pipe'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>{{ 'Published Ports' | i18n }}</h2>
      </hgroup>
      <aside tuiAccessories>
        @if (!loading()) {
          <button tuiButton iconStart="@tui.plus" (click)="edit()">
            {{ 'Add' | i18n }}
          </button>
        }
      </aside>
    </header>
    <table
      [style.margin-block.rem]="1"
      [style.min-height.rem]="loading() ? 10 : 0"
      [publishedPorts]="loading() ? [] : service.data() || []"
      [ipv4EndpointHost]="ipv4EndpointHost()"
      [ipv6Available]="ipv6Available()"
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
    PublishedPortsTable,
    TuiSkeleton,
    i18nPipe,
  ],
})
export default class PublishedPorts {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(PublishedPortsService)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)
  protected readonly loading = computed(() => !this.service.data())

  protected readonly ipv6Available = signal(true)

  // IPv4 endpoint host: DDNS hostname or WAN IP
  protected readonly ipv4EndpointHost = signal<string | null>(null)

  constructor() {
    this.loadDependencies()
  }

  private async loadDependencies() {
    const [wanIpv6, lanIpv6, ddns, wanIpv4] = await Promise.all([
      this.api.wanIpv6Get(),
      this.api.lanIpv6Get(),
      this.api.wanDdnsGet(),
      this.api.wanIpv4Get(),
    ])
    // IPv6 port forwarding requires WAN IPv6 + LAN IPv6
    const wanIpv6Enabled = wanIpv6.mode !== 'disabled'
    const lanIpv6Enabled = lanIpv6.slaac || lanIpv6.dhcpv6
    this.ipv6Available.set(wanIpv6Enabled && lanIpv6Enabled)

    // Use DDNS hostname if available, otherwise WAN IP
    const ddnsHostname = ddns.enabled ? ddns.hostname || null : null
    this.ipv4EndpointHost.set(ddnsHostname || wanIpv4.assigned_ip)
  }

  edit(existing?: PublishedPortDisplay) {
    const devices = this.service.getDevices()
    const data = this.service.data() || []

    this.dialogs
      .open<PublishedPortDialogResult>(
        new PolymorpheusComponent(PublishPortDialog),
        {
          label: existing
            ? this.i18n.transform('Edit Published Port')
            : this.i18n.transform('Publish Ports'),
          data: {
            devices,
            existing,
            ipv6Available: this.ipv6Available(),
          },
        },
      )
      .subscribe(result => {
        const { port: value, reserveIpv4, reserveIpv6 } = result

        // Handle IP reservation if needed
        if (reserveIpv4 || reserveIpv6) {
          this.service.reserveDeviceIps(
            value.deviceMac,
            reserveIpv4,
            reserveIpv6,
          )
        }

        // Update/enrich the port
        const device = this.service.getDevice(value.deviceMac)
        const port: PublishedPortDisplay = {
          ...value,
          status: value.enabled ? 'active' : 'disabled',
          deviceName: device?.name || device?.hostname,
          deviceIpv4: device?.ipv4,
          deviceIpv6: device?.ipv6,
        }

        // Update array
        const items = existing
          ? data.map(p => (p.id === value.id ? port : p))
          : data.concat(port)

        this.service.save(items)
      })
  }
}
