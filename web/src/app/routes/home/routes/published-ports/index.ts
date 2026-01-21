import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'

import { PublishedPortsAside } from './aside'
import { PublishPortDialog } from './dialog'
import { PublishedPortsService } from './service'
import { PublishedPortsTable } from './table'
import { PublishedPort, PublishedPortDisplay } from './types'

@Component({
  template: `
    <published-ports-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Published Ports</h2></hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="add()">New</button>
      </aside>
    </header>
    <table
      [style.margin-block.rem]="1"
      [style.min-height.rem]="loading() ? 10 : 0"
      [publishedPortsTable]="loading() ? [] : service.data() || []"
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
  protected readonly service = injectFormService<
    PublishedPortDisplay[]
  >() as PublishedPortsService
  protected readonly loading = computed(() => !this.service.data())

  add() {
    const devices = this.service.getDevices()

    this.dialogs
      .open<PublishedPort>(new PolymorpheusComponent(PublishPortDialog), {
        label: 'Publish Ports',
        data: { devices },
      })
      .subscribe(value => {
        // Enrich the new port with display data
        const device = this.service.getDevice(value.deviceMac)
        const newPort: PublishedPortDisplay = {
          ...value,
          status: 'active',
          deviceName: device?.name || device?.hostname,
          deviceIpv4: device?.ipv4,
          deviceIpv6: device?.ipv6,
          endpointIpv4: value.ipv4
            ? `example.ddns.net:${value.ipv4PublicPort || value.ports}`
            : undefined,
          endpointIpv6:
            value.ipv6 && device?.ipv6
              ? `[${device.ipv6}]:${value.ports}`
              : undefined,
        }

        this.service.save([...(this.service.data() || []), newPort])
      })
  }

  edit(item: PublishedPortDisplay) {
    const devices = this.service.getDevices()

    this.dialogs
      .open<PublishedPort>(new PolymorpheusComponent(PublishPortDialog), {
        label: 'Edit Published Port',
        data: { devices, existing: item },
      })
      .subscribe(value => {
        // Update the port with new values
        const device = this.service.getDevice(value.deviceMac)
        const updatedPort: PublishedPortDisplay = {
          ...value,
          status: value.enabled ? 'active' : 'disabled',
          deviceName: device?.name || device?.hostname,
          deviceIpv4: device?.ipv4,
          deviceIpv6: device?.ipv6,
          endpointIpv4: value.ipv4
            ? `example.ddns.net:${value.ipv4PublicPort || value.ports}`
            : undefined,
          endpointIpv6:
            value.ipv6 && device?.ipv6
              ? `[${device.ipv6}]:${value.ports}`
              : undefined,
        }

        // Replace the existing item
        const items = (this.service.data() || []).map(p =>
          p.id === value.id ? updatedPort : p,
        )
        this.service.save(items)
      })
  }
}
