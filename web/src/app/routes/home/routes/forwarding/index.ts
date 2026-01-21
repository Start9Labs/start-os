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
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { DevicesUciService } from 'src/app/routes/home/routes/devices/uci/service'

import { ForwardingAside } from './aside'
import { ForwardingDialog } from './dialog'
import { Forwarding, ForwardingService } from './service'
import { ForwardingTable } from './table'

@Component({
  template: `
    <forwarding-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Port Forwarding</h2></hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="add()">
          Add Rule
        </button>
      </aside>
    </header>
    <table
      [style.margin-block.rem]="1"
      [style.min-height.rem]="loading() ? 10 : 0"
      [forwardingTable]="loading() ? [] : service.data() || []"
      [deviceNames]="deviceNames() ?? {}"
      [tuiSkeleton]="loading()"
    ></table>
  `,
  providers: [provideFormService(ForwardingService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiHeader,
    TuiTitle,
    TuiButton,
    Help,
    ForwardingAside,
    ForwardingTable,
    TuiSkeleton,
  ],
})
export default class PortForwarding {
  private readonly devicesUci = inject(DevicesUciService)

  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = injectFormService<Forwarding[]>()
  protected readonly deviceNames = signal<Record<string, string> | null>(null)
  protected readonly loading = computed(
    () => !this.service.data() || !this.deviceNames(),
  )

  constructor() {
    this.loadDeviceNames()
  }

  private async loadDeviceNames() {
    const devices = await this.devicesUci.get()
    const names: Record<string, string> = {}
    for (const device of devices) {
      if (device.ipv4) {
        names[device.ipv4] = device.name || device.hostname
      }
    }
    this.deviceNames.set(names)
  }

  add() {
    this.dialogs
      .open<Forwarding>(new PolymorpheusComponent(ForwardingDialog), {
        label: 'Add Port Forwarding Rule',
        data: { deviceNames: this.deviceNames() ?? {} },
      })
      .subscribe(value => {
        this.service.save((this.service.data() || []).concat(value))
      })
  }
}
