import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Help } from 'src/app/directives/help'
import { EthernetTable } from 'src/app/routes/home/routes/ethernet/table'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'

import { EthernetAside } from './aside'
import { CHANGE_WAN_DIALOG } from './dialog'
import { EthernetPort, EthernetService } from './service'

@Component({
  template: `
    <ethernet-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Ethernet</h2></hgroup>
      <aside tuiAccessories>
        @if (service.data()) {
          <button tuiButton size="s" appearance="flat" (click)="onChangeWan()">
            Change WAN Port
          </button>
        }
      </aside>
    </header>
    <table
      [tuiSkeleton]="!service.data()"
      [ethernetTable]="service.data() || []"
    ></table>
  `,
  styles: `
    :host {
      max-width: 30rem;
      padding-top: 0;
    }
  `,
  host: { class: 'g-page' },
  providers: [provideFormService(EthernetService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiHeader,
    TuiTitle,
    TuiButton,
    TuiSkeleton,
    Help,
    EthernetAside,
    EthernetTable,
  ],
})
export default class Ethernet {
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly service = injectFormService<
    EthernetPort[]
  >() as EthernetService

  onChangeWan() {
    const ports = this.service.data()
    if (!ports) return

    this.dialogs
      .open<EthernetPort | null>(CHANGE_WAN_DIALOG, {
        label: 'Change WAN Port',
        size: 's',
        data: ports,
      })
      .pipe(filter(Boolean))
      .subscribe(async newWanPort => {
        const items = ports.map(p => ({
          ...p,
          wan: p.name === newWanPort.name,
        }))

        const success = await this.service.save(items)
        if (success) {
          this.service.restart()
        }
      })
  }
}
