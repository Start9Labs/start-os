import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Help } from 'src/app/directives/help'
import { EthernetTable } from 'src/app/routes/home/routes/ethernet/table'
import { provideFormService } from 'src/app/services/form.service'

import { EthernetAside } from './aside'
import { CHANGE_WAN_DIALOG } from './dialog'
import { EthernetPort, EthernetService } from './service'

@Component({
  template: `
    <ethernet-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Ethernet</h2></hgroup>
      @if (service.data(); as data) {
        <aside tuiAccessories>
          <button tuiButton size="s" (click)="onChangeWan(data)">
            Change WAN Port
          </button>
        </aside>
      }
    </header>
    <table
      [tuiSkeleton]="!service.data()"
      [ethernetTable]="service.data() || []"
    ></table>
  `,
  styles: `
    :host {
      max-width: 50rem;
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

  protected readonly service = inject(EthernetService)

  onChangeWan(data: EthernetPort[]) {
    this.dialogs
      .open<EthernetPort | null>(CHANGE_WAN_DIALOG, { size: 's', data })
      .pipe(filter(Boolean))
      .subscribe(async ({ name }) => {
        const items = data.map(port => ({
          ...port,
          wan: port.name === name,
        }))

        if (await this.service.save(items)) {
          this.service.restart()
        }
      })
  }
}
