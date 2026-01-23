import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help'
import { EthernetTable } from 'src/app/routes/home/routes/ethernet/table'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'

import { EthernetAside } from './aside'
import { EthernetPort, EthernetService } from './service'

@Component({
  template: `
    <ethernet-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Ethernet</h2></hgroup>
    </header>
    <table
      [tuiSkeleton]="!service.data()"
      [ethernetTable]="service.data() || []"
    ></table>
  `,
  providers: [provideFormService(EthernetService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiHeader,
    TuiTitle,
    TuiSkeleton,
    Help,
    EthernetAside,
    EthernetTable,
  ],
})
export default class Ethernet {
  protected readonly service = injectFormService<EthernetPort[]>()
}
