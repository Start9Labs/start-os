import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
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
      [forwardingTable]="service.data() || []"
      [tuiSkeleton]="!service.data()"
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
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = injectFormService<Forwarding[]>()

  add() {
    this.dialogs
      .open<Forwarding>(new PolymorpheusComponent(ForwardingDialog), {
        label: 'Add Port Forwarding Rule',
      })
      .subscribe(value => {
        this.service.save((this.service.data() || []).concat(value))
      })
  }
}
