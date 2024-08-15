import { Component, inject } from '@angular/core'
import { ServerComponent, StartOSDiskInfo } from '@start9labs/shared'
import { TuiDialogContext } from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'

interface Data {
  servers: StartOSDiskInfo[]
}

@Component({
  standalone: true,
  template: `
    @for (server of context.data.servers; track $index) {
      <button
        [server]="server"
        (click)="this.context.completeWith(server)"
      ></button>
    }
  `,
  imports: [ServerComponent],
})
export class ServersComponent {
  readonly context =
    inject<TuiDialogContext<StartOSDiskInfo, Data>>(POLYMORPHEUS_CONTEXT)
}

export const SERVERS = new PolymorpheusComponent(ServersComponent)
