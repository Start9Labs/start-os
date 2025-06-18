import { Component } from '@angular/core'
import { ServerComponent, StartOSDiskInfo } from '@start9labs/shared'
import { TuiDialogContext } from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

interface Data {
  servers: StartOSDiskInfo[]
}

@Component({
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
  readonly context = injectContext<TuiDialogContext<StartOSDiskInfo, Data>>()
}

export const SERVERS = new PolymorpheusComponent(ServersComponent)
