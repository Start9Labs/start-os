import { Component, inject } from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { ServerComponent } from 'src/app/components/server.component'
import { StartOSDiskInfoWithId } from 'src/app/services/api.service'

interface Data {
  servers: StartOSDiskInfoWithId[]
}

export interface ServersResponse {
  password: string
  serverId: string
}

@Component({
  standalone: true,
  template: `
    @for (server of context.data.servers; track $index) {
      <button [server]="server" (password)="select($event, server.id)"></button>
    }
  `,
  imports: [ServerComponent],
})
export class ServersComponent {
  readonly context =
    inject<TuiDialogContext<ServersResponse, Data>>(POLYMORPHEUS_CONTEXT)

  select(password: string, serverId: string) {
    this.context.completeWith({ serverId, password })
  }
}

export const SERVERS = new PolymorpheusComponent(ServersComponent)
