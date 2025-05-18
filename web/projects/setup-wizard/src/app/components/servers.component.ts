import { Component } from '@angular/core'
import { ServerComponent } from '@start9labs/shared'
import { TuiDialogContext } from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PasswordDirective } from 'src/app/components/password.directive'
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
  imports: [ServerComponent, PasswordDirective],
})
export class ServersComponent {
  readonly context = injectContext<TuiDialogContext<ServersResponse, Data>>()

  select(password: string, serverId: string) {
    this.context.completeWith({ serverId, password })
  }
}

export const SERVERS = new PolymorpheusComponent(ServersComponent)
