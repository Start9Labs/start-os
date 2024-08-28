import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { Observable, map } from 'rxjs'
import {
  InterfaceComponent,
  ServiceInterfaceWithAddresses,
} from 'src/app/routes/portal/components/interfaces/interface.component'
import { getMultihostAddresses } from 'src/app/routes/portal/components/interfaces/interface.utils'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <app-interface
      *ngIf="ui$ | async as ui"
      [style.max-width.rem]="50"
      [serviceInterface]="ui"
    />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfaceComponent],
})
export class StartOsUiComponent {
  readonly ui$: Observable<ServiceInterfaceWithAddresses> = inject(
    PatchDB<DataModel>,
  )
    .watch$('serverInfo', 'ui')
    .pipe(
      map(hosts => {
        const serviceInterface: T.ServiceInterface = {
          id: 'startos-ui',
          name: 'StartOS UI',
          description: 'The primary web user interface for StartOS',
          type: 'ui',
          hasPrimary: false,
          masked: false,
          addressInfo: {
            hostId: '',
            username: null,
            internalPort: 80,
            scheme: 'http',
            sslScheme: 'https',
            suffix: '',
          },
        }

        // @TODO Aiden confirm this is correct
        const host: T.Host = {
          kind: 'multi',
          bindings: {},
          hostnameInfo: {
            80: hosts,
          },
          addresses: [],
        }

        return {
          ...serviceInterface,
          addresses: getMultihostAddresses(serviceInterface, host),
        }
      }),
    )
}
