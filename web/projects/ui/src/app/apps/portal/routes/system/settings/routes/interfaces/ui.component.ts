import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { Observable, map } from 'rxjs'
import {
  InterfaceComponent,
  ServiceInterfaceWithAddresses,
} from 'src/app/apps/portal/components/interfaces/interface.component'
import { getAddresses } from 'src/app/apps/portal/components/interfaces/interface.utils'
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
        const serviceInterface: T.ServiceInterfaceWithHostInfo = {
          id: 'startos-ui',
          name: 'StartOS UI',
          description: 'The primary web user interface for StartOS',
          type: 'ui',
          hasPrimary: false,
          disabled: false,
          masked: false,
          addressInfo: {
            hostId: '',
            username: null,
            suffix: '',
            bindOptions: {
              scheme: 'http',
              preferredExternalPort: 80,
              addSsl: {
                scheme: 'https',
                preferredExternalPort: 443,
                // @TODO is this alpn correct?
                alpn: { specified: ['http/1.1', 'h2'] },
              },
              secure: {
                ssl: false,
              },
            },
          },
          hostInfo: {
            id: 'start-os-ui-host',
            kind: 'multi',
            hostnames: hosts,
          },
        }

        return {
          ...serviceInterface,
          addresses: getAddresses(serviceInterface),
        }
      }),
    )
}
