import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { T } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { Observable, map } from 'rxjs'
import {
  InterfaceComponent,
  MappedServiceInterface,
} from 'src/app/routes/portal/components/interfaces/interface.component'
import { getAddresses } from 'src/app/routes/portal/components/interfaces/interface.utils'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'

const iface: T.ServiceInterface = {
  id: '',
  name: 'StartOS User Interface',
  description:
    'The primary user interface for your StartOS server, accessible from any browser.',
  type: 'ui' as const,
  masked: false,
  addressInfo: {
    hostId: 'startos-ui',
    internalPort: 80,
    scheme: 'http',
    sslScheme: 'https',
    suffix: '',
    username: null,
  },
}

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      Web Addresses
    </ng-container>
    <app-interface
      *ngIf="ui$ | async as ui"
      [style.max-width.rem]="50"
      [serviceInterface]="ui"
    />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    InterfaceComponent,
    RouterLink,
    TuiButton,
    TitleDirective,
  ],
})
export class StartOsUiComponent {
  private readonly config = inject(ConfigService)

  readonly ui$: Observable<MappedServiceInterface> = inject<PatchDB<DataModel>>(
    PatchDB,
  )
    .watch$('serverInfo', 'network', 'host')
    .pipe(
      map(host => ({
        ...iface,
        public: host.bindings[iface.addressInfo.internalPort].net.public,
        addresses: getAddresses(iface, host, this.config),
      })),
    )
}
