import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { getAddresses } from 'src/app/routes/portal/components/interfaces/interface.utils'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'

const iface: T.ServiceInterface = {
  id: '',
  name: 'StartOS UI',
  description:
    'The web user interface for your StartOS server, accessible from any browser.',
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
      StartOS UI
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>{{ iface.name }}</h3>
        <p tuiSubtitle>{{ iface.description }}</p>
      </hgroup>
    </header>
    @if (ui(); as ui) {
      <app-interface [serviceInterface]="ui" />
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    InterfaceComponent,
    RouterLink,
    TuiButton,
    TitleDirective,
    TuiHeader,
    TuiTitle,
  ],
})
export default class StartOsUiComponent {
  private readonly config = inject(ConfigService)
  iface = iface

  readonly ui = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('serverInfo', 'network', 'host')
      .pipe(
        map(host => ({
          ...iface,
          public: host.bindings[iface.addressInfo.internalPort].net.public,
          addresses: getAddresses(iface, host, this.config),
        })),
      ),
  )
}
