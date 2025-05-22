import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { getAddresses } from 'src/app/routes/portal/components/interfaces/interface.utils'
import { InterfaceStatusComponent } from 'src/app/routes/portal/components/interfaces/status.component'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ iface.name }}
      <interface-status [style.margin-left.rem]="0.5" [public]="public()" />
    </ng-container>
    <header tuiHeader>
      <hgroup>
        <h3>
          {{ iface.name }}
          <interface-status [public]="public()" />
        </h3>
        <p tuiSubtitle>{{ iface.description }}</p>
      </hgroup>
    </header>
    @if (ui(); as ui) {
      <app-interface [value]="ui" [isRunning]="true" />
    }
  `,
  styles: `
    h3 {
      display: flex;
      align-items: center;
      gap: 0.5rem;
      margin: 1rem 0 0.5rem 0;
      font-size: 2.4rem;

      tui-badge {
        text-transform: uppercase;
        font-weight: bold;
      }
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    InterfaceComponent,
    RouterLink,
    TuiButton,
    TitleDirective,
    TuiHeader,
    InterfaceStatusComponent,
    i18nPipe,
  ],
})
export default class StartOsUiComponent {
  private readonly config = inject(ConfigService)
  private readonly i18n = inject(i18nPipe)

  readonly iface: T.ServiceInterface = {
    id: '',
    name: 'StartOS UI',
    description: this.i18n.transform(
      'The web user interface for your StartOS server, accessible from any browser.',
    )!,
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

  readonly ui = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('serverInfo', 'network', 'host')
      .pipe(
        map(host => {
          const port = this.iface.addressInfo.internalPort

          return {
            ...this.iface,
            addSsl: host.bindings[port]?.options.addSsl,
            public: !!host.bindings[port]?.net.public,
            addresses: getAddresses(this.iface, host, this.config),
          }
        }),
      ),
  )

  readonly public = computed((ui = this.ui()) => !!ui?.public)
}
