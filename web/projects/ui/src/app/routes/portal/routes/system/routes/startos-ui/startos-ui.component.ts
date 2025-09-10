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
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import {
  getPublicDomains,
  InterfaceService,
} from 'src/app/routes/portal/components/interfaces/interface.service'
import { GatewayService } from 'src/app/services/gateway.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ iface.name }}
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>
          {{ iface.name }}
        </h3>
        <p tuiSubtitle>{{ iface.description }}</p>
      </hgroup>
    </header>
    <service-interface [value]="ui()" [isRunning]="true" />
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [GatewayService],
  imports: [
    InterfaceComponent,
    RouterLink,
    TuiButton,
    TitleDirective,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
})
export default class StartOsUiComponent {
  private readonly interfaceService = inject(InterfaceService)
  private readonly gatewayService = inject(GatewayService)
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

  readonly network = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo', 'network'),
  )

  readonly ui = computed(() => {
    const network = this.network()
    const gateways = this.gatewayService.gateways()

    if (!network || !gateways) return

    const binding = network.host.bindings['80']

    return {
      ...this.iface,
      addresses: this.interfaceService.getAddresses(
        this.iface,
        network.host,
        gateways,
      ),
      gateways: gateways.map(g => ({
        enabled:
          (g.public
            ? binding?.net.publicEnabled.includes(g.id)
            : !binding?.net.privateDisabled.includes(g.id)) ?? false,
        ...g,
      })),
      torDomains: network.host.onions,
      publicDomains: getPublicDomains(network.host.publicDomains, gateways),
      privateDomains: network.host.privateDomains,
    }
  })
}
