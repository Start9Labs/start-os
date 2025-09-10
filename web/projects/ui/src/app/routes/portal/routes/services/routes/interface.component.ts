import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { getPkgId, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiItem } from '@taiga-ui/cdk'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiBreadcrumbs } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import {
  getPublicDomains,
  InterfaceService,
} from '../../../components/interfaces/interface.service'
import { GatewayService } from 'src/app/services/gateway.service'

@Component({
  template: `
    <ng-container *title>
      <a routerLink="../.." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ serviceInterface()?.name }}
    </ng-container>
    <tui-breadcrumbs size="l">
      <a *tuiItem tuiLink appearance="action-grayscale" routerLink="../..">
        {{ 'Dashboard' | i18n }}
      </a>
      <span *tuiItem class="g-primary">{{ serviceInterface()?.name }}</span>
    </tui-breadcrumbs>
    @if (serviceInterface(); as value) {
      <header tuiHeader [style.margin-bottom.rem]="1">
        <hgroup tuiTitle>
          <h3>
            {{ value.name }}
            <tui-badge size="l" [appearance]="getAppearance(value.type)">
              {{ value.type }}
            </tui-badge>
          </h3>
          <p tuiSubtitle>{{ value.description }}</p>
        </hgroup>
      </header>
      <service-interface
        [packageId]="pkgId"
        [value]="value"
        [isRunning]="isRunning()"
      />
    }
  `,
  styles: `
    :host-context(tui-root._mobile) tui-breadcrumbs,
    :host-context(tui-root._mobile) h3 {
      display: none;
    }

    h3 {
      display: flex;
      align-items: center;
      gap: 0.5rem;

      tui-badge {
        text-transform: uppercase;
        font-weight: bold;
      }
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [GatewayService],
  imports: [
    InterfaceComponent,
    RouterLink,
    TuiButton,
    TitleDirective,
    TuiBreadcrumbs,
    TuiItem,
    TuiLink,
    i18nPipe,
    TuiBadge,
    TuiHeader,
    TuiTitle,
  ],
})
export default class ServiceInterfaceRoute {
  private readonly interfaceService = inject(InterfaceService)
  private readonly gatewayService = inject(GatewayService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly pkgId = getPkgId()
  readonly interfaceId = input('')

  readonly pkg = toSignal(this.patch.watch$('packageData', this.pkgId))

  readonly isRunning = computed(() => {
    return this.pkg()?.status.main === 'running'
  })

  readonly serviceInterface = computed(() => {
    const pkg = this.pkg()
    const id = this.interfaceId()

    if (!pkg || !id) {
      return
    }

    const { serviceInterfaces, hosts } = pkg
    const iFace = serviceInterfaces[this.interfaceId()]
    const key = iFace!.addressInfo.hostId || ''
    const host = hosts[key]
    const port = iFace!.addressInfo.internalPort

    if (!host || !iFace || !port) {
      return
    }

    const binding = host.bindings[port]

    const gateways = this.gatewayService.gateways() || []

    return {
      ...iFace,
      addresses: this.interfaceService.getAddresses(iFace, host, gateways),
      gateways:
        gateways.map(g => ({
          enabled:
            (g.public
              ? binding?.net.publicEnabled.includes(g.id)
              : !binding?.net.privateDisabled.includes(g.id)) ?? false,
          ...g,
        })) || [],
      torDomains: host.onions,
      publicDomains: getPublicDomains(host.publicDomains, gateways),
      privateDomains: host.privateDomains,
    }
  })

  getAppearance(type: T.ServiceInterfaceType = 'ui'): string {
    switch (type) {
      case 'ui':
        return 'positive'
      case 'api':
        return 'info'
      case 'p2p':
        return 'negative'
    }
  }
}
