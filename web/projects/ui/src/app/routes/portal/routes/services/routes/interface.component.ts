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
import { InterfaceService } from '../../../components/interfaces/interface.service'
import { GatewayService } from 'src/app/services/gateway.service'
import { getInstalledBaseStatus } from 'src/app/services/pkg-status-rendering.service'

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
            <span tuiBadge size="l" [appearance]="getAppearance(value.type)">
              {{ value.type }}
            </span>
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
  readonly allPackageData = toSignal(this.patch.watch$('packageData'))

  readonly isRunning = computed(() => {
    const pkg = this.pkg()
    return pkg ? getInstalledBaseStatus(pkg.statusInfo) === 'running' : false
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

    const sharedHostNames = Object.values(serviceInterfaces)
      .filter(si => si.addressInfo.hostId === key && si.id !== iFace.id)
      .map(si => si.name)

    return {
      ...iFace,
      gatewayGroups: this.interfaceService.getGatewayGroups(
        iFace,
        host,
        gateways,
      ),
      pluginGroups: this.interfaceService.getPluginGroups(
        iFace,
        host,
        this.allPackageData(),
      ),
      addSsl: !!binding?.options.addSsl,
      sharedHostNames,
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
