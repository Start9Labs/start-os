import { UpperCasePipe } from '@angular/common'
import { Component, computed, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { getPkgId, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { tuiDefaultSort } from '@taiga-ui/cdk'
import { TuiTitle } from '@taiga-ui/core'
import { TuiAccordion, TuiBadge } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import {
  InterfaceService,
  MappedServiceInterface,
} from 'src/app/routes/portal/components/interfaces/interface.service'
import { GatewayService } from 'src/app/services/gateway.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getInstalledBaseStatus } from 'src/app/services/pkg-status-rendering.service'

@Component({
  template: `
    @if (pkg()) {
      @if (interfaces().length) {
        <tui-accordion [closeOthers]="false">
          @for (iface of interfaces(); track iface.id) {
            <button [tuiAccordion]="interfaces().length === 1">
              <span tuiTitle>
                <b>
                  {{ iface.name }}
                  <span tuiBadge [appearance]="getAppearance(iface.type)">
                    <b>{{ iface.type | uppercase }}</b>
                  </span>
                </b>
                @if (iface.description) {
                  <span tuiSubtitle>{{ iface.description }}</span>
                }
              </span>
            </button>
            <tui-expand>
              <service-interface
                [packageId]="pkgId"
                [value]="iface"
                [isRunning]="isRunning()"
              />
            </tui-expand>
          }
        </tui-accordion>
      } @else {
        <app-placeholder icon="@tui.monitor-x">
          {{ 'No service interfaces' | i18n }}
        </app-placeholder>
      }
    }
  `,
  styles: `
    [tuiAccordion] {
      block-size: auto;
      min-block-size: var(--tui-height-l);
      padding-block: 0.75rem;
      white-space: normal;
    }
  `,
  host: { class: 'g-subpage' },
  providers: [GatewayService],
  imports: [
    TuiAccordion,
    TuiBadge,
    TuiTitle,
    InterfaceComponent,
    PlaceholderComponent,
    i18nPipe,
    UpperCasePipe,
  ],
})
export default class ServiceInterfacesRoute {
  private readonly interfaceService = inject(InterfaceService)
  private readonly gatewayService = inject(GatewayService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly pkgId = getPkgId()
  readonly pkg = toSignal(this.patch.watch$('packageData', this.pkgId))
  readonly allPackageData = toSignal(this.patch.watch$('packageData'))
  readonly isRunning = computed((pkg = this.pkg()) =>
    pkg ? getInstalledBaseStatus(pkg.statusInfo) === 'running' : false,
  )

  readonly interfaces = computed<MappedServiceInterface[]>(() => {
    const pkg = this.pkg()
    if (!pkg) return []

    const { serviceInterfaces, hosts } = pkg
    const gateways = this.gatewayService.gateways() || []
    const allPackageData = this.allPackageData()

    return Object.values(serviceInterfaces)
      .sort(tuiDefaultSort)
      .map(iFace => {
        const hostId = iFace.addressInfo.hostId || ''
        const host = hosts[hostId]
        const binding = host?.bindings[iFace.addressInfo.internalPort]
        const sharedHostNames = Object.values(serviceInterfaces)
          .filter(si => si.addressInfo.hostId === hostId && si.id !== iFace.id)
          .map(si => si.name)

        return {
          ...iFace,
          gatewayGroups: host
            ? this.interfaceService.getGatewayGroups(iFace, host, gateways)
            : [],
          pluginGroups: host
            ? this.interfaceService.getPluginGroups(iFace, host, allPackageData)
            : [],
          addSsl: !!binding?.options.addSsl,
          sharedHostNames,
        }
      })
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
