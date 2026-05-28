import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { getPkgId, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { tuiDefaultSort } from '@taiga-ui/cdk'
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
      @let ifaces = interfaces();

      @if (ifaces.length) {
        <tui-accordion [closeOthers]="false">
          @for (iface of ifaces; track iface.id) {
            <button [tuiAccordion]="ifaces.length === 1">
              <span class="header">
                <span class="title">
                  {{ iface.name }}
                  <span
                    tuiBadge
                    size="m"
                    [appearance]="getAppearance(iface.type)"
                  >
                    {{ iface.type }}
                  </span>
                </span>
                @if (iface.description) {
                  <span class="description">{{ iface.description }}</span>
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
      padding: 0.75rem 1.25rem;
      white-space: normal;
    }

    .header {
      flex: 1;
      min-width: 0;
      display: flex;
      flex-direction: column;
      gap: 0.25rem;
      align-items: flex-start;
    }

    .title {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    tui-badge {
      text-transform: uppercase;
      font-weight: bold;
    }

    .description {
      font: var(--tui-typography-body-s);
      color: var(--tui-text-secondary);
      text-align: start;
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [GatewayService],
  imports: [
    TuiAccordion,
    TuiBadge,
    InterfaceComponent,
    PlaceholderComponent,
    i18nPipe,
  ],
})
export default class ServiceInterfacesRoute {
  private readonly interfaceService = inject(InterfaceService)
  private readonly gatewayService = inject(GatewayService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly pkgId = getPkgId()
  readonly pkg = toSignal(this.patch.watch$('packageData', this.pkgId))
  readonly allPackageData = toSignal(this.patch.watch$('packageData'))

  readonly isRunning = computed(() => {
    const pkg = this.pkg()
    return pkg ? getInstalledBaseStatus(pkg.statusInfo) === 'running' : false
  })

  readonly interfaces = computed<MappedServiceInterface[]>(() => {
    const pkg = this.pkg()
    if (!pkg) return []

    const { serviceInterfaces, hosts } = pkg
    const gateways = this.gatewayService.gateways() || []
    const allPackageData = this.allPackageData()

    return Object.values(serviceInterfaces)
      .sort((a, b) => tuiDefaultSort(a, b))
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
