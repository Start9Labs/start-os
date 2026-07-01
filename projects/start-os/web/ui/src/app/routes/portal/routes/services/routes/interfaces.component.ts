import { UpperCasePipe } from '@angular/common'
import { Component, computed, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { getPkgId, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-core'
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

// A port-range interface renders like a single-port interface (same per-gateway
// address card) plus its external port span shown in the header.
type MappedRangeInterface = MappedServiceInterface & { portRange: string }

@Component({
  template: `
    @if (pkg()) {
      @if (interfaces().length || ranges().length) {
        <tui-accordion [closeOthers]="false">
          @for (iface of interfaces(); track iface.id) {
            <button [tuiAccordion]="single()">
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
          @for (range of ranges(); track range.id) {
            <button [tuiAccordion]="single()">
              <span tuiTitle>
                <b>
                  {{ range.name }}
                  <span tuiBadge appearance="info"><b>API</b></span>
                  <span tuiBadge appearance="neutral">
                    {{ range.portRange }}
                  </span>
                </b>
                @if (range.description) {
                  <span tuiSubtitle>{{ range.description }}</span>
                }
              </span>
            </button>
            <tui-expand>
              <service-interface
                [packageId]="pkgId"
                [value]="range"
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

    [tuiBadge] {
      margin-inline-start: 0.25rem;
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

  readonly single = computed(
    () => this.interfaces().length + this.ranges().length === 1,
  )

  // Single-port service interfaces, reached host -> binding -> interface.
  readonly interfaces = computed<MappedServiceInterface[]>(() => {
    const pkg = this.pkg()
    if (!pkg) return []

    const { hosts } = pkg
    const gateways = this.gatewayService.gateways() || []
    const allPackageData = this.allPackageData()

    const all = Object.values(hosts).flatMap(host =>
      Object.values(host.bindings).flatMap(binding =>
        Object.values(binding.interfaces),
      ),
    )

    return all.sort(tuiDefaultSort).map(iFace => {
      const hostId = iFace.addressInfo.hostId || ''
      const host = hosts[hostId]
      const binding = host?.bindings[iFace.addressInfo.internalPort]
      const sharedHostNames = all
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

  // Port-range interfaces, reached host -> bindingRange -> interface. They reuse
  // the single-port per-gateway address card (non-SSL), driven by the range's
  // own `addresses`; the external port span is shown in the header.
  readonly ranges = computed<MappedRangeInterface[]>(() => {
    const pkg = this.pkg()
    if (!pkg) return []

    const gateways = this.gatewayService.gateways() || []

    return Object.entries(pkg.hosts)
      .flatMap(([hostId, host]) =>
        Object.entries(host.bindingRanges)
          .filter(([, range]) => range.interface)
          .map(([key, range]) => {
            const iface = range.interface!
            const internalStartPort = Number(key)
            const end = range.externalStartPort + range.numberOfPorts - 1
            // Representative addressInfo: `internalPort` is the range's internal
            // start port (its key in bindingRanges) so the per-address toggle
            // and add-domain effects target the range; `scheme` prefixes URLs.
            const addressInfo: T.AddressInfo = {
              username: null,
              hostId,
              internalPort: internalStartPort,
              scheme: iface.scheme,
              sslScheme: null,
              suffix: '',
            }

            return {
              id: iface.id,
              name: iface.name,
              description: iface.description,
              masked: false,
              type: 'api' as const,
              addressInfo,
              gatewayGroups: this.interfaceService.getRangeGatewayGroups(
                addressInfo,
                range.addresses,
                host,
                gateways,
                range.numberOfPorts,
              ),
              pluginGroups: [],
              addSsl: false,
              sharedHostNames: [],
              portRange:
                range.numberOfPorts > 1
                  ? `${range.externalStartPort}-${end}`
                  : `${range.externalStartPort}`,
            }
          }),
      )
      .sort((a, b) => a.addressInfo.internalPort - b.addressInfo.internalPort)
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
