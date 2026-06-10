import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { getPkgId, i18nPipe } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import {
  MappedPortRange,
  PortRangeComponent,
} from 'src/app/routes/portal/components/port-ranges/port-range.component'
import { GatewayService } from 'src/app/services/gateway.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    @if (pkg()) {
      @for (range of ranges(); track range.internalStartPort) {
        <service-port-range [packageId]="pkgId" [value]="range" />
      } @empty {
        <app-placeholder icon="@tui.arrow-right-left">
          {{ 'No port ranges' | i18n }}
        </app-placeholder>
      }
    }
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [GatewayService],
  imports: [PortRangeComponent, PlaceholderComponent, i18nPipe],
})
export default class ServicePortRangesRoute {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly pkgId = getPkgId()
  readonly pkg = toSignal(this.patch.watch$('packageData', this.pkgId))

  readonly ranges = computed<MappedPortRange[]>(() => {
    const pkg = this.pkg()
    if (!pkg) return []

    const out: MappedPortRange[] = []
    for (const [hostId, host] of Object.entries(pkg.hosts)) {
      for (const [key, range] of Object.entries(host.bindingRanges)) {
        out.push({
          hostId,
          internalStartPort: Number(key),
          externalStartPort: range.externalStartPort,
          numberOfPorts: range.numberOfPorts,
          gatewayAccess: range.gatewayAccess,
        })
      }
    }

    return out.sort((a, b) => a.internalStartPort - b.internalStartPort)
  })
}
