import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ErrorService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon, TuiLoader } from '@taiga-ui/core'
import { TuiButtonLoading } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

export type PortForwardsModalData = {
  gatewayId: string
  gatewayName: string
}

type PortForwardRow = {
  interfaces: string[]
  externalPort: number
  internalPort: number
}

function parseSocketAddr(s: string): { ip: string; port: number } {
  const lastColon = s.lastIndexOf(':')
  return {
    ip: s.substring(0, lastColon),
    port: Number(s.substring(lastColon + 1)),
  }
}

@Component({
  selector: 'port-forwards-modal',
  template: `
    <p>
      {{ 'Port forwarding rules required on gateway' | i18n }}
      "{{ context.data.gatewayName }}"
    </p>

    <table
      [appTable]="[
        'Interface(s)',
        null,
        'External Port',
        'Internal Port',
        null,
      ]"
    >
      @for (row of rows(); track row.externalPort; let i = $index) {
        <tr>
          <td class="interfaces">
            @for (iface of row.interfaces; track iface) {
              <div>{{ iface }}</div>
            }
          </td>
          <td class="status">
            @if (loading()[i]) {
              <tui-loader size="s" />
            } @else if (results()[i] === true) {
              <tui-icon class="g-positive" icon="@tui.check" />
            } @else if (results()[i] === false) {
              <tui-icon class="g-negative" icon="@tui.x" />
            } @else {
              <tui-icon class="g-secondary" icon="@tui.minus" />
            }
          </td>
          <td>{{ row.externalPort }}</td>
          <td>{{ row.internalPort }}</td>
          <td>
            <button
              tuiButton
              size="s"
              [loading]="!!loading()[i]"
              (click)="testPort(i, row.externalPort)"
            >
              {{ 'Test' | i18n }}
            </button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="5">
            <app-placeholder icon="@tui.list-x">
              {{ 'No port forwarding rules' | i18n }}
            </app-placeholder>
          </td>
        </tr>
      }
    </table>
  `,
  styles: `
    p {
      margin: 0 0 1rem 0;
    }

    .interfaces {
      white-space: nowrap;
    }

    tui-icon {
      font-size: 1.3rem;
      vertical-align: text-bottom;
    }

    .status {
      width: 3.2rem;
    }

    td:last-child {
      text-align: end;
    }

    :host-context(tui-root._mobile) table {
      thead {
        display: table-header-group !important;
      }

      tr {
        display: table-row !important;
        box-shadow: none !important;
      }

      td,
      th {
        padding: 0.5rem 0.5rem !important;
        font: var(--tui-font-text-s) !important;
        color: var(--tui-text-primary) !important;
        font-weight: normal !important;
      }

      th {
        font-weight: bold !important;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    i18nPipe,
    TableComponent,
    PlaceholderComponent,
    TuiIcon,
    TuiLoader,
    TuiButtonLoading,
  ],
})
export class PortForwardsModalComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)

  readonly context =
    injectContext<TuiDialogContext<void, PortForwardsModalData>>()

  readonly loading = signal<Record<number, boolean>>({})
  readonly results = signal<Record<number, boolean>>({})

  private readonly portForwards$ = combineLatest([
    this.patch.watch$('serverInfo', 'network', 'host', 'portForwards').pipe(
      map(pfs =>
        pfs.map(pf => ({
          ...pf,
          interfaces: ['StartOS - UI'],
        })),
      ),
    ),
    this.patch.watch$('packageData').pipe(
      map(pkgData => {
        const rows: Array<{
          src: string
          dst: string
          gateway: string
          interfaces: string[]
        }> = []

        for (const [pkgId, pkg] of Object.entries(pkgData)) {
          const title =
            pkg.stateInfo.manifest?.title ??
            pkg.stateInfo.installingInfo?.newManifest?.title ??
            pkgId

          for (const [hostId, host] of Object.entries(pkg.hosts)) {
            // Find interface names pointing to this host
            const ifaceNames: string[] = []
            for (const iface of Object.values(pkg.serviceInterfaces)) {
              if (iface.addressInfo.hostId === hostId) {
                ifaceNames.push(`${title} - ${iface.name}`)
              }
            }

            const label =
              ifaceNames.length > 0 ? ifaceNames : [`${title} - ${hostId}`]

            for (const pf of host.portForwards) {
              rows.push({ ...pf, interfaces: label })
            }
          }
        }

        return rows
      }),
    ),
  ]).pipe(
    map(([osForwards, pkgForwards]) => {
      const gatewayId = this.context.data.gatewayId
      const all = [...osForwards, ...pkgForwards].filter(
        pf => pf.gateway === gatewayId,
      )

      // Group by (externalPort, internalPort)
      const grouped = new Map<string, PortForwardRow>()

      for (const pf of all) {
        const src = parseSocketAddr(pf.src)
        const dst = parseSocketAddr(pf.dst)
        const key = `${src.port}:${dst.port}`

        const existing = grouped.get(key)
        if (existing) {
          for (const iface of pf.interfaces) {
            if (!existing.interfaces.includes(iface)) {
              existing.interfaces.push(iface)
            }
          }
        } else {
          grouped.set(key, {
            interfaces: [...pf.interfaces],
            externalPort: src.port,
            internalPort: dst.port,
          })
        }
      }

      return [...grouped.values()].sort(
        (a, b) => a.externalPort - b.externalPort,
      )
    }),
  )

  readonly rows = toSignal(this.portForwards$, { initialValue: [] })

  async testPort(index: number, port: number) {
    this.loading.update(l => ({ ...l, [index]: true }))

    try {
      const result = await this.api.checkPort({
        gateway: this.context.data.gatewayId,
        port,
      })

      this.results.update(r => ({ ...r, [index]: result.reachable }))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading.update(l => ({ ...l, [index]: false }))
    }
  }
}

export const PORT_FORWARDS_MODAL = new PolymorpheusComponent(
  PortForwardsModalComponent,
)
