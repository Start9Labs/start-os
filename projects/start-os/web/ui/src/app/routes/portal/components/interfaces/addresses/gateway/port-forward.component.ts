import { Component, computed, inject, signal } from '@angular/core'
import { ErrorService, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-core'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { TuiButtonLoading } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PortCheckIconComponent } from 'src/app/routes/portal/components/port-check-icon.component'
import { PortCheckWarningsComponent } from 'src/app/routes/portal/components/port-check-warnings.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { formatPortRange } from 'src/app/utils/format-port-range'
import { DnsGateway } from './dns.component'

export type PortForwardValidationData = {
  gateway: DnsGateway
  port: number
  internalPort: number
  count: number
  initialResults?: { portResult: T.CheckPortRes | null }
}

@Component({
  selector: 'port-forward-validation',
  template: `
    @let gatewayName =
      context.data.gateway.name || context.data.gateway.ipInfo.name;

    <h2>{{ 'Port Forwarding' | i18n }}</h2>
    <p>
      {{ 'In your gateway' | i18n }} "{{ gatewayName }}",
      {{ 'create this port forwarding rule' | i18n }}
    </p>

    @let portRes = portResult();

    <div class="desktop">
      <table
        [class.range-table]="isRange"
        [appTable]="
          isRange
            ? ['External Range', 'Internal Range']
            : [null, 'External Port', 'Internal Port', null]
        "
      >
        <tr>
          @if (!isRange) {
            <td class="status">
              <port-check-icon [result]="portRes" [loading]="loading()" />
            </td>
          }
          <td>{{ portDisplay }}</td>
          <td>{{ internalPortDisplay }}</td>
          @if (!isRange) {
            <td>
              <button
                tuiButton
                size="s"
                [loading]="loading()"
                (click)="testPort()"
              >
                {{ 'Test' | i18n }}
              </button>
            </td>
          }
        </tr>
      </table>
    </div>
    <div class="mobile">
      <div class="card">
        @if (!isRange) {
          <div class="card-status">
            <port-check-icon [result]="portRes" [loading]="loading()" />
          </div>
        }
        <div class="card-fields">
          <div class="field">
            <span class="field-label">
              {{ (isRange ? 'External Range' : 'External Port') | i18n }}
            </span>
            <span>{{ portDisplay }}</span>
          </div>
          <div class="field">
            <span class="field-label">
              {{ (isRange ? 'Internal Range' : 'Internal Port') | i18n }}
            </span>
            <span>{{ internalPortDisplay }}</span>
          </div>
        </div>
        @if (!isRange) {
          <button tuiButton size="s" [loading]="loading()" (click)="testPort()">
            {{ 'Test' | i18n }}
          </button>
        }
      </div>
    </div>

    <port-check-warnings [result]="portRes" />

    @if (!isManualMode) {
      <footer class="g-buttons padding-top">
        <button
          tuiButton
          appearance="flat"
          [disabled]="portOk()"
          (click)="context.completeWith()"
        >
          {{ 'Later' | i18n }}
        </button>
        <button
          tuiButton
          [disabled]="!portOk()"
          (click)="context.completeWith()"
        >
          {{ 'Done' | i18n }}
        </button>
      </footer>
    }
  `,
  styles: `
    h2 {
      margin: 2rem 0 0 0;
    }

    p {
      margin-top: 0.5rem;
    }

    tui-icon {
      font-size: 1.3rem;
      vertical-align: text-bottom;
    }

    .status {
      width: 3.2rem;
    }

    .padding-top {
      padding-top: 2rem;
    }

    td:last-child {
      text-align: end;
    }

    // A range table has no status/Test columns, so its last cell is the
    // internal-range value — keep it left-aligned with its header.
    .range-table td:last-child {
      text-align: start;
    }

    footer {
      margin-top: 1.5rem;
    }

    .mobile {
      display: none;
    }

    .card {
      display: flex;
      align-items: center;
      gap: 1rem;
      padding: 1rem;
      border: 1px solid var(--tui-border-normal);
      border-radius: var(--tui-radius-l);
      margin-top: 1rem;
    }

    .card-status {
      flex-shrink: 0;
      width: 1.5rem;
      text-align: center;
    }

    .card-fields {
      flex: 1;
      min-width: 0;
    }

    .field {
      display: flex;
      gap: 0.5rem;
    }

    .field-label {
      color: var(--tui-text-secondary);
      font: var(--tui-typography-body-s);

      &::after {
        content: ':';
      }
    }

    :host-context(tui-root._mobile) {
      .desktop {
        display: none;
      }

      .mobile {
        display: block;
      }
    }
  `,
  imports: [
    TuiButton,
    i18nPipe,
    TableComponent,
    TuiButtonLoading,
    PortCheckIconComponent,
    PortCheckWarningsComponent,
  ],
})
export class PortForwardValidationComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly context =
    injectContext<TuiDialogContext<void, PortForwardValidationData>>()

  // A port range forwards a span of ports and can't be tested a port at a time.
  readonly isRange = this.context.data.count > 1
  readonly portDisplay = formatPortRange(
    this.context.data.port,
    this.context.data.count,
  )
  readonly internalPortDisplay = formatPortRange(
    this.context.data.internalPort,
    this.context.data.count,
  )

  readonly loading = signal(false)
  readonly portResult = signal<T.CheckPortRes | undefined>(undefined)

  readonly portOk = computed(() => {
    const result = this.portResult()
    return (
      !!result?.openInternally &&
      !!result?.openExternally &&
      !!result?.hairpinning
    )
  })

  readonly isManualMode = !this.context.data.initialResults

  constructor() {
    const initial = this.context.data.initialResults
    if (initial) {
      if (initial.portResult) this.portResult.set(initial.portResult)
    }
  }

  async testPort() {
    this.loading.set(true)

    try {
      const result = await this.api.checkPort({
        gateway: this.context.data.gateway.id,
        port: this.context.data.port,
      })

      this.portResult.set(result)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading.set(false)
    }
  }
}

export const PORT_FORWARD_VALIDATION = new PolymorpheusComponent(
  PortForwardValidationComponent,
)
