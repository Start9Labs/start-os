import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { ErrorService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon, TuiLoader } from '@taiga-ui/core'
import { TuiButtonLoading } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DnsGateway } from './dns.component'

export type PortForwardValidationData = {
  gateway: DnsGateway
  port: number
  initialResults?: { portPass: boolean }
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

    <table [appTable]="[null, 'External Port', 'Internal Port', null]">
      <tr>
        <td class="status">
          @if (loading()) {
            <tui-loader size="s" />
          } @else if (pass() === true) {
            <tui-icon class="g-positive" icon="@tui.check" />
          } @else if (pass() === false) {
            <tui-icon class="g-negative" icon="@tui.x" />
          } @else {
            <tui-icon class="g-secondary" icon="@tui.minus" />
          }
        </td>
        <td>{{ context.data.port }}</td>
        <td>{{ context.data.port }}</td>
        <td>
          <button tuiButton size="s" [loading]="loading()" (click)="testPort()">
            {{ 'Test' | i18n }}
          </button>
        </td>
      </tr>
    </table>

    @if (!isManualMode) {
      <footer class="g-buttons padding-top">
        <button
          tuiButton
          appearance="flat"
          [disabled]="pass() === true"
          (click)="context.completeWith()"
        >
          {{ 'Later' | i18n }}
        </button>
        <button
          tuiButton
          [disabled]="pass() !== true"
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

    footer {
      margin-top: 1.5rem;
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
    TuiButtonLoading,
    TuiIcon,
    TuiLoader,
  ],
})
export class PortForwardValidationComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly context =
    injectContext<TuiDialogContext<void, PortForwardValidationData>>()

  readonly loading = signal(false)
  readonly pass = signal<boolean | undefined>(undefined)

  readonly isManualMode = !this.context.data.initialResults

  constructor() {
    const initial = this.context.data.initialResults
    if (initial) {
      this.pass.set(initial.portPass)
    }
  }

  async testPort() {
    this.loading.set(true)

    try {
      const result = await this.api.checkPort({
        gateway: this.context.data.gateway.id,
        port: this.context.data.port,
      })

      this.pass.set(result.reachable)
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
