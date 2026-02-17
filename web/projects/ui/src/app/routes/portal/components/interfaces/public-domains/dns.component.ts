import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon } from '@taiga-ui/core'
import {
  TuiButtonLoading,
  TuiSwitch,
  tuiSwitchOptionsProvider,
} from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { T } from '@start9labs/start-sdk'
import { parse } from 'tldts'

export type DnsGateway = T.NetworkInterfaceInfo & {
  id: string
  ipInfo: T.IpInfo
}

export type DomainValidationData = {
  fqdn: string
  gateway: DnsGateway
  port: number
  dnsPass: boolean
  portPass: boolean
}

@Component({
  selector: 'domain-validation',
  template: `
    @let wanIp = context.data.gateway.ipInfo.wanIp || ('Error' | i18n);
    @let gatewayName =
      context.data.gateway.name || context.data.gateway.ipInfo.name;
    @let internalIp = context.data.gateway.ipInfo.lanIp[0] || ('Error' | i18n);

    <h3>{{ 'DNS' | i18n }}</h3>
    <p>
      {{ 'In your domain registrar for' | i18n }} {{ domain }},
      {{ 'create this DNS record' | i18n }}
    </p>

    @if (context.data.gateway.ipInfo.deviceType !== 'wireguard') {
      <label>
        IP
        <input
          type="checkbox"
          appearance="flat"
          tuiSwitch
          [(ngModel)]="ddns"
          (ngModelChange)="dnsPass.set(undefined)"
        />
        {{ 'Dynamic DNS' | i18n }}
      </label>
    }

    <table [appTable]="[null, 'Type', 'Host', 'Value', null]">
      <tr>
        <td class="status">
          @if (dnsPass() === true) {
            <tui-icon class="g-positive" icon="@tui.check" />
          } @else if (dnsPass() === false) {
            <tui-icon class="g-negative" icon="@tui.x" />
          }
        </td>
        <td>{{ ddns ? 'ALIAS' : 'A' }}</td>
        <td>*</td>
        <td>{{ ddns ? '[DDNS Address]' : wanIp }}</td>
        <td>
          <button
            tuiButton
            size="s"
            [loading]="dnsLoading()"
            (click)="testDns()"
          >
            {{ 'Test' | i18n }}
          </button>
        </td>
      </tr>
    </table>

    <h3>{{ 'Port Forwarding' | i18n }}</h3>
    <p>
      {{ 'In your gateway' | i18n }} "{{ gatewayName }}",
      {{ 'create this port forwarding rule' | i18n }}
    </p>

    <table
      [appTable]="[null, 'External Port', 'Internal IP', 'Internal Port', null]"
    >
      <tr>
        <td class="status">
          @if (portPass() === true) {
            <tui-icon class="g-positive" icon="@tui.check" />
          } @else if (portPass() === false) {
            <tui-icon class="g-negative" icon="@tui.x" />
          }
        </td>
        <td>{{ context.data.port }}</td>
        <td>{{ internalIp }}</td>
        <td>{{ context.data.port }}</td>
        <td>
          <button
            tuiButton
            size="s"
            [loading]="portLoading()"
            (click)="testPort()"
          >
            {{ 'Test' | i18n }}
          </button>
        </td>
      </tr>
    </table>

    <footer class="g-buttons">
      <button
        tuiButton
        appearance="flat"
        [disabled]="allPass()"
        (click)="context.completeWith()"
      >
        {{ 'Later' | i18n }}
      </button>
      <button
        tuiButton
        [disabled]="!allPass()"
        (click)="context.completeWith()"
      >
        {{ 'Done' | i18n }}
      </button>
    </footer>
  `,
  styles: `
    label {
      display: flex;
      gap: 0.75rem;
      align-items: center;
      margin: 1rem 0;
    }

    h3 {
      margin: 1.5rem 0 0.5rem;

      &:first-child {
        margin-top: 0;
      }
    }

    tui-icon {
      font-size: 1rem;
      vertical-align: text-bottom;
    }

    .status {
      width: 1.5rem;
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
  providers: [
    tuiSwitchOptionsProvider({
      appearance: () => 'glass',
      icon: () => '',
    }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    i18nPipe,
    TableComponent,
    TuiSwitch,
    FormsModule,
    TuiButtonLoading,
    TuiIcon,
  ],
})
export class DomainValidationComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly ddns = false

  readonly context =
    injectContext<TuiDialogContext<void, DomainValidationData>>()

  readonly domain =
    parse(this.context.data.fqdn).domain || this.context.data.fqdn

  readonly dnsLoading = signal(false)
  readonly portLoading = signal(false)
  readonly dnsPass = signal<boolean | undefined>(this.context.data.dnsPass)
  readonly portPass = signal<boolean | undefined>(this.context.data.portPass)

  readonly allPass = computed(
    () => this.dnsPass() === true && this.portPass() === true,
  )

  async testDns() {
    this.dnsLoading.set(true)

    try {
      const ip = await this.api.queryDns({
        fqdn: this.context.data.fqdn,
      })

      this.dnsPass.set(ip === this.context.data.gateway.ipInfo.wanIp)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.dnsLoading.set(false)
    }
  }

  async testPort() {
    this.portLoading.set(true)

    try {
      const result = await this.api.checkPort({
        gateway: this.context.data.gateway.id,
        port: this.context.data.port,
      })

      this.portPass.set(result.reachable)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.portLoading.set(false)
    }
  }
}

export const DOMAIN_VALIDATION = new PolymorpheusComponent(
  DomainValidationComponent,
)
