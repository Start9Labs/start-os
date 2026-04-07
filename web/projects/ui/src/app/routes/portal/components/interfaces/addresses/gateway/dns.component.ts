import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon, TuiLoader } from '@taiga-ui/core'
import {
  TuiButtonLoading,
  TuiSwitch,
  tuiSwitchOptionsProvider,
} from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PortCheckIconComponent } from 'src/app/routes/portal/components/port-check-icon.component'
import { PortCheckWarningsComponent } from 'src/app/routes/portal/components/port-check-warnings.component'
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
  initialResults?: { dnsPass: boolean; portResult: T.CheckPortRes | null }
}

@Component({
  selector: 'domain-validation',
  template: `
    @let wanIp = context.data.gateway.ipInfo.wanIp || ('Error' | i18n);
    @let gatewayName =
      context.data.gateway.name || context.data.gateway.ipInfo.name;

    <h2>{{ 'DNS' | i18n }}</h2>
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

    <div class="desktop">
      <table [appTable]="[null, 'Type', 'Host', 'Value', null]">
        <tr>
          <td class="status">
            @if (dnsLoading()) {
              <tui-loader size="s" />
            } @else if (dnsPass() === true) {
              <tui-icon class="g-positive" icon="@tui.check" />
            } @else if (dnsPass() === false) {
              <tui-icon class="g-negative" icon="@tui.x" />
            } @else {
              <tui-icon class="g-secondary" icon="@tui.minus" />
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
    </div>
    <div class="mobile">
      <div class="card">
        <div class="card-status">
          @if (dnsLoading()) {
            <tui-loader size="s" />
          } @else if (dnsPass() === true) {
            <tui-icon class="g-positive" icon="@tui.check" />
          } @else if (dnsPass() === false) {
            <tui-icon class="g-negative" icon="@tui.x" />
          } @else {
            <tui-icon class="g-secondary" icon="@tui.minus" />
          }
        </div>
        <div class="card-fields">
          <div class="field">
            <span class="field-label">{{ 'Type' | i18n }}</span>
            <span>{{ ddns ? 'ALIAS' : 'A' }}</span>
          </div>
          <div class="field">
            <span class="field-label">{{ 'Host' | i18n }}</span>
            <span>*</span>
          </div>
          <div class="field">
            <span class="field-label">{{ 'Value' | i18n }}</span>
            <span>{{ ddns ? '[DDNS Address]' : wanIp }}</span>
          </div>
        </div>
        <button tuiButton size="s" [loading]="dnsLoading()" (click)="testDns()">
          {{ 'Test' | i18n }}
        </button>
      </div>
    </div>

    <h2>{{ 'Port Forwarding' | i18n }}</h2>
    <p>
      {{ 'In your gateway' | i18n }} "{{ gatewayName }}",
      {{ 'create this port forwarding rule' | i18n }}
    </p>

    @let portRes = portResult();

    <div class="desktop">
      <table [appTable]="[null, 'External Port', 'Internal Port', null]">
        <tr>
          <td class="status">
            <port-check-icon [result]="portRes" [loading]="portLoading()" />
          </td>
          <td>{{ context.data.port }}</td>
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
    </div>
    <div class="mobile">
      <div class="card">
        <div class="card-status">
          <port-check-icon [result]="portRes" [loading]="portLoading()" />
        </div>
        <div class="card-fields">
          <div class="field">
            <span class="field-label">{{ 'External Port' | i18n }}</span>
            <span>{{ context.data.port }}</span>
          </div>
          <div class="field">
            <span class="field-label">{{ 'Internal Port' | i18n }}</span>
            <span>{{ context.data.port }}</span>
          </div>
        </div>
        <button
          tuiButton
          size="s"
          [loading]="portLoading()"
          (click)="testPort()"
        >
          {{ 'Test' | i18n }}
        </button>
      </div>
    </div>

    <port-check-warnings [result]="portRes" />

    @if (!isManualMode) {
      <footer class="g-buttons padding-top">
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
    }
  `,
  styles: `
    label {
      display: flex;
      gap: 0.75rem;
      align-items: center;
      margin: 1rem 0;
    }

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
    TuiLoader,
    PortCheckIconComponent,
    PortCheckWarningsComponent,
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
  readonly dnsPass = signal<boolean | undefined>(undefined)
  readonly portResult = signal<T.CheckPortRes | undefined>(undefined)

  readonly allPass = computed(() => {
    const result = this.portResult()
    return (
      this.dnsPass() === true &&
      !!result?.openInternally &&
      !!result?.openExternally
    )
  })

  readonly isManualMode = !this.context.data.initialResults

  constructor() {
    const initial = this.context.data.initialResults
    if (initial) {
      this.dnsPass.set(initial.dnsPass)
      if (initial.portResult) this.portResult.set(initial.portResult)
    }
  }

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

      this.portResult.set(result)
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
