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

@Component({
  selector: 'dns',
  template: `
    <p>{{ context.data.message }}</p>

    @let wanIp = context.data.gateway.ipInfo.wanIp || ('Error' | i18n);

    @if (context.data.gateway.ipInfo.deviceType !== 'wireguard') {
      <label>
        IP
        <input
          type="checkbox"
          tuiSwitch
          [(ngModel)]="ddns"
          (ngModelChange)="pass.set(undefined)"
        />
        {{ 'Dynamic DNS' | i18n }}
      </label>
    }

    <table [appTable]="['Type', 'Host', 'Value', 'Purpose']">
      @for (row of rows(); track $index) {
        <tr>
          <td>
            @if (pass() === true) {
              <tui-icon class="g-positive" icon="@tui.check" />
            } @else if (pass() === false) {
              <tui-icon class="g-negative" icon="@tui.x" />
            }
            {{ ddns ? 'ALIAS' : 'A' }}
          </td>
          <td>{{ row.host }}</td>
          <td>{{ ddns ? '[DDNS Address]' : wanIp }}</td>
          <td>{{ row.purpose }}</td>
        </tr>
      }
    </table>

    <footer class="g-buttons">
      <button tuiButton [loading]="loading()" (click)="testDns()">
        {{ 'Test' | i18n }}
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

    tui-icon {
      font-size: 1rem;
      vertical-align: text-bottom;
    }
  `,
  providers: [
    tuiSwitchOptionsProvider({
      appearance: () => 'primary',
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
export class DnsComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  readonly ddns = false

  readonly context =
    injectContext<
      TuiDialogContext<
        void,
        { fqdn: string; gateway: DnsGateway; message: string }
      >
    >()

  readonly loading = signal(false)
  readonly pass = signal<boolean | undefined>(undefined)

  readonly rows = computed<{ host: string; purpose: string }[]>(() => {
    const { domain, subdomain } = parse(this.context.data.fqdn)

    if (!subdomain) {
      return [
        {
          host: '@',
          purpose: domain!,
        },
      ]
    }

    const segments = subdomain.split('.').slice(1)

    const subdomains = this.i18n.transform('all subdomains of')

    return [
      {
        host: subdomain,
        purpose: `only ${subdomain}`,
      },
      ...segments.map((_, i) => {
        const parent = segments.slice(i).join('.')
        return {
          host: `*.${parent}`,
          purpose: `${subdomains} ${parent}`,
        }
      }),
      {
        host: '*',
        purpose: `${subdomains} ${domain}`,
      },
    ]
  })

  async testDns() {
    this.pass.set(undefined)
    this.loading.set(true)

    try {
      const ip = await this.api.queryDns({
        fqdn: this.context.data.fqdn,
      })

      this.pass.set(ip === this.context.data.gateway.ipInfo.wanIp)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading.set(false)
    }
  }
}

export const DNS = new PolymorpheusComponent(DnsComponent)
