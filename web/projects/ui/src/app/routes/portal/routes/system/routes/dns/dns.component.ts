import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import {
  DocsLinkDirective,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { combineLatest, first, switchMap } from 'rxjs'
import { FormGroupComponent } from 'src/app/routes/portal/components/form/containers/group.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ 'DNS Servers' | i18n }}
    </ng-container>
    @if (data(); as d) {
      <form [formGroup]="d.form">
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>
              {{ 'DNS Servers' | i18n }}
              <a
                tuiIconButton
                size="xs"
                docsLink
                path="/user-manual/dns.html"
                appearance="icon"
                iconStart="@tui.external-link"
              >
                {{ 'Documentation' | i18n }}
              </a>
            </b>
          </h3>
        </header>

        <form-group [spec]="d.spec" />

        @for (warn of d.warn; track $index) {
          <p>{{ warn }}</p>
        }

        <footer>
          <button
            tuiButton
            size="l"
            [disabled]="d.form.invalid || d.form.pristine"
            (click)="save(d.form.value)"
          >
            {{ 'Save' | i18n }}
          </button>
        </footer>
      </form>
    }
  `,
  styles: `
    :host {
      max-width: 36rem;
    }

    form header,
    form footer {
      margin: 1rem 0;
      display: flex;
      gap: 1rem;
    }

    footer {
      justify-content: flex-end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    FormGroupComponent,
    TuiButton,
    TuiHeader,
    TuiTitle,
    RouterLink,
    TitleDirective,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class SystemDnsComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  private readonly dnsSpec = ISB.InputSpec.of({
    strategy: ISB.Value.union({
      name: 'strategy',
      default: 'dhcp',
      variants: ISB.Variants.of({
        dhcp: {
          name: 'DHCP',
          spec: ISB.InputSpec.of({
            servers: ISB.Value.dynamicText(() => ({
              name: this.i18n.transform('DHCP Servers'),
              default: null,
              required: true,
              disabled: this.i18n.transform('Cannot edit DHCP servers'),
            })),
          }),
        },
        static: {
          name: this.i18n.transform('Static'),
          spec: ISB.InputSpec.of({
            servers: ISB.Value.list(
              ISB.List.text(
                {
                  name: this.i18n.transform('Static Servers'),
                  minLength: 1,
                  maxLength: 3,
                },
                { placeholder: '1.1.1.1' },
              ),
            ),
          }),
        },
      }),
    }),
  })

  readonly data = toSignal(
    combineLatest([
      this.patch.watch$('packageData').pipe(first()),
      this.patch.watch$('serverInfo', 'network'),
    ]).pipe(
      switchMap(async ([pkgs, { gateways, dns }]) => {
        const spec = await configBuilderToSpec(this.dnsSpec)

        const dhcpServers = { servers: dns.dhcpServers.join(', ') }

        const current: (typeof this.dnsSpec._TYPE)['strategy'] =
          dns.staticServers
            ? {
                selection: 'static',
                value: { servers: dns.staticServers || [] },
                other: {
                  dhcp: dhcpServers,
                },
              }
            : {
                selection: 'dhcp',
                value: dhcpServers,
              }

        const form = this.formService.createForm(spec, { strategy: current })

        let warn: string[] = []

        if (
          Object.values(pkgs).some(p =>
            Object.values(p.hosts).some(h => h?.privateDomains.length),
          )
        ) {
          Object.values(gateways)
            .filter(g =>
              (dns.staticServers || dns.dhcpServers).some(d =>
                g.ipInfo?.lanIp.includes(d),
              ),
            )
            .map(
              g =>
                `${this.i18n.transform('Warning. StartOS is currently using the following gateway for DNS')}: ${g.ipInfo!.name}. ${this.i18n.transform('If you intend to use this gateway for private domain resolution, set alternative static DNS servers using the form above.')}`,
            )
        }

        return {
          spec,
          form,
          warn,
        }
      }),
    ),
  )

  async save(value: typeof this.dnsSpec._TYPE): Promise<void> {
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.setDns({
        servers:
          value.strategy.selection === 'dhcp'
            ? null
            : value.strategy.value.servers,
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
