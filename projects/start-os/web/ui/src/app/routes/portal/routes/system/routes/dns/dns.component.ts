import { Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  FormsModule,
  ReactiveFormsModule,
  UntypedFormGroup,
} from '@angular/forms'
import { ActivatedRoute, Router, RouterLink } from '@angular/router'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { DocsLinkDirective, i18nPipe, TaskService } from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-core'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiButton, TuiInput, TuiNotification, TuiTitle } from '@taiga-ui/core'
import { TuiChevron, TuiDataListWrapper, TuiSelect } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { combineLatest, first, switchMap } from 'rxjs'
import { FormGroupComponent } from 'src/app/routes/portal/components/form/containers/group.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService, trim } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

@Component({
  template: `
    <ng-container *title>
      <div>
        <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
          {{ 'Back' | i18n }}
        </a>
        {{ 'DNS Servers' | i18n }}
        <a
          tuiIconButton
          size="xs"
          docsLink
          path="/start-os/dns.html"
          appearance="icon"
          iconStart="@tui.book-open-text"
        ></a>
      </div>
    </ng-container>
    @if (data(); as d) {
      <p>
        {{
          'StartOS and services query the OS DNS server, which uses the DNS settings of their outbound gateway.'
            | i18n
        }}
      </p>
      <p>
        {{
          'A service-specific outbound gateway overrides the system default. Services without an override follow the system setting; Auto uses the physical default route, preferring IPv4 over IPv6.'
            | i18n
        }}
      </p>
      <p>
        {{
          'Host lookups of configured WireGuard endpoint names use the automatic underlay so tunnels can reconnect. Service lookups still follow their outbound gateway.'
            | i18n
        }}
      </p>
      <tui-textfield
        tuiChevron
        [stringify]="d.stringify"
        [tuiTextfieldCleaner]="false"
      >
        <label tuiLabel>{{ 'Gateway' | i18n }}</label>
        @if (mobile) {
          <select
            tuiSelect
            [ngModel]="d.gatewayId"
            [items]="d.options"
            (ngModelChange)="selectGateway($event)"
          ></select>
        } @else {
          <input
            tuiSelect
            [ngModel]="d.gatewayId"
            (ngModelChange)="selectGateway($event)"
          />
        }
        <tui-data-list-wrapper *tuiDropdown [items]="d.options" />
      </tui-textfield>
      @if (d.gateway; as gateway) {
        @if (!gateway.ipInfo) {
          <p tuiNotification appearance="warning">
            {{ 'Disconnected' | i18n }}
          </p>
        }
        <p>
          <b>{{ 'Effective DNS servers' | i18n }}:</b>
          {{ gateway.dns.servers.join(', ') }}
        </p>
        <p>
          <b>{{ 'DNS source' | i18n }}:</b>
          @switch (gateway.dns.source) {
            @case ('gateway') {
              {{ 'Provided by gateway' | i18n }}
            }
            @case ('static') {
              {{ 'Static' | i18n }}
            }
            @case ('public-fallback') {
              {{ 'Public fallback (Cloudflare)' | i18n }}
            }
          }
        </p>
        @if (gateway.dns.source === 'public-fallback') {
          <p tuiNotification appearance="warning" role="status">
            {{
              'This gateway uses Cloudflare public DNS because it provides no usable DNS servers. Queries travel through this gateway.'
                | i18n
            }}
          </p>
        }
        <form [formGroup]="d.form" (submit.prevent)="save(d.gatewayId, d.form)">
          <header tuiHeader="body-l">
            <h3 tuiTitle>
              <b>
                {{ 'DNS Servers' | i18n }}
                <a
                  tuiIconButton
                  size="xs"
                  docsLink
                  path="/start-os/dns.html"
                  appearance="icon"
                  iconStart="@tui.book-open-text"
                >
                  {{ 'Documentation' | i18n }}
                </a>
              </b>
            </h3>
          </header>

          <form-group [spec]="d.spec" />

          @for (warn of d.warn; track $index) {
            <p tuiNotification appearance="warning">{{ warn }}</p>
          }

          <footer>
            <button
              tuiButton
              size="l"
              [disabled]="d.form.pristine"
              type="submit"
            >
              {{ 'Save' | i18n }}
            </button>
          </footer>
        </form>
      }
    }
  `,
  styles: `
    :host {
      max-width: 36rem;
    }

    :host-context(tui-root._mobile) [tuiHeader] {
      display: none;
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
  imports: [
    FormsModule,
    ReactiveFormsModule,
    FormGroupComponent,
    TuiButton,
    TuiInput,
    TuiNotification,
    TuiChevron,
    TuiDataListWrapper,
    TuiSelect,
    TuiHeader,
    TuiTitle,
    RouterLink,
    TitleDirective,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class SystemDnsComponent {
  private readonly tasks = inject(TaskService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  protected readonly mobile = inject(WA_IS_MOBILE)

  private readonly dnsSpec = ISB.InputSpec.of({
    strategy: ISB.Value.union({
      name: this.i18n.transform('DNS mode'),
      default: 'automatic',
      description: `<ul><li><b>${this.i18n.transform('Auto')}</b>: ${this.i18n.transform('Use gateway-provided DNS, or Cloudflare public DNS when none is provided')}</li><li><b>${this.i18n.transform('Static')}</b>: ${this.i18n.transform('Use DNS servers you specify manually')}</li></ul>`,
      variants: ISB.Variants.of({
        automatic: {
          name: this.i18n.transform('Auto'),
          spec: ISB.InputSpec.of({}),
        },
        static: {
          name: this.i18n.transform('Static'),
          spec: ISB.InputSpec.of({
            servers: ISB.Value.list(
              ISB.List.text(
                {
                  name: this.i18n.transform('Servers'),
                  minLength: 1,
                  maxLength: 3,
                },
                {
                  placeholder: '1.1.1.1',
                  patterns: [
                    {
                      regex: `^(${utils.regexes.ipv4.contains()}(:\\d{1,5})?|${utils.regexes.ipv6.contains()}|\\[${utils.regexes.ipv6.contains()}\\](:\\d{1,5})?)$`,
                      description: this.i18n.transform(
                        'Must be a valid IPv4 or Ipv6 address with optional port',
                      ),
                    },
                  ],
                },
              ),
            ),
          }),
        },
      }),
    }),
  })

  private readonly spec = configBuilderToSpec(this.dnsSpec)
  private currentForm?: { key: string; form: UntypedFormGroup }

  protected readonly data = toSignal(
    combineLatest([
      this.patch.watch$('packageData').pipe(first()),
      this.patch.watch$('serverInfo', 'network'),
      this.route.queryParamMap,
    ]).pipe(
      switchMap(async ([pkgs, { gateways, host }, query]) => {
        const options = Object.entries(gateways)
          .filter(
            ([, gateway]) =>
              !['bridge', 'loopback'].includes(
                gateway.ipInfo?.deviceType ?? '',
              ),
          )
          .map(([id]) => id)
        const requested = query.get('gateway')
        const gatewayId =
          requested && options.includes(requested)
            ? requested
            : (options[0] ?? '')
        const gateway = gateways[gatewayId]
        const spec = await this.spec
        const current: (typeof this.dnsSpec._TYPE)['strategy'] = gateway?.dns
          .staticServers
          ? {
              selection: 'static',
              value: { servers: gateway.dns.staticServers },
            }
          : { selection: 'automatic', value: {} }
        const key = JSON.stringify([gatewayId, gateway?.dns.staticServers])
        const form =
          this.currentForm?.key === key
            ? this.currentForm.form
            : this.formService.createForm(spec, { strategy: current })
        this.currentForm = { key, form }
        const warn: string[] = []

        if (
          gateway &&
          (Object.keys(host.privateDomains).length > 0 ||
            Object.values(pkgs).some(p =>
              Object.values(p.hosts).some(
                h => Object.keys(h?.privateDomains || {}).length,
              ),
            ))
        ) {
          for (const g of Object.values(gateways)) {
            if (
              gateway.dns.servers.some(server =>
                g.ipInfo?.lanIp.some(
                  ip =>
                    server === ip ||
                    (!ip.includes(':') && server.startsWith(ip + ':')) ||
                    server.startsWith('[' + ip + ']:'),
                ),
              )
            ) {
              warn.push(
                `${this.i18n.transform('Warning. StartOS is currently using the following gateway for DNS')}: ${g.name ?? g.ipInfo!.name}. ${this.i18n.transform('If you intend to use this gateway for private domain resolution, set alternative static DNS servers using the form above.')}`,
              )
            }
          }
        }

        return {
          options,
          gatewayId,
          gateway,
          stringify: (id: string) =>
            gateways[id]?.name ?? gateways[id]?.ipInfo?.name ?? id,
          spec,
          form,
          warn,
        }
      }),
    ),
  )

  protected selectGateway(gateway: string) {
    void this.router.navigate([], {
      relativeTo: this.route,
      queryParams: { gateway },
      queryParamsHandling: 'merge',
    })
  }

  protected async save(gateway: string, form: UntypedFormGroup): Promise<void> {
    trim(form)
    tuiMarkControlAsTouchedAndValidate(form)

    if (form.invalid) return

    const { strategy } = form.value as typeof this.dnsSpec._TYPE

    await this.tasks.run(async () => {
      await this.api.rpc.net.dns.setStatic({
        gateway,
        servers:
          strategy.selection === 'automatic' ? null : strategy.value.servers,
      })
      form.markAsPristine()
    }, 'Saving')
  }
}
