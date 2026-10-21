import { Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule, UntypedFormGroup } from '@angular/forms'
import {
  DnsSourcePipe,
  DocsLinkDirective,
  i18nPipe,
  TaskService,
} from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-core'
import {
  tuiMarkControlAsTouchedAndValidate,
  TuiValueChanges,
} from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiConfirmService } from '@taiga-ui/kit'
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { injectContext } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { combineLatest, first, switchMap } from 'rxjs'

import { FormGroupComponent } from 'src/app/routes/portal/components/form/containers/group.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService, trim } from 'src/app/services/form.service'
import { gatewayName } from 'src/app/services/gateway.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

@Component({
  template: `
    <header tuiHeader>
      <h2 tuiTitle [id]="context.id">
        {{ 'DNS' | i18n }}
        <span tuiSubtitle>{{ data()?.name }} ({{ context.data }})</span>
      </h2>
    </header>
    @if (data(); as d) {
      @if (d.gateway; as gateway) {
        <form
          [formGroup]="d.form"
          (tuiValueChanges)="confirm.markAsDirty()"
          (submit.prevent)="save(d.form)"
        >
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
            {{ gateway.dns.source | dnsSource | i18n }}
          </p>
          @if (gateway.dns.source === 'public-fallback') {
            <p tuiNotification appearance="warning" role="status">
              {{
                'This gateway uses Cloudflare public DNS because it provides no usable DNS servers. Queries travel through this gateway.'
                  | i18n
              }}
            </p>
          }
          <form-group [spec]="d.spec" />
          @for (warn of d.warn; track $index) {
            <p tuiNotification appearance="warning">{{ warn }}</p>
          }
          <div tuiForm="m">
            <footer>
              <a
                tuiButton
                appearance="flat-grayscale"
                docsLink
                path="/start-os/dns.html"
              >
                {{ 'Documentation' | i18n }}
              </a>
              <button tuiButton [disabled]="d.form.pristine" type="submit">
                {{ 'Save' | i18n }}
              </button>
            </footer>
          </div>
        </form>
      } @else {
        <p tuiNotification appearance="warning">
          {{ 'Gateway is no longer available' | i18n }}
        </p>
      }
    }
  `,
  imports: [
    ReactiveFormsModule,
    DnsSourcePipe,
    DocsLinkDirective,
    FormGroupComponent,
    i18nPipe,
    TuiButton,
    TuiForm,
    TuiHeader,
    TuiNotification,
    TuiTitle,
    TuiValueChanges,
  ],
})
export default class GatewayDns {
  private readonly tasks = inject(TaskService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)
  protected readonly confirm = inject(TuiConfirmService)
  protected readonly context = injectContext<TuiDialogContext<void, string>>()

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
    ]).pipe(
      switchMap(async ([pkgs, { gateways, host }]) => {
        const gateway = gateways[this.context.data]
        const spec = await this.spec
        const current: (typeof this.dnsSpec._TYPE)['strategy'] = gateway?.dns
          .staticServers
          ? {
              selection: 'static',
              value: { servers: gateway.dns.staticServers },
            }
          : { selection: 'automatic', value: {} }
        const key = JSON.stringify([
          this.context.data,
          gateway?.dns.staticServers,
        ])
        const form =
          this.currentForm?.key === key
            ? this.currentForm.form
            : this.formService.createForm(spec, { strategy: current })
        if (this.currentForm?.form !== form) this.confirm.markAsPristine()
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
          for (const [id, g] of Object.entries(gateways)) {
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
                `${this.i18n.transform('Warning. StartOS is currently using the following gateway for DNS')}: ${gatewayName(id, g)}. ${this.i18n.transform('If you intend to use this gateway for private domain resolution, set alternative static DNS servers using the form above.')}`,
              )
            }
          }
        }
        return {
          gateway,
          name: gatewayName(this.context.data, gateway),
          spec,
          form,
          warn,
        }
      }),
    ),
  )

  protected async save(form: UntypedFormGroup): Promise<void> {
    trim(form)
    tuiMarkControlAsTouchedAndValidate(form)
    if (form.invalid) return
    const { strategy } = form.value as typeof this.dnsSpec._TYPE
    if (
      await this.tasks.run(async () => {
        await this.api.rpc.net.dns.setStatic({
          gateway: this.context.data,
          servers:
            strategy.selection === 'automatic' ? null : strategy.value.servers,
        })
      }, 'Saving')
    ) {
      this.confirm.markAsPristine()
      this.context.completeWith()
    }
  }
}
