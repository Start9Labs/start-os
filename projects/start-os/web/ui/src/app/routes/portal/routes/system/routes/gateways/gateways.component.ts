import { Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { DocsLinkDirective, i18nPipe, TaskService } from '@start9labs/shared'
import { ISB } from '@start9labs/start-core'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { distinctUntilChanged, map, switchMap } from 'rxjs'

import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  GatewayService,
  outboundGatewayOptions,
} from 'src/app/services/gateway.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import {
  outboundSettingsParams,
  outboundSettingsSpec,
  outboundSettingsValue,
} from 'src/app/utils/outbound-settings'

import { GatewaysTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>
      <div>
        <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
          {{ 'Back' | i18n }}
        </a>
        {{ 'Gateways' | i18n }}
        <a
          tuiIconButton
          size="xs"
          docsLink
          path="/start-os/gateways.html"
          appearance="icon"
          iconStart="@tui.book-open-text"
        ></a>
      </div>
    </ng-container>

    <section class="g-card">
      <header>
        {{ 'Gateways' | i18n }}
        <a
          tuiIconButton
          size="xs"
          docsLink
          path="/start-os/gateways.html"
          appearance="icon"
          iconStart="@tui.book-open-text"
        >
          {{ 'Documentation' | i18n }}
        </a>
        <button
          tuiButton
          size="xs"
          [style.margin]="'0 0.5rem 0 auto'"
          iconStart="@tui.plus"
          (click)="add()"
        >
          {{ 'Add' | i18n }}
        </button>
      </header>
      <gateways-table />
    </section>

    @if (outboundForm(); as d) {
      <section class="outbound">
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>
              {{ 'Outbound Traffic' | i18n }}
              <a
                tuiIconButton
                size="xs"
                docsLink
                path="/start-os/gateways.html"
                fragment="#outbound-traffic"
                appearance="icon"
                iconStart="@tui.book-open-text"
              >
                {{ 'Documentation' | i18n }}
              </a>
            </b>
          </h3>
        </header>
        @for (config of [d]; track config.key) {
          <app-form
            [spec]="config.spec"
            [value]="config.value"
            [buttons]="outboundButtons"
          />
        }
      </section>
    }
  `,
  styles: `
    :host-context(tui-root._mobile) .g-card > header > [docsLink] {
      display: none;
    }

    .outbound {
      max-inline-size: 24rem;
      margin-block-start: 2rem;
    }

    .outbound header {
      margin-block-end: 1rem;
    }
  `,
  providers: [GatewayService],
  imports: [
    RouterLink,
    FormComponent,
    TuiButton,
    TuiTitle,
    TuiHeader,
    GatewaysTableComponent,
    TitleDirective,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class GatewaysComponent {
  private readonly tasks = inject(TaskService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  protected readonly outboundForm = toSignal(
    this.patch.watch$('serverInfo', 'network').pipe(
      map(network => ({
        gateway: network.defaultOutbound,
        dns: network.outboundDns,
        options: outboundGatewayOptions(
          network.gateways,
          network.defaultOutbound,
        ),
      })),
      distinctUntilChanged((a, b) => JSON.stringify(a) === JSON.stringify(b)),
      switchMap(async ({ gateway, dns, options }) => ({
        key: JSON.stringify([gateway, dns]),
        spec: await configBuilderToSpec(
          outboundSettingsSpec(this.i18n, options, 'Auto'),
        ),
        value: outboundSettingsValue(gateway, dns),
      })),
    ),
  )
  protected readonly outboundButtons = [
    {
      text: this.i18n.transform('Save'),
      handler: (input: ReturnType<typeof outboundSettingsValue>) =>
        this.tasks.run(
          async () =>
            await this.api.rpc.net.gateway.setDefaultOutbound(
              outboundSettingsParams(input),
            ),
          'Saving',
        ),
    },
  ]

  async add() {
    const spec = ISB.InputSpec.of({
      name: ISB.Value.text({
        name: this.i18n.transform('Name'),
        description: this.i18n.transform(
          'A name to easily identify the gateway',
        ),
        required: true,
        default: null,
        placeholder: 'StartTunnel 1',
      }),
      config: ISB.Value.union({
        name: this.i18n.transform('WireGuard Config File'),
        default: 'paste',
        variants: ISB.Variants.of({
          paste: {
            name: this.i18n.transform('Copy/Paste'),
            spec: ISB.InputSpec.of({
              file: ISB.Value.textarea({
                name: this.i18n.transform('File Contents'),
                default: null,
                required: true,
                minRows: 8,
                maxRows: 8,
              }),
            }),
          },
          select: {
            name: this.i18n.transform('Upload'),
            spec: ISB.InputSpec.of({
              file: ISB.Value.file({
                name: this.i18n.transform('File'),
                required: true,
                extensions: ['.conf'],
              }),
            }),
          },
        }),
      }),
    })

    this.formDialog.open(FormComponent, {
      label: 'Add Wireguard Gateway',
      data: {
        spec: await configBuilderToSpec(spec),
        buttons: [
          {
            text: this.i18n.transform('Save'),
            handler: async (input: typeof spec._TYPE) =>
              this.tasks.run(
                async () =>
                  await this.api.rpc.net.tunnel.add({
                    name: input.name,
                    config:
                      input.config.selection === 'paste'
                        ? input.config.value.file
                        : await (input.config.value.file as any as File).text(),
                    type: null, // let StartOS auto-detect from the config
                    setAsDefaultOutbound: false,
                  }),
                'Saving',
              ),
          },
        ],
      },
    })
  }
}
