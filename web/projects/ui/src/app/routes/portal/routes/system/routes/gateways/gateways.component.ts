import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  linkedSignal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { DocsLinkDirective, ErrorService, i18nPipe } from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import { TuiButton, TuiInput, TuiTitle } from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiNotificationMiddleService,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { GatewayService } from 'src/app/services/gateway.service'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { GatewaysTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ 'Gateways' | i18n }}
    </ng-container>

    <section class="g-card">
      <header>
        {{ 'Gateways' | i18n }}
        <a
          tuiIconButton
          size="xs"
          docsLink
          path="/start-os/user-manual/gateways.html"
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
          Add
        </button>
      </header>
      <gateways-table />
    </section>

    @if (outboundOptions(); as options) {
      <section class="outbound">
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>
              {{ 'Outbound Traffic' | i18n }}
              <a
                tuiIconButton
                size="xs"
                docsLink
                path="/start-os/user-manual/gateways.html"
                fragment="#outbound-traffic"
                appearance="icon"
                iconStart="@tui.book-open-text"
              >
                {{ 'Documentation' | i18n }}
              </a>
            </b>
          </h3>
        </header>
        <tui-textfield
          tuiChevron
          [stringify]="stringifyOutbound"
          [tuiTextfieldCleaner]="false"
        >
          <label tuiLabel>{{ 'Use gateway' | i18n }}</label>
          @if (mobile) {
            <select
              tuiSelect
              [ngModel]="selectedOutbound()"
              (ngModelChange)="selectedOutbound.set($event)"
              [items]="options"
            ></select>
          } @else {
            <input
              tuiSelect
              [ngModel]="selectedOutbound()"
              (ngModelChange)="selectedOutbound.set($event)"
            />
          }
          @if (!mobile) {
            <tui-data-list-wrapper *tuiDropdown [items]="options" />
          }
        </tui-textfield>
        <footer>
          <button
            tuiButton
            [disabled]="
              selectedOutbound()?.id ===
              (gatewayService.defaultOutbound() ?? null)
            "
            (click)="saveOutbound()"
          >
            {{ 'Save' | i18n }}
          </button>
        </footer>
      </section>
    }
  `,
  styles: `
    .outbound {
      max-width: 24rem;
      margin-top: 2rem;
    }

    .outbound header {
      margin-bottom: 1rem;
    }

    .outbound footer {
      display: flex;
      justify-content: flex-end;
      margin-top: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [GatewayService],
  imports: [
    FormsModule,
    RouterLink,
    TuiButton,
    TuiInput,
    TuiTitle,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiHeader,
    GatewaysTableComponent,
    TitleDirective,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class GatewaysComponent {
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)
  readonly gatewayService = inject(GatewayService)
  readonly mobile = inject(WA_IS_MOBILE)

  private readonly autoOption = {
    id: null,
    name: this.i18n.transform('Auto') ?? 'Auto',
  }

  readonly outboundOptions = computed(() => {
    const gateways = this.gatewayService.gateways()
    if (!gateways) return null
    return [
      this.autoOption,
      ...gateways.map(g => ({ id: g.id as string | null, name: g.name })),
    ]
  })

  readonly selectedOutbound = linkedSignal(() => {
    const options = this.outboundOptions()
    const defaultId = this.gatewayService.defaultOutbound() ?? null
    if (options) {
      return options.find(o => o.id === defaultId) ?? options[0]
    }
    return this.autoOption
  })

  readonly stringifyOutbound = (opt: { id: string | null; name: string }) =>
    opt.name

  async saveOutbound() {
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.setDefaultOutbound({
        gateway: this.selectedOutbound()?.id ?? null,
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

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
            handler: async (input: typeof spec._TYPE) => {
              const loader = this.loader.open('Saving').subscribe()

              try {
                await this.api.addTunnel({
                  name: input.name,
                  config:
                    input.config.selection === 'paste'
                      ? input.config.value.file
                      : await (input.config.value.file as any as File).text(),
                  type: null, // @TODO Aiden why is attr here?
                  setAsDefaultOutbound: false,
                })
                return true
              } catch (e: any) {
                this.errorService.handleError(e)
                return false
              } finally {
                loader.unsubscribe()
              }
            },
          },
        ],
      },
    })
  }
}
