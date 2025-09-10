import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import {
  DocsLinkDirective,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GatewaysTableComponent } from './table.component'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { TitleDirective } from 'src/app/services/title.service'
import { ISB } from '@start9labs/start-sdk'

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
          path="/user-manual/gateways.html"
          appearance="icon"
          iconStart="@tui.external-link"
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
  `,
  styles: `
    :host {
      max-width: 64rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    RouterLink,
    TuiButton,
    GatewaysTableComponent,
    TitleDirective,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class GatewaysComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)

  async add() {
    const spec = ISB.InputSpec.of({
      name: ISB.Value.text({
        name: this.i18n.transform('Name'),
        description: this.i18n.transform(
          'A name to easily identify the gateway',
        ),
        required: true,
        default: null,
      }),
      type: ISB.Value.select({
        name: this.i18n.transform('Type'),
        description: `-**${this.i18n.transform('private')}**: ${this.i18n.transform('select this option if the gateway is configured for private access to authorized clients only. StartTunnel is a private gateway.')}\n-**${this.i18n.transform('public')}**: ${this.i18n.transform('select this option if the gateway is configured for unfettered public access.')}`,
        default: 'private',
        values: {
          private: this.i18n.transform('private'),
          public: this.i18n.transform('public'),
        },
      }),
      config: ISB.Value.union({
        name: this.i18n.transform('Wireguard Config File'),
        default: 'paste',
        variants: ISB.Variants.of({
          paste: {
            name: this.i18n.transform('Copy/Paste'),
            spec: ISB.InputSpec.of({
              file: ISB.Value.textarea({
                name: this.i18n.transform('File Contents'),
                default: null,
                required: true,
              }),
            }),
          },
          select: {
            name: this.i18n.transform('Select'),
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
      label: 'Add gateway',
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
                  public: input.type === 'public',
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
