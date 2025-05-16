import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import {
  DialogService,
  DocsLinkDirective,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-sdk'
import {
  TuiAppearance,
  TuiButton,
  TuiIcon,
  TuiLink,
  TuiOption,
} from '@taiga-ui/core'
import { TuiFade, TuiFluidTypography, TuiTooltip } from '@taiga-ui/kit'
import { defaultIfEmpty, firstValueFrom } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { InterfaceActionsComponent } from './actions.component'
import { TorAddress } from './interface.utils'
import { MaskPipe } from './mask.pipe'

type OnionForm = {
  key: string
}

@Component({
  standalone: true,
  selector: 'section[tor]',
  template: `
    <header>
      Tor
      <tui-icon [tuiTooltip]="tooltip" />
      <ng-template #tooltip>
        {{
          'Add an onion address to anonymously expose this interface on the darknet. Onion addresses can only be reached over the Tor network.'
            | i18n
        }}
        <a tuiLink docsLink href="/user-manual/connecting-remotely/tor.html">
          {{ 'Learn More' | i18n }}
        </a>
      </ng-template>
      @if (tor().length) {
        <button
          tuiButton
          iconStart="@tui.plus"
          [style.margin-inline-start]="'auto'"
          (click)="add()"
        >
          {{ 'Add' | i18n }}
        </button>
      }
    </header>
    @if (tor().length) {
      <table [appTable]="['Protocol', 'URL', null]">
        @for (address of tor(); track $index) {
          <tr>
            <td [style.width.rem]="12">{{ address.protocol || '-' }}</td>
            <td>
              <div [tuiFluidTypography]="[0.625, 0.8125]" tuiFade>
                {{ address.url | mask }}
              </div>
            </td>
            <td [actions]="address.url">
              <button
                tuiButton
                appearance="primary-destructive"
                [style.margin-inline-end.rem]="0.5"
                (click)="remove(address)"
              >
                {{ 'Delete' | i18n }}
              </button>
              <button
                tuiOption
                tuiAppearance="action-destructive"
                iconStart="@tui.trash"
                (click)="remove(address)"
              >
                {{ 'Delete' | i18n }}
              </button>
            </td>
          </tr>
        }
      </table>
    } @else {
      <app-placeholder icon="@tui.app-window">
        {{ 'No onion addresses' | i18n }}
        <button tuiButton iconStart="@tui.plus" (click)="add()">
          {{ 'Add' | i18n }}
        </button>
      </app-placeholder>
    }
  `,
  styles: `
    [tuiFade] {
      white-space: nowrap;
      max-width: 30rem;
    }
  `,
  host: { class: 'g-card' },
  imports: [
    TuiButton,
    TuiIcon,
    TuiTooltip,
    TuiLink,
    TuiAppearance,
    TuiOption,
    TableComponent,
    PlaceholderComponent,
    MaskPipe,
    InterfaceActionsComponent,
    TuiFade,
    TuiFluidTypography,
    i18nPipe,
    DocsLinkDirective,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceTorComponent {
  private readonly dialog = inject(DialogService)
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly interface = inject(InterfaceComponent)
  private readonly i18n = inject(i18nPipe)

  readonly tor = input.required<readonly TorAddress[]>()

  async remove({ url }: TorAddress) {
    const confirm = await firstValueFrom(
      this.dialog
        .openConfirm({ label: 'Are you sure?', size: 's' })
        .pipe(defaultIfEmpty(false)),
    )

    if (!confirm) {
      return
    }

    const loader = this.loader.open('Removing').subscribe()
    const params = { onion: new URL(url).hostname }

    try {
      if (this.interface.packageId()) {
        await this.api.pkgRemoveOnion({
          ...params,
          package: this.interface.packageId(),
          host: this.interface.interface().addressInfo.hostId,
        })
      } else {
        await this.api.serverRemoveOnion(params)
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async add() {
    this.formDialog.open<FormContext<OnionForm>>(FormComponent, {
      label: 'New Onion Address',
      data: {
        spec: await configBuilderToSpec(
          ISB.InputSpec.of({
            key: ISB.Value.text({
              name: this.i18n.transform('Private Key (optional)')!,
              description: this.i18n.transform(
                'Optionally provide a base64-encoded ed25519 private key for generating the Tor V3 (.onion) address. If not provided, a random key will be generated and used.',
              ),
              required: false,
              default: null,
              patterns: [utils.Patterns.base64],
            }),
          }),
        ),
        buttons: [
          {
            text: this.i18n.transform('Save')!,
            handler: async value => this.save(value),
          },
        ],
      },
    })
  }

  private async save(form: OnionForm): Promise<boolean> {
    const loader = this.loader.open('Saving').subscribe()

    try {
      let onion = form.key
        ? await this.api.addTorKey({ key: form.key })
        : await this.api.generateTorKey({})
      onion = `${onion}.onion`

      if (this.interface.packageId) {
        await this.api.pkgAddOnion({
          onion,
          package: this.interface.packageId(),
          host: this.interface.interface().addressInfo.hostId,
        })
      } else {
        await this.api.serverAddOnion({ onion })
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}
