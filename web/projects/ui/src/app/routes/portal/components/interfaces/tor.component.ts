import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-sdk'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiAppearance,
  TuiButton,
  TuiDialogOptions,
  TuiIcon,
  TuiLink,
  TuiOption,
} from '@taiga-ui/core'
import {
  TUI_CONFIRM,
  TuiFade,
  TuiFluidTypography,
  TuiTooltip,
} from '@taiga-ui/kit'
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
import { AddressDetails } from './interface.utils'
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
        Add an onion address to anonymously expose this interface on the
        darknet. Onion addresses can only be reached over the Tor network.
        <a
          tuiLink
          href="https://docs.start9.com/latest/user-manual/interface-addresses#tor"
          target="_blank"
          rel="noreferrer"
        >
          Learn More
        </a>
      </ng-template>
      @if (tor().length) {
        <button
          tuiButton
          iconStart="@tui.plus"
          [style.margin-inline-start]="'auto'"
          (click)="add()"
        >
          Add
        </button>
      }
    </header>
    @if (tor().length) {
      <table [appTable]="['Protocol', 'URL', '']">
        @for (address of tor(); track $index) {
          <tr>
            <td [style.width.rem]="12">{{ address.label }}</td>
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
                Delete
              </button>
              <button
                tuiOption
                tuiAppearance="action-destructive"
                iconStart="@tui.trash"
                (click)="remove(address)"
              >
                Delete
              </button>
            </td>
          </tr>
        }
      </table>
    } @else {
      <app-placeholder icon="@tui.app-window">
        No Tor addresses available
        <button tuiButton iconStart="@tui.plus" (click)="add()">Add</button>
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
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceTorComponent {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly interface = inject(InterfaceComponent)

  readonly tor = input.required<readonly AddressDetails[]>()

  async remove({ url }: AddressDetails) {
    const confirm = await firstValueFrom(
      this.dialogs
        .open(TUI_CONFIRM, { label: 'Are you sure?', size: 's' })
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
          host: this.interface.serviceInterface().addressInfo.hostId,
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
    const options: Partial<TuiDialogOptions<FormContext<OnionForm>>> = {
      label: 'New Tor Address',
      data: {
        spec: await configBuilderToSpec(
          ISB.InputSpec.of({
            key: ISB.Value.text({
              name: 'Private Key (optional)',
              description:
                'Optionally provide a base64-encoded ed25519 private key for generating the Tor V3 (.onion) address. If not provided, a random key will be generated and used.',
              required: false,
              default: null,
              patterns: [utils.Patterns.base64],
            }),
          }),
        ),
        buttons: [
          {
            text: 'Save',
            handler: async value => this.save(value),
          },
        ],
      },
    }
    this.formDialog.open(FormComponent, options)
  }

  private async save(form: OnionForm): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      let onion = form.key
        ? await this.api.addTorKey({ key: form.key })
        : await this.api.generateTorKey({})
      onion = `${onion}.onion`

      if (this.interface.packageId) {
        await this.api.pkgAddOnion({
          onion,
          package: this.interface.packageId(),
          host: this.interface.serviceInterface().addressInfo.hostId,
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
