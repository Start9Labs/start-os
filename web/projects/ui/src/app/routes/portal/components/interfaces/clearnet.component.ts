import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-sdk'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiAppearance,
  TuiButton,
  TuiDataList,
  TuiDialogOptions,
  TuiIcon,
  TuiLink,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiTooltip } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { defaultIfEmpty, firstValueFrom, map } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { AcmePipe } from 'src/app/routes/portal/components/interfaces/acme.pipe'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { toAcmeName } from 'src/app/utils/acme'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { InterfaceActionsComponent } from './actions.component'
import { AddressDetails } from './interface.utils'
import { MaskPipe } from './mask.pipe'

type ClearnetForm = {
  domain: string
  acme: string
}

@Component({
  standalone: true,
  selector: 'section[clearnet]',
  template: `
    <header>
      Clearnet
      <tui-icon [tuiTooltip]="tooltip" />
      <ng-template #tooltip>
        Add a clearnet address to expose this interface on the Internet.
        Clearnet addresses are fully public and not anonymous.
        <a
          tuiLink
          href="https://docs.start9.com/latest/user-manual/interface-addresses#clearnet"
          target="_blank"
          rel="noreferrer"
        >
          Learn More
        </a>
      </ng-template>
      <button
        tuiButton
        [appearance]="isPublic() ? 'primary-destructive' : 'accent'"
        [iconStart]="isPublic() ? '@tui.globe-lock' : '@tui.globe'"
        [style.margin-inline-start]="'auto'"
        (click)="toggle()"
      >
        Make {{ isPublic() ? 'private' : 'public' }}
      </button>
      @if (clearnet().length) {
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
    @if (clearnet().length) {
      <table [appTable]="['Domain', 'ACME', 'URL', '']">
        @for (address of clearnet(); track $index) {
          <tr>
            <td [style.width.rem]="15">{{ address.label }}</td>
            <td>{{ address.acme | acme }}</td>
            <td>{{ address.url | mask }}</td>
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
        No public addresses
        <button tuiButton iconStart="@tui.plus" (click)="add()">
          Add Domain
        </button>
      </app-placeholder>
    }
  `,
  host: { class: 'g-card' },
  imports: [
    TuiButton,
    TuiIcon,
    TuiTooltip,
    TuiLink,
    TuiDataList,
    TuiAppearance,
    PlaceholderComponent,
    TableComponent,
    MaskPipe,
    AcmePipe,
    InterfaceActionsComponent,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceClearnetComponent {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly interface = inject(InterfaceComponent)
  readonly isPublic = computed(() => this.interface.serviceInterface().public)

  readonly clearnet = input.required<readonly AddressDetails[]>()
  readonly acme = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('serverInfo', 'network', 'acme')
      .pipe(map(acme => Object.keys(acme))),
    { initialValue: [] },
  )

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
    const params = { domain: new URL(url).hostname }

    try {
      if (this.interface.packageId()) {
        await this.api.pkgRemoveDomain({
          ...params,
          package: this.interface.packageId(),
          host: this.interface.serviceInterface().addressInfo.hostId,
        })
      } else {
        await this.api.serverRemoveDomain(params)
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async toggle() {
    const loader = this.loader
      .open(`Making ${this.isPublic() ? 'private' : 'public'}`)
      .subscribe()

    const params = {
      internalPort: this.interface.serviceInterface().addressInfo.internalPort,
      public: !this.isPublic(),
    }

    try {
      if (this.interface.packageId()) {
        await this.api.pkgBindingSetPubic({
          ...params,
          host: this.interface.serviceInterface().addressInfo.hostId,
          package: this.interface.packageId(),
        })
      } else {
        await this.api.serverBindingSetPubic(params)
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async add() {
    const options: Partial<TuiDialogOptions<FormContext<ClearnetForm>>> = {
      label: 'Select Domain/Subdomain',
      data: {
        spec: await configBuilderToSpec(
          ISB.InputSpec.of({
            domain: ISB.Value.text({
              name: 'Domain',
              description: 'The domain or subdomain you want to use',
              placeholder: `e.g. 'mydomain.com' or 'sub.mydomain.com'`,
              required: true,
              default: null,
              patterns: [utils.Patterns.domain],
            }),
            acme: ISB.Value.select({
              name: 'ACME Provider',
              description:
                'Select which ACME provider to use for obtaining your SSL certificate. Add new ACME providers in the System tab. Optionally use your system Root CA. Note: only devices that have trusted your Root CA will be able to access the domain without security warnings.',
              values: this.acme().reduce(
                (obj, url) => ({
                  ...obj,
                  [url]: toAcmeName(url),
                }),
                { none: 'None (use system Root CA)' } as Record<string, string>,
              ),
              default: '',
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

  private async save(domainInfo: ClearnetForm): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    const { domain, acme } = domainInfo

    const params = {
      domain,
      acme: acme === 'none' ? null : acme,
      private: false,
    }

    try {
      if (this.interface.packageId()) {
        await this.api.pkgAddDomain({
          ...params,
          package: this.interface.packageId(),
          host: this.interface.serviceInterface().addressInfo.hostId,
        })
      } else {
        await this.api.serverAddDomain(params)
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
