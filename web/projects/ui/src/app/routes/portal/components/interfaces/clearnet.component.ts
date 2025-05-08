import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  DialogService,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-sdk'
import {
  TuiAppearance,
  TuiButton,
  TuiDataList,
  TuiIcon,
  TuiLink,
} from '@taiga-ui/core'
import { TuiTooltip } from '@taiga-ui/kit'
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
        {{
          'Add a clearnet address to expose this interface on the Internet. Clearnet addresses are fully public and not anonymous.'
            | i18n
        }}
        <a
          tuiLink
          href="/user-manual/connecting-remotely/clearnet.html"
          target="_blank"
          rel="noreferrer"
        >
          {{ 'Learn more' | i18n }}
        </a>
      </ng-template>
      <button
        tuiButton
        [appearance]="isPublic() ? 'primary-destructive' : 'accent'"
        [iconStart]="isPublic() ? '@tui.globe-lock' : '@tui.globe'"
        [style.margin-inline-start]="'auto'"
        (click)="toggle()"
      >
        {{ isPublic() ? ('Make private' | i18n) : ('Make public' | i18n) }}
      </button>
      @if (clearnet().length) {
        <button tuiButton iconStart="@tui.plus" (click)="add()">
          {{ 'Add' | i18n }}
        </button>
      }
    </header>
    @if (clearnet().length) {
      <table [appTable]="['ACME', 'URL', null]">
        @for (address of clearnet(); track $index) {
          <tr>
            <td [style.width.rem]="12">
              {{
                interface.serviceInterface().addSsl
                  ? (address.acme | acme)
                  : '-'
              }}
            </td>
            <td>{{ address.url | mask }}</td>
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
        {{ 'No public addresses' | i18n }}
        <button tuiButton iconStart="@tui.plus" (click)="add()">
          {{ 'Add domain' | i18n }}
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
    i18nPipe,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceClearnetComponent {
  private readonly dialog = inject(DialogService)
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
      this.dialog
        .openConfirm({ label: 'Are you sure?', size: 's' })
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
    const domain = ISB.Value.text({
      name: 'Domain',
      description: 'The domain or subdomain you want to use',
      placeholder: `e.g. 'mydomain.com' or 'sub.mydomain.com'`,
      required: true,
      default: null,
      patterns: [utils.Patterns.domain],
    })
    const acme = ISB.Value.select({
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
    })

    this.formDialog.open<FormContext<ClearnetForm>>(FormComponent, {
      label: 'Select Domain',
      data: {
        spec: await configBuilderToSpec(
          ISB.InputSpec.of(
            this.interface.serviceInterface().addSsl
              ? { domain, acme }
              : { domain },
          ),
        ),
        buttons: [
          {
            text: 'Save',
            handler: async value => this.save(value),
          },
        ],
      },
    })
  }

  private async save(domainInfo: ClearnetForm): Promise<boolean> {
    const loader = this.loader.open('Saving').subscribe()

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
