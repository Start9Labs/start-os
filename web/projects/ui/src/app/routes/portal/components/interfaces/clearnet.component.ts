import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
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
  TuiDataList,
  TuiIcon,
  TuiLink,
  TuiNotification,
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
import { ClearnetAddress } from './interface.utils'
import { MaskPipe } from './mask.pipe'

type ClearnetForm = {
  domain: string
  acme: string
}

@Component({
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
          docsLink
          href="/user-manual/connecting-remotely/clearnet.html"
        >
          {{ 'Learn more' | i18n }}
        </a>
      </ng-template>
      @if (clearnet().length) {
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
    @if (clearnet().length) {
      @if (!isPublic()) {
        <tui-notification appearance="negative" [style.margin-bottom]="'1rem'">
          {{
            'To publish clearnet domains, you must click "Make Public", above.'
              | i18n
          }}
        </tui-notification>
      }
      <table [appTable]="['ACME', 'URL', null]">
        @for (address of clearnet(); track $index) {
          <tr>
            <td [style.width.rem]="12">
              {{ interface.value().addSsl ? (address.acme | acme) : '-' }}
            </td>
            <td [style.order]="-1">{{ address.url | mask }}</td>
            <td
              actions
              [href]="address.url"
              [disabled]="!isRunning() || !isPublic()"
            >
              @if (address.isDomain) {
                <button
                  tuiIconButton
                  iconStart="@tui.trash"
                  appearance="flat-grayscale"
                  (click)="remove(address)"
                >
                  {{ 'Delete' | i18n }}
                </button>
              }
              @if (address.isDomain) {
                <button
                  tuiOption
                  tuiAppearance="action-destructive"
                  iconStart="@tui.trash"
                  (click)="remove(address)"
                >
                  {{ 'Delete' | i18n }}
                </button>
              }
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
  styles: `
    :host-context(tui-root._mobile) {
      td {
        font-weight: bold;
        color: var(--tui-text-primary);

        &:first-child {
          font-weight: normal;
          color: var(--tui-text-secondary);
        }
      }
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
    DocsLinkDirective,
    TuiNotification,
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

  readonly clearnet = input.required<readonly ClearnetAddress[]>()
  readonly isRunning = input.required<boolean>()
  readonly isPublic = input.required<boolean>()

  readonly acme = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('serverInfo', 'network', 'acme')
      .pipe(map(acme => Object.keys(acme))),
    { initialValue: [] },
  )

  async remove({ url }: ClearnetAddress) {
    const confirm = await firstValueFrom(
      this.dialog
        .openConfirm({
          label: 'Confirm',
          size: 's',
          data: {
            yes: 'Delete',
            no: 'Cancel',
            content: 'Are you sure you want to delete this address?',
          },
        })
        .pipe(defaultIfEmpty(false)),
    )

    if (!confirm) {
      return
    }

    const loader = this.loader.open('Removing').subscribe()

    if (!/^[a-zA-Z][a-zA-Z\d+\-.]*:\/\//.test(url)) {
      url = 'http://' + url
    }

    const params = { domain: new URL(url).hostname }

    try {
      if (this.interface.packageId()) {
        await this.api.pkgRemoveDomain({
          ...params,
          package: this.interface.packageId(),
          host: this.interface.value().addressInfo.hostId,
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
            this.interface.value().addSsl ? { domain, acme } : { domain },
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
          host: this.interface.value().addressInfo.hostId,
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
