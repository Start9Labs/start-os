import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { LoadingService, CopyService, ErrorService } from '@start9labs/shared'
import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { DataModel, NetworkInfo } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormContext, FormPage } from '../../../modals/form/form.page'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { DOCUMENT } from '@angular/common'

export type ClearnetForm = {
  domain: string | null
  subdomain: string
}

@Component({
  selector: 'os-addresses',
  templateUrl: './os-addresses.page.html',
  styleUrls: ['./os-addresses.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OSAddressesPage {
  readonly network$ = this.patch.watch$('server-info', 'network')

  readonly crtName$ = this.network$.pipe(
    map(network => `${network.lanHostname}.crt`),
  )

  constructor(
    readonly copyService: CopyService,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly patch: PatchDB<DataModel>,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
    private readonly dialogs: TuiDialogService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  launch(url: string): void {
    this.document.defaultView?.open(url, '_blank', 'noreferrer')
  }

  installCert(): void {
    this.document.getElementById('install-cert')?.click()
  }

  async presentModalAddClearnet(network: NetworkInfo) {
    const clearnetAddress = network.clearnetAddress || ''
    const options: Partial<TuiDialogOptions<FormContext<ClearnetForm>>> = {
      label: 'Select Domain/Subdomain',
      data: {
        value: {
          domain: clearnetAddress.split('.').slice(-2).join('.') || null,
          subdomain: clearnetAddress.split('.').slice(0, -2).join('.'),
        },
        spec: await this.getClearnetSpec(network),
        buttons: [
          {
            text: 'Save',
            handler: async value => this.saveClearnet(value),
          },
        ],
      },
    }
    this.formDialog.open(FormPage, options)
  }

  presentAlertRemoveClearnet() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Remove clearnet address?',
          yes: 'Remove',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.removeClearnet())
  }

  private async saveClearnet(value: ClearnetForm): Promise<boolean> {
    const address = `${value.subdomain}.${value.domain}`
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.setServerClearnetAddress({ address })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async removeClearnet(): Promise<void> {
    const loader = this.loader.open('Removing...').subscribe()

    try {
      await this.api.setServerClearnetAddress({ address: null })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async getClearnetSpec({
    domains,
    start9MeSubdomain,
  }: NetworkInfo): Promise<InputSpec> {
    const start9MeDomain = `${start9MeSubdomain?.value}.start9.me`
    const base = start9MeSubdomain ? { [start9MeDomain]: start9MeDomain } : {}
    const config = configBuilderToSpec(
      Config.of({
        domain: Value.dynamicSelect(() => {
          return {
            name: 'Domain',
            required: { default: null },
            values: domains.reduce((prev, curr) => {
              return {
                [curr.value]: curr.value,
                ...prev,
              }
            }, base),
          }
        }),
        subdomain: Value.text({
          name: 'Subdomain',
          required: false,
        }),
      }),
    )

    return config
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
