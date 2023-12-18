import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { LoadingService, CopyService, ErrorService } from '@start9labs/shared'
import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { filter } from 'rxjs'
import {
  AddressInfo,
  DataModel,
  DomainInfo,
  NetworkInfo,
} from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { Pipe, PipeTransform } from '@angular/core'
import { getClearnetAddress } from 'src/app/util/clearnetAddress'
import { DOCUMENT } from '@angular/common'
import { FormContext, FormPage } from 'src/app/apps/ui/modals/form/form.page'
import { PatchDB } from 'patch-db-client'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { QRComponent } from 'src/app/common/qr/qr.component'

export type ClearnetForm = {
  domain: string
  subdomain: string | null
}

@Component({
  selector: 'interface-addresses',
  templateUrl: './interface-addresses.component.html',
  styleUrls: ['./interface-addresses.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressesComponent {
  @Input() packageContext?: {
    packageId: string
    interfaceId: string
  }
  @Input({ required: true }) addressInfo!: AddressInfo
  @Input({ required: true }) isUi!: boolean

  readonly network$ = this.patch.watch$('server-info', 'network')

  constructor(
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
    private readonly dialogs: TuiDialogService,
    private readonly patch: PatchDB<DataModel>,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  installCert(): void {
    this.document.getElementById('install-cert')?.click()
  }

  async presentModalAddClearnet(networkInfo: NetworkInfo) {
    const domainInfo = this.addressInfo.domainInfo
    const options: Partial<TuiDialogOptions<FormContext<ClearnetForm>>> = {
      label: 'Select Domain/Subdomain',
      data: {
        value: {
          domain: domainInfo?.domain || '',
          subdomain: domainInfo?.subdomain || '',
        },
        spec: await getClearnetSpec(networkInfo),
        buttons: [
          {
            text: 'Manage domains',
            link: '/system/domains',
          },
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

  private async saveClearnet(domainInfo: ClearnetForm): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      if (this.packageContext) {
        await this.api.setInterfaceClearnetAddress({
          ...this.packageContext,
          domainInfo,
        })
      } else {
        await this.api.setServerClearnetAddress({ domainInfo })
      }
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
      if (this.packageContext) {
        await this.api.setInterfaceClearnetAddress({
          ...this.packageContext,
          domainInfo: null,
        })
      } else {
        await this.api.setServerClearnetAddress({ domainInfo: null })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

function getClearnetSpec({
  domains,
  start9ToSubdomain,
}: NetworkInfo): Promise<InputSpec> {
  const start9ToDomain = `${start9ToSubdomain?.value}.start9.to`
  const base = start9ToSubdomain ? { [start9ToDomain]: start9ToDomain } : {}

  const values = domains.reduce((prev, curr) => {
    return {
      [curr.value]: curr.value,
      ...prev,
    }
  }, base)

  return configBuilderToSpec(
    Config.of({
      domain: Value.select({
        name: 'Domain',
        required: { default: null },
        values,
      }),
      subdomain: Value.text({
        name: 'Subdomain',
        required: false,
      }),
    }),
  )
}

@Component({
  selector: 'interface-addresses-item',
  templateUrl: './interface-addresses-item.component.html',
  styleUrls: ['./interface-addresses.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressItemComponent {
  @Input({ required: true }) label!: string
  @Input({ required: true }) hostname!: string
  @Input({ required: true }) isUi!: boolean

  constructor(
    readonly copyService: CopyService,
    private readonly dialogs: TuiDialogService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  launch(url: string): void {
    this.document.defaultView?.open(url, '_blank', 'noreferrer')
  }

  showQR(data: string) {
    this.dialogs
      .open(new PolymorpheusComponent(QRComponent), {
        size: 'auto',
        data,
      })
      .subscribe()
  }
}

@Pipe({
  name: 'interfaceClearnetPipe',
})
export class InterfaceClearnetPipe implements PipeTransform {
  transform(clearnet: DomainInfo): string {
    return getClearnetAddress('https', clearnet)
  }
}