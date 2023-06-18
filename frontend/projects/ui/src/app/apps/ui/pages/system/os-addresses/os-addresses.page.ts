import { ChangeDetectionStrategy, Component } from '@angular/core'
import { AlertController, ToastController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import {
  ErrorToastService,
  copyToClipboard,
  LoadingService,
} from '@start9labs/shared'
import { DataModel, NetworkInfo } from 'src/app/services/patch-db/data-model'
import { TuiDialogOptions } from '@taiga-ui/core'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { map } from 'rxjs'
import { FormContext, FormPage } from '../../../modals/form/form.page'

export type ClearnetForm = {
  domain: string
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
    private readonly toastCtrl: ToastController,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly patch: PatchDB<DataModel>,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
    private readonly alertCtrl: AlertController,
  ) {}

  launch(url: string): void {
    window.open(url, '_blank', 'noreferrer')
  }

  installCert(): void {
    document.getElementById('install-cert')?.click()
  }

  async copy(address: string) {
    let message = ''
    await copyToClipboard(address || '').then(success => {
      message = success
        ? 'Copied to clipboard!'
        : 'Failed to copy to clipboard.'
    })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async presentModalAddClearnet(network: NetworkInfo) {
    const clearnetAddress = network.clearnetAddress || ''
    const options: Partial<TuiDialogOptions<FormContext<ClearnetForm>>> = {
      label: 'Add Domain',
      data: {
        value: {
          domain: clearnetAddress.split('.').slice(-2).join('.'),
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

  async presentAlertRemoveClearnet() {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Remove clearnet address?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Remove',
          handler: () => {
            this.removeClearnet()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private async saveClearnet(value: ClearnetForm): Promise<boolean> {
    const address = `${value.subdomain}.${value.domain}`

    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.setServerClearnetAddress({ address })
      return true
    } catch (e: any) {
      this.errToast.present(e)
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
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async getClearnetSpec(network: NetworkInfo): Promise<InputSpec> {
    const start9MeSubdomain = network.start9MeSubdomain

    const start9MeDomain = start9MeSubdomain
      ? `${start9MeSubdomain.value}.start9.me`
      : null

    const base = start9MeDomain
      ? {
          [start9MeDomain]: start9MeDomain,
        }
      : {}

    const config = configBuilderToSpec(
      Config.of({
        domain: Value.dynamicSelect(() => {
          return {
            name: 'Domain',
            required: { default: null },
            values: network.domains.reduce((prev, curr) => {
              return {
                [curr.value]: curr.value,
                ...prev,
              }
            }, base),
          }
        }),
        subdomain: Value.text({
          name: 'Subdomain',
          required: { default: null },
        }),
      }),
    )
    console.error(config)
    return config
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
