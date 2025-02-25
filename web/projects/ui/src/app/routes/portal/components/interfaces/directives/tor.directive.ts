import { Directive, inject } from '@angular/core'
import { AddressesService } from '../interface.utils'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { TuiDialogOptions } from '@taiga-ui/core'
import { FormComponent, FormContext } from '../../form.component'
import { ISB, utils } from '@start9labs/start-sdk'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { InterfaceComponent } from '../interface.component'

type OnionForm = {
  key: string
}

@Directive({
  standalone: true,
  selector: '[torAddresses]',
  providers: [
    { provide: AddressesService, useExisting: TorAddressesDirective },
  ],
})
export class TorAddressesDirective implements AddressesService {
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly interface = inject(InterfaceComponent)

  static = false

  async add() {
    const options: Partial<TuiDialogOptions<FormContext<OnionForm>>> = {
      label: 'Select Domain/Subdomain',
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

  async remove() {}

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
          package: this.interface.packageId,
          host: this.interface.serviceInterface.addressInfo.hostId,
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
