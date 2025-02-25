import { Directive, Input } from '@angular/core'
import { AddressesService } from '../interface.utils'
import { inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions } from '@taiga-ui/core'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { InterfaceComponent } from '../interface.component'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { ISB, utils } from '@start9labs/start-sdk'
import { toAcmeName } from 'src/app/utils/acme'

type ClearnetForm = {
  domain: string
  acme: string
}

@Directive({
  standalone: true,
  selector: '[clearnetAddresses]',
  providers: [
    { provide: AddressesService, useExisting: ClearnetAddressesDirective },
  ],
})
export class ClearnetAddressesDirective implements AddressesService {
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly interface = inject(InterfaceComponent)

  @Input({ required: true }) acme!: string[]

  static = false

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
              values: this.acme.reduce(
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

  async remove() {}

  private async save(domainInfo: ClearnetForm): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    const { domain, acme } = domainInfo

    const params = {
      domain,
      acme: acme === 'none' ? null : acme,
      private: false,
    }

    try {
      if (this.interface.packageId) {
        await this.api.pkgAddDomain({
          ...params,
          package: this.interface.packageId,
          host: this.interface.serviceInterface.addressInfo.hostId,
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
