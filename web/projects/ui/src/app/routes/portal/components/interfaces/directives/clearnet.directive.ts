import { Directive, Input } from '@angular/core'
import { AddressesService } from '../interface.utils'
import { inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions } from '@taiga-ui/core'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { getClearnetSpec } from 'src/app/routes/portal/components/interfaces/interface.utils'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { NetworkInfo } from 'src/app/services/patch-db/data-model'
import { InterfaceComponent } from '../interface.component'

type ClearnetForm = {
  domain: string
  subdomain: string | null
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

  @Input({ required: true }) network!: NetworkInfo

  async add() {
    const options: Partial<TuiDialogOptions<FormContext<ClearnetForm>>> = {
      label: 'Select Domain/Subdomain',
      data: {
        spec: await getClearnetSpec(this.network),
        buttons: [
          {
            text: 'Manage domains',
            link: 'portal/settings/domains',
          },
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

    try {
      if (this.interface.packageContext) {
        await this.api.setInterfaceClearnetAddress({
          ...this.interface.packageContext,
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
}
