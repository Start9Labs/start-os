import { Component } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from '../../../services/patch-db/data-model'
import { FormDialogService } from '../../../services/form-dialog.service'
import { FormComponent } from '../../../components/form.component'
import { configBuilderToSpec } from '../../../util/configBuilderToSpec'
import { ISB, utils } from '@start9labs/start-sdk'
import { toAcmeName } from 'src/app/util/acme'

@Component({
  selector: 'acme',
  templateUrl: 'acme.page.html',
  styleUrls: ['acme.page.scss'],
})
export class ACMEPage {
  readonly docsUrl = 'https://docs.start9.com/0.3.6/user-manual/acme'

  acme$ = this.patch.watch$('serverInfo', 'acme')

  toAcmeName = toAcmeName

  constructor(
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly formDialog: FormDialogService,
  ) {}

  async presentFormAcme() {
    this.formDialog.open(FormComponent, {
      label: 'Add ACME Provider',
      data: {
        spec: await configBuilderToSpec(acmeSpec),
        buttons: [
          {
            text: 'Save',
            handler: async (val: typeof acmeSpec._TYPE) => this.saveAcme(val),
          },
        ],
      },
    })
  }

  async removeAcme(provider: string) {
    const loader = this.loader.open('Removing').subscribe()

    try {
      await this.api.removeAcme({ provider })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async saveAcme(val: typeof acmeSpec._TYPE) {
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.initAcme({
        provider:
          val.provider.selection === 'other'
            ? val.provider.value.url
            : val.provider.selection,
        contact: [`mailto:${val.contact}`],
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}

const acmeSpec = ISB.InputSpec.of({
  provider: ISB.Value.union(
    { name: 'Provider', default: 'letsencrypt' },
    ISB.Variants.of({
      letsencrypt: {
        name: `Let's Encrypt`,
        spec: ISB.InputSpec.of({}),
      },
      'letsencrypt-staging': {
        name: `Let's Encrypt (Staging)`,
        spec: ISB.InputSpec.of({}),
      },
      other: {
        name: 'Other',
        spec: ISB.InputSpec.of({
          url: ISB.Value.text({
            name: 'URL',
            default: null,
            required: true,
            inputmode: 'url',
            patterns: [utils.Patterns.url],
          }),
        }),
      },
    }),
  ),
  contact: ISB.Value.text({
    name: 'Contact Email',
    default: null,
    required: true,
    inputmode: 'email',
    patterns: [utils.Patterns.email],
  }),
})
