import { Component } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from '../../../services/patch-db/data-model'
import { FormDialogService } from '../../../services/form-dialog.service'
import { FormComponent } from '../../../components/form.component'
import { configBuilderToSpec } from '../../../util/configBuilderToSpec'
import { ISB, utils } from '@start9labs/start-sdk'
import { knownACME, toAcmeName } from 'src/app/util/acme'
import { map } from 'rxjs'

@Component({
  selector: 'acme',
  templateUrl: 'acme.page.html',
  styleUrls: ['acme.page.scss'],
})
export class ACMEPage {
  readonly docsUrl = 'https://docs.start9.com/0.3.6/user-manual/acme'

  acme$ = this.patch.watch$('serverInfo', 'acme').pipe(
    map(acme => {
      const providerUrls = Object.keys(acme)
      return providerUrls.map(url => {
        const contact = acme[url].contact.map(mailto =>
          mailto.replace('mailto:', ''),
        )
        return {
          url,
          contact,
          contactString: contact.join(', '),
        }
      })
    }),
  )

  toAcmeName = toAcmeName

  constructor(
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly formDialog: FormDialogService,
  ) {}

  async addAcme(
    providers: {
      url: string
      contact: string[]
      contactString: string
    }[],
  ) {
    this.formDialog.open(FormComponent, {
      label: 'Add ACME Provider',
      data: {
        spec: await configBuilderToSpec(
          getAddAcmeSpec(providers.map(p => p.url)),
        ),
        buttons: [
          {
            text: 'Save',
            handler: async (
              val: ReturnType<typeof getAddAcmeSpec>['_TYPE'],
            ) => {
              const providerUrl =
                val.provider.selection === 'other'
                  ? val.provider.value.url
                  : val.provider.selection

              return this.saveAcme(providerUrl, val.contact)
            },
          },
        ],
      },
    })
  }

  async editAcme(provider: string, contact: string[]) {
    this.formDialog.open(FormComponent, {
      label: 'Edit ACME Provider',
      data: {
        spec: await configBuilderToSpec(editAcmeSpec),
        buttons: [
          {
            text: 'Save',
            handler: async (val: typeof editAcmeSpec._TYPE) =>
              this.saveAcme(provider, val.contact),
          },
        ],
        value: { contact },
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

  private async saveAcme(providerUrl: string, contact: string[]) {
    console.log(providerUrl, contact)
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.initAcme({
        provider: new URL(providerUrl).href,
        contact: contact.map(address => `mailto:${address}`),
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

const emailListSpec = ISB.Value.list(
  ISB.List.text(
    {
      name: 'Contact Emails',
      description:
        'Needed to obtain a certificate from a Certificate Authority',
      minLength: 1,
    },
    {
      inputmode: 'email',
      patterns: [utils.Patterns.email],
    },
  ),
)

function getAddAcmeSpec(providers: string[]) {
  const availableAcme = knownACME.filter(acme => !providers.includes(acme.url))

  return ISB.InputSpec.of({
    provider: ISB.Value.union(
      { name: 'Provider', default: (availableAcme[0]?.url as any) || 'other' },
      ISB.Variants.of({
        ...availableAcme.reduce(
          (obj, curr) => ({
            ...obj,
            [curr.url]: {
              name: curr.name,
              spec: ISB.InputSpec.of({}),
            },
          }),
          {},
        ),
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
    contact: emailListSpec,
  })
}

const editAcmeSpec = ISB.InputSpec.of({
  contact: emailListSpec,
})
