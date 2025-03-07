import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ISB, utils } from '@start9labs/start-sdk'
import { knownACME, toAcmeName } from 'src/app/utils/acme'
import { map } from 'rxjs'
import { CommonModule } from '@angular/common'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormComponent } from 'src/app/routes/portal/components/form.component'

@Component({
  selector: 'acme',
  template: ``,
  styles: [],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule],
})
export class SettingsACMEComponent {
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)

  readonly docsUrl = 'https://docs.start9.com/0.3.6/user-manual/acme'

  acme$ = this.patch.watch$('serverInfo', 'network', 'acme').pipe(
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
