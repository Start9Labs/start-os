import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-sdk'
import { TuiButton, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiCell, TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { knownACME, toAcmeName } from 'src/app/utils/acme'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { AcmeInfoComponent } from './info.component'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>ACME</h3>
        <p tuiSubtitle>
          Add ACME providers to create SSL certificates for clearnet access
        </p>
      </hgroup>
    </header>
    <acme-info />
    <section class="g-card">
      <header>
        Saved Providers
        @if (acme(); as value) {
          <button
            tuiButton
            size="xs"
            iconStart="@tui.plus"
            [style.margin-inline-start]="'auto'"
            (click)="addAcme(value)"
          >
            Add Provider
          </button>
        }
      </header>
      @if (acme(); as value) {
        @for (provider of value; track $index) {
          <div tuiCell>
            <span tuiTitle>
              <strong>{{ toAcmeName(provider.url) }}</strong>
              <span tuiSubtitle>Contact: {{ provider.contactString }}</span>
            </span>
            <button
              tuiIconButton
              iconStart="@tui.pencil"
              appearance="icon"
              (click)="editAcme(provider.url, provider.contact)"
            >
              Edit
            </button>
            <button
              tuiIconButton
              iconStart="@tui.trash"
              appearance="icon"
              (click)="removeAcme(provider.url)"
            >
              Edit
            </button>
          </div>
        }
      } @else {
        <tui-loader [style.height.rem]="5" />
      }
    </section>
  `,
  styles: `
    :host {
      max-width: 40rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    TuiButton,
    TuiLoader,
    TuiCell,
    TuiTitle,
    AcmeInfoComponent,
    TuiHeader,
  ],
})
export default class SystemAcmeComponent {
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)

  acme = toSignal(
    this.patch.watch$('serverInfo', 'network', 'acme').pipe(
      map(acme =>
        Object.keys(acme).map(url => {
          const contact = acme[url].contact.map(mailto =>
            mailto.replace('mailto:', ''),
          )
          return {
            url,
            contact,
            contactString: contact.join(', '),
          }
        }),
      ),
    ),
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
