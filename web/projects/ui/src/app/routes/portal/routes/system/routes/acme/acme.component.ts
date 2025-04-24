import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import {
  DocsLinkDirective,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-sdk'
import { TuiButton, TuiLink, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiCell, TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { knownACME, toAcmeName } from 'src/app/utils/acme'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      ACME
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>ACME</h3>
        <p tuiSubtitle>
          {{
            'Add ACME providers in order to generate SSL (https) certificates for clearnet access.'
              | i18n
          }}
          <a
            tuiLink
            docsLink
            href="/user-manual/connecting-remotely/clearnet.html#adding-acme"
            appearance="action-grayscale"
            iconEnd="@tui.external-link"
            [pseudo]="true"
            [textContent]="'View instructions' | i18n"
          ></a>
        </p>
      </hgroup>
    </header>
    <section class="g-card">
      <header>
        {{ 'Saved Providers' | i18n }}
        @if (acme(); as value) {
          <button
            tuiButton
            size="xs"
            iconStart="@tui.plus"
            [style.margin-inline-start]="'auto'"
            (click)="addAcme(value)"
          >
            {{ 'Add Provider' | i18n }}
          </button>
        }
      </header>
      @if (acme(); as value) {
        @for (provider of value; track $index) {
          <div tuiCell>
            <span tuiTitle>
              <strong>{{ toAcmeName(provider.url) }}</strong>
              <span tuiSubtitle>
                {{ 'Contact' | i18n }}: {{ provider.contactString }}
              </span>
            </span>
            <button
              tuiIconButton
              iconStart="@tui.pencil"
              appearance="icon"
              (click)="editAcme(provider.url, provider.contact)"
            >
              {{ 'Edit' | i18n }}
            </button>
            <button
              tuiIconButton
              iconStart="@tui.trash"
              appearance="icon"
              (click)="removeAcme(provider.url)"
            >
              {{ 'Edit' | i18n }}
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
      max-width: 36rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    TuiButton,
    TuiLoader,
    TuiCell,
    TuiTitle,
    TuiHeader,
    TuiLink,
    RouterLink,
    TitleDirective,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class SystemAcmeComponent {
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  acme = toSignal(
    this.patch.watch$('serverInfo', 'network', 'acme').pipe(
      map(acme =>
        Object.keys(acme).map(url => {
          const contact =
            acme[url]?.contact.map(mailto => mailto.replace('mailto:', '')) ||
            []
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
          this.addAcmeSpec(providers.map(p => p.url)),
        ),
        buttons: [
          {
            text: this.i18n.transform('Save'),
            handler: async (
              val: ReturnType<typeof this.addAcmeSpec>['_TYPE'],
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
        spec: await configBuilderToSpec(this.editAcmeSpec()),
        buttons: [
          {
            text: this.i18n.transform('Save'),
            handler: async (
              val: ReturnType<typeof this.editAcmeSpec>['_TYPE'],
            ) => this.saveAcme(provider, val.contact),
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

  private addAcmeSpec(providers: string[]) {
    const availableAcme = knownACME.filter(
      acme => !providers.includes(acme.url),
    )

    return ISB.InputSpec.of({
      provider: ISB.Value.union(
        {
          name: 'Provider',
          default: (availableAcme[0]?.url as any) || 'other',
        },
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
      contact: this.emailListSpec(),
    })
  }

  private editAcmeSpec() {
    return ISB.InputSpec.of({
      contact: this.emailListSpec(),
    })
  }

  private emailListSpec() {
    return ISB.Value.list(
      ISB.List.text(
        {
          name: this.i18n.transform('Contact Emails')!,
          description: this.i18n.transform(
            'Needed to obtain a certificate from a Certificate Authority',
          ),
          minLength: 1,
        },
        {
          inputmode: 'email',
          patterns: [utils.Patterns.email],
        },
      ),
    )
  }
}
