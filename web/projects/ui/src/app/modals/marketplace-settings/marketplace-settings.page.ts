import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import {
  ErrorService,
  LoadingService,
  sameUrl,
  toUrl,
} from '@start9labs/shared'
import { CT } from '@start9labs/start-sdk'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCellModule,
  TuiIconModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, firstValueFrom, Subscription } from 'rxjs'
import { map } from 'rxjs/operators'
import { FormComponent } from 'src/app/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'

import { MarketplaceRegistryComponent } from './registry.component'

@Component({
  standalone: true,
  imports: [
    CommonModule,
    TuiCellModule,
    TuiIconModule,
    TuiTitleModule,
    TuiButtonModule,
    MarketplaceRegistryComponent,
  ],
  selector: 'marketplace-settings',
  templateUrl: 'marketplace-settings.page.html',
  styleUrls: ['marketplace-settings.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceSettingsPage {
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService
  private readonly hosts$ = inject(PatchDB<DataModel>).watch$(
    'ui',
    'marketplace',
    'knownHosts',
  )

  readonly stores$ = combineLatest([
    this.marketplace.getKnownHosts$(),
    this.marketplace.getSelectedHost$(),
  ]).pipe(
    map(([stores, selected]) =>
      stores.map(s => ({
        ...s,
        selected: sameUrl(s.url, selected.url),
      })),
    ),
    // 0 and 1 are prod and community, 2 and beyond are alts
    map(stores => ({ standard: stores.slice(0, 2), alt: stores.slice(2) })),
  )

  async add() {
    const { name, spec } = getMarketplaceValueSpec()

    this.formDialog.open(FormComponent, {
      label: name,
      data: {
        spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: async ({ url }: { url: string }) => this.save(url),
          },
          {
            text: 'Save and Connect',
            handler: async ({ url }: { url: string }) => this.save(url, true),
            isSubmit: true,
          },
        ],
      },
    })
  }
  delete(url: string, name: string = '') {
    this.dialogs
      .open(TUI_PROMPT, getPromptOptions(name))
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting...').subscribe()
        const hosts = await firstValueFrom(this.hosts$)
        const filtered: { [url: string]: UIStore } = Object.keys(hosts)
          .filter(key => !sameUrl(key, url))
          .reduce(
            (prev, curr) => ({
              ...prev,
              [curr]: hosts[curr],
            }),
            {},
          )

        try {
          await this.api.setDbValue(['marketplace', 'knownHosts'], filtered)
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  async connect(
    url: string,
    loader: Subscription = new Subscription(),
  ): Promise<void> {
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Changing Registry...').subscribe())

    try {
      await this.api.setDbValue<string>(['marketplace', 'selectedUrl'], url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(rawUrl: string, connect = false): Promise<boolean> {
    const loader = this.loader.open('Loading').subscribe()
    const url = new URL(rawUrl).toString()

    try {
      await this.validateAndSave(url, loader)
      if (connect) await this.connect(url, loader)
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async validateAndSave(
    url: string,
    loader: Subscription,
  ): Promise<void> {
    // Error on duplicates
    const hosts = await firstValueFrom(this.hosts$)
    const currentUrls = Object.keys(hosts).map(toUrl)
    if (currentUrls.includes(url)) throw new Error('Marketplace already added')

    // Validate
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Validating marketplace...').subscribe())

    const { name } = await firstValueFrom(this.marketplace.fetchInfo$(url))

    // Save
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Saving...').subscribe())

    await this.api.setDbValue(['marketplace', 'knownHosts', url], { name })
  }
}

export const MARKETPLACE_REGISTRY = new PolymorpheusComponent(
  MarketplaceSettingsPage,
)

function getMarketplaceValueSpec(): CT.ValueSpecObject {
  return {
    type: 'object',
    name: 'Add Custom Registry',
    description: null,
    warning: null,
    spec: {
      url: {
        type: 'text',
        name: 'URL',
        description: 'A fully-qualified URL of the custom registry',
        inputmode: 'url',
        required: true,
        masked: false,
        minLength: null,
        maxLength: null,
        patterns: [
          {
            regex: `https?:\/\/[a-zA-Z0-9][a-zA-Z0-9-\.]+[a-zA-Z0-9]\.[^\s]{2,}`,
            description: 'Must be a valid URL',
          },
        ],
        placeholder: 'e.g. https://example.org',
        default: null,
        warning: null,
        disabled: false,
        immutable: false,
        generate: null,
      },
    },
  }
}

function getPromptOptions(
  name: string,
): Partial<TuiDialogOptions<TuiPromptData>> {
  return {
    label: 'Confirm',
    size: 's',
    data: {
      content: `Are you sure you want to delete ${name}?`,
      yes: 'Delete',
      no: 'Cancel',
    },
  }
}
