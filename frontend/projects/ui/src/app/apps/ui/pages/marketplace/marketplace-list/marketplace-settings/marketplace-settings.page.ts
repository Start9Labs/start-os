import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import {
  ErrorService,
  LoadingService,
  sameUrl,
  toUrl,
} from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { ValueSpecObject } from '@start9labs/start-sdk/lib/config/configTypes'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, firstValueFrom, map, Subscription } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormPage } from 'src/app/apps/ui/modals/form/form.page'

@Component({
  selector: 'marketplace-settings',
  templateUrl: 'marketplace-settings.page.html',
  styleUrls: ['marketplace-settings.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceSettingsPage {
  stores$ = combineLatest([
    this.marketplaceService.getKnownHosts$(),
    this.marketplaceService.getSelectedHost$(),
  ]).pipe(
    map(([stores, selected]) => {
      const toSlice = stores.map(s => ({
        ...s,
        selected: sameUrl(s.url, selected.url),
      }))
      // 0 and 1 are prod and community
      const standard = toSlice.slice(0, 2)
      // 2 and beyond are alts
      const alt = toSlice.slice(2)

      return { standard, alt }
    }),
  )

  constructor(
    private readonly api: ApiService,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly errorService: ErrorService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly patch: PatchDB<DataModel>,
    private readonly dialogs: TuiDialogService,
  ) {}

  async presentModalAdd() {
    const { name, spec } = getMarketplaceValueSpec()

    this.formDialog.open(FormPage, {
      label: name,
      data: {
        spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: async (value: { url: string }) => this.saveOnly(value.url),
          },
          {
            text: 'Save and Connect',
            handler: async (value: { url: string }) =>
              this.saveAndConnect(value.url),
            isSubmit: true,
          },
        ],
      },
    })
  }

  async presentAlertDelete(url: string, name: string = '') {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: `Are you sure you want to delete ${name}?`,
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.delete(url))
  }

  async connect(
    url: string,
    loader: Subscription = new Subscription(),
  ): Promise<void> {
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Changing Registry...').subscribe())

    try {
      await this.api.setDbValue<string>(['marketplace', 'selected-url'], url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async saveOnly(rawUrl: string): Promise<boolean> {
    const loader = this.loader.open('Loading').subscribe()

    try {
      const url = new URL(rawUrl).toString()
      await this.validateAndSave(url, loader)
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async saveAndConnect(rawUrl: string): Promise<boolean> {
    const loader = this.loader.open('Loading').subscribe()

    try {
      const url = new URL(rawUrl).toString()
      await this.validateAndSave(url, loader)
      await this.connect(url, loader)
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
    const hosts = await firstValueFrom(
      this.patch.watch$('ui', 'marketplace', 'known-hosts'),
    )
    const currentUrls = Object.keys(hosts).map(toUrl)
    if (currentUrls.includes(url)) throw new Error('marketplace already added')

    // Validate
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Validating marketplace...').subscribe())

    const { name } = await firstValueFrom(
      this.marketplaceService.fetchInfo$(url),
    )

    // Save
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Saving...').subscribe())

    await this.api.setDbValue<{ name: string }>(
      ['marketplace', 'known-hosts', url],
      { name },
    )
  }

  private async delete(url: string): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    const hosts = await firstValueFrom(
      this.patch.watch$('ui', 'marketplace', 'known-hosts'),
    )

    const filtered: { [url: string]: UIStore } = Object.keys(hosts)
      .filter(key => !sameUrl(key, url))
      .reduce((prev, curr) => {
        const name = hosts[curr]
        return {
          ...prev,
          [curr]: name,
        }
      }, {})

    try {
      await this.api.setDbValue<{ [url: string]: UIStore }>(
        ['marketplace', 'known-hosts'],
        filtered,
      )
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

function getMarketplaceValueSpec(): ValueSpecObject {
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
