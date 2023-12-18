import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  ErrorService,
  LoadingService,
  sameUrl,
  toUrl,
} from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  StoreIconComponentModule,
} from '@start9labs/marketplace'
import { TuiDialogService } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCellModule,
  TuiIconModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, firstValueFrom, map, Subscription } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormPage } from 'src/app/apps/ui/modals/form/form.page'
import { MarketplaceRegistryComponent } from '../components/registry.component'
import { getMarketplaceValueSpec, getPromptOptions } from '../utils/registry'

@Component({
  standalone: true,
  template: `
    @if (stores$ | async; as stores) {
      <h3 class="g-title">Default Registries</h3>
      @for (registry of stores.standard; track $index) {
        <button
          tuiCell
          [disabled]="registry.selected"
          [registry]="registry"
          (click)="connect(registry.url)"
        ></button>
      }
      <h3 class="g-title">Custom Registries</h3>
      <button tuiCell (click)="add()">
        <tui-icon icon="tuiIconPlus" [style.margin-inline.rem]="0.5" />
        <div tuiTitle>Add custom registry</div>
      </button>
      @for (registry of stores.alt; track $index) {
        <div tuiCell [registry]="registry">
          <button
            tuiIconButton
            appearance="icon"
            iconLeft="tuiIconTrash2"
            (click)="delete(registry.url, registry.name)"
          >
            Delete
          </button>
          <button
            tuiIconButton
            appearance="icon"
            iconLeft="tuiIconLogIn"
            (click)="connect(registry.url)"
          >
            Connect
          </button>
        </div>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiCellModule,
    TuiIconModule,
    TuiTitleModule,
    TuiButtonModule,
    MarketplaceRegistryComponent,
    StoreIconComponentModule,
  ],
})
export class MarketplaceRegistryModal {
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
    'known-hosts',
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

  add() {
    const { name, spec } = getMarketplaceValueSpec()

    this.formDialog.open(FormPage, {
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
          await this.api.setDbValue(['marketplace', 'known-hosts'], filtered)
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
      await this.api.setDbValue<string>(['marketplace', 'selected-url'], url)
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

    await this.api.setDbValue(['marketplace', 'known-hosts', url], { name })
  }
}

export const MARKETPLACE_REGISTRY = new PolymorpheusComponent(
  MarketplaceRegistryModal,
)