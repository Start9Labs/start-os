import { TUI_CONFIRM } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
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
import { TuiDialogService, TuiIcon, TuiTitle, TuiButton } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, firstValueFrom, map, Subscription } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
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
      <button tuiCell (click)="add()" [style.width]="'-webkit-fill-available'">
        <tui-icon icon="@tui.plus" [style.margin-inline.rem]="'0.5'" />
        <div tuiTitle>Add custom registry</div>
      </button>
      @for (registry of stores.alt; track $index) {
        <div class="connect-container">
          <button
            tuiCell
            [registry]="registry"
            (click)="connect(registry.url)"
          ></button>
          <button
            tuiIconButton
            appearance="icon"
            iconStart="@tui.trash-2"
            (click)="delete(registry.url, registry.name)"
          >
            Delete
          </button>
        </div>
      }
    }
  `,
  styles: [
    `
      .connect-container {
        display: flex;
        flex-direction: row;
        align-items: center;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiCell,
    TuiIcon,
    TuiTitle,
    TuiButton,
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

  add() {
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
      .open(TUI_CONFIRM, getPromptOptions(name))
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
  MarketplaceRegistryModal,
)
