import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import {
  MarketplaceRegistryComponent,
  StoreIconComponentModule,
} from '@start9labs/marketplace'
import {
  DialogService,
  ErrorService,
  i18nKey,
  i18nPipe,
  LoadingService,
  sameUrl,
  toUrl,
} from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, firstValueFrom, map, Subscription } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { IST, utils } from '@start9labs/start-sdk'
import { StorageService } from 'src/app/services/storage.service'

@Component({
  standalone: true,
  template: `
    @if (registries$ | async; as registries) {
      <h3 class="g-title">{{ 'Default Registries' | i18n }}</h3>
      @for (registry of registries.standard; track $index) {
        <button
          tuiCell
          [disabled]="registry.selected"
          [registry]="registry"
          (click)="connect(registry.url)"
        ></button>
      }
      <h3 class="g-title">{{ 'Custom Registries' | i18n }}</h3>
      <button tuiCell (click)="add()">
        <tui-icon icon="@tui.plus" [style.margin-inline.rem]="'0.5'" />
        <div tuiTitle>{{ 'Add custom registry' | i18n }}</div>
      </button>
      @for (registry of registries.alt; track $index) {
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
            (click)="delete(registry.url)"
          >
            {{ 'Delete' | i18n }}
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

      [tuiCell] {
        width: stretch;
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
    i18nPipe,
  ],
})
export class MarketplaceRegistryModal {
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly dialog = inject(DialogService)
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly context = injectContext<TuiDialogContext>()
  private readonly router = inject(Router)
  private readonly rawRegistries$ = inject<PatchDB<DataModel>>(PatchDB).watch$(
    'ui',
    'registries',
  )
  private readonly i18n = inject(i18nPipe)
  private readonly storage = inject(StorageService)

  readonly registries$ = combineLatest([
    this.marketplaceService.registries$,
    this.marketplaceService.currentRegistryUrl$,
  ]).pipe(
    map(([registries, currentUrl]) =>
      registries.map(s => ({
        ...s,
        selected: sameUrl(s.url, currentUrl),
      })),
    ),
    // 0 and 1 are prod and community, 2 and beyond are alts
    map(registries => ({
      standard: registries.slice(0, 2),
      alt: registries.slice(2),
    })),
  )

  add() {
    const { name, spec } = this.getMarketplaceValueSpec()

    this.formDialog.open(FormComponent, {
      label: name as i18nKey,
      data: {
        spec,
        buttons: [
          {
            text: this.i18n.transform('Save for later'),
            handler: async ({ url }: { url: string }) => this.save(url),
          },
          {
            text: this.i18n.transform('Save and connect'),
            handler: async ({ url }: { url: string }) => this.save(url, true),
            isSubmit: true,
          },
        ],
      },
    })
  }

  delete(url: string) {
    this.dialog
      .openConfirm({
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Are you sure you want to delete this registry?',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting').subscribe()
        const rawRegistries = await firstValueFrom(this.rawRegistries$)
        const filtered: { [url: string]: string | null } = Object.keys(
          rawRegistries,
        )
          .filter(key => !sameUrl(key, url))
          .reduce(
            (prev, curr) => ({
              ...prev,
              [curr]: rawRegistries[curr],
            }),
            {},
          )

        try {
          await this.api.setDbValue(['registries'], filtered)
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
    loader.add(this.loader.open('Changing registry').subscribe())
    try {
      this.marketplaceService.currentRegistryUrl$.next(url)
      this.router.navigate([], {
        queryParams: { registry: url },
        queryParamsHandling: 'merge',
      })
      this.storage.set('selectedRegistry', url)
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private getMarketplaceValueSpec(): IST.ValueSpecObject {
    return {
      type: 'object',
      name: this.i18n.transform('Add Custom Registry')!,
      description: null,
      warning: null,
      spec: {
        url: {
          type: 'text',
          name: 'URL',
          description: this.i18n.transform(
            'A fully-qualified URL of the custom registry',
          )!,
          inputmode: 'url',
          required: true,
          masked: false,
          minLength: null,
          maxLength: null,
          patterns: [utils.Patterns.url],
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

  private async save(rawUrl: string, connect = false): Promise<boolean> {
    const loader = this.loader.open('Loading').subscribe()
    const url = new URL(rawUrl).origin + '/'

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
    const rawRegistries = await firstValueFrom(this.rawRegistries$)
    const currentUrls = Object.keys(rawRegistries).map(toUrl)
    if (currentUrls.includes(url))
      throw new Error(this.i18n.transform('Registry already added'))

    // Validate
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Validating registry').subscribe())

    const { name } = await firstValueFrom(
      this.marketplaceService.fetchInfo$(url),
    )

    // Save
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Saving').subscribe())

    await this.api.setDbValue(['registries', url], name)
  }
}

export const MARKETPLACE_REGISTRY = new PolymorpheusComponent(
  MarketplaceRegistryModal,
)
