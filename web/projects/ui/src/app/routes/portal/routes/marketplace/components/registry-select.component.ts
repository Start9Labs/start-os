import { Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { Router } from '@angular/router'
import { StoreIconDirective } from '@start9labs/marketplace'
import {
  DialogService,
  ErrorService,
  i18nKey,
  i18nPipe,
  sameUrl,
  toUrl,
} from '@start9labs/shared'
import { IST, utils } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiAvatar, TuiFade, TuiNotificationMiddleService } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, firstValueFrom, map, Subscription } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StorageService } from 'src/app/services/storage.service'

import { MarketplaceAlertsService } from '../services/alerts.service'

@Component({
  selector: 'marketplace-registry-select',
  template: `
    <button
      tuiButton
      tuiDropdown
      tuiDropdownSided
      iconEnd="@tui.chevron-right"
      size="s"
      appearance="flat-grayscale"
      [(tuiDropdownOpen)]="open"
    >
      <span tuiAvatar appearance="action-grayscale" size="xs">
        <img [storeIcon]="data()?.current?.url" />
      </span>
      <b tuiFade>{{ data()?.current?.name || 'Loading...' }}</b>
      @if (data(); as d) {
        <tui-data-list *tuiDropdown size="m">
          <div class="g-title">
            <small>{{ 'Saved Registries' | i18n }}</small>
            <button
              tuiButton
              size="xs"
              appearance="primary"
              iconStart="@tui.plus"
              (click)="add()"
            >
              {{ 'Add' | i18n }}
            </button>
          </div>
          @for (registry of d.standard; track registry.url) {
            <button tuiOption (click)="connect(registry.url)">
              <span tuiAvatar><img [storeIcon]="registry.url" /></span>
              <span tuiTitle>
                {{ registry.name }}
                <span tuiSubtitle>{{ registry.url }}</span>
              </span>
              @if (registry.selected) {
                <tui-icon icon="@tui.check" />
              }
            </button>
          }
          @for (registry of d.custom; track registry.url) {
            @if ($first) {
              <hr />
            }
            <button
              tuiOption
              (click)="connect(registry.url)"
              (keydown.backspace)="delete(registry.url)"
              (keydown.delete)="delete(registry.url)"
            >
              <span tuiAvatar><img [storeIcon]="registry.url" /></span>
              <span tuiTitle>
                {{ registry.name }}
                <span tuiSubtitle>{{ registry.url }}</span>
              </span>
              <tui-icon icon="@tui.trash" (click.stop)="delete(registry.url)" />
              @if (registry.selected) {
                <tui-icon icon="@tui.check" />
              }
            </button>
          }
        </tui-data-list>
      }
    </button>
  `,
  styles: `
    :host {
      display: grid;
      margin-block-end: 0.75rem;

      [tuiAvatar] {
        margin-inline-start: -0.375rem;
      }

      [tuiButton] {
        justify-content: flex-start;

        &::after {
          margin-inline-start: auto;
        }
      }
    }

    tui-icon {
      font-size: 1rem;
    }

    .g-title {
      justify-content: space-between;
      margin: 0.5rem 0.5rem 0.25rem;
    }
  `,
  imports: [
    StoreIconDirective,
    TuiButton,
    TuiDropdown,
    TuiDataList,
    TuiIcon,
    TuiTitle,
    TuiAvatar,
    TuiFade,
    i18nPipe,
  ],
})
export class MarketplaceRegistrySelectComponent {
  private readonly api = inject(ApiService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly dialog = inject(DialogService)
  private readonly marketplace = inject(MarketplaceService)
  private readonly router = inject(Router)
  private readonly storage = inject(StorageService)
  private readonly alerts = inject(MarketplaceAlertsService)
  private readonly i18n = inject(i18nPipe)
  private readonly rawRegistries$ = inject<PatchDB<DataModel>>(PatchDB).watch$(
    'ui',
    'registries',
  )

  protected open = false

  // 0 and 1 are prod and community, 2 and beyond are custom
  protected readonly data = toSignal(
    combineLatest([
      this.marketplace.registries$,
      this.marketplace.currentRegistryUrl$,
    ]).pipe(
      map(([registries, currentUrl]) => {
        const withSelected = registries.map(s => ({
          ...s,
          selected: sameUrl(s.url, currentUrl),
        }))

        return {
          current: withSelected.find(s => s.selected),
          standard: withSelected.slice(0, 2),
          custom: withSelected.slice(2),
        }
      }),
    ),
  )

  async connect(
    url: string,
    loader: Subscription = new Subscription(),
  ): Promise<void> {
    this.open = false

    // Already on this registry: just close the dropdown
    if (sameUrl(url, this.data()?.current?.url)) return

    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Changing registry').subscribe())
    try {
      this.marketplace.currentRegistryUrl$.next(url)
      this.router.navigate([], {
        queryParams: { registry: url },
        queryParamsHandling: 'merge',
      })
      this.storage.set('selectedRegistry', url)
      this.alerts.alertRegistryChange(url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  add() {
    this.open = false
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
          footnote: null,
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

    const { name } = await firstValueFrom(this.marketplace.fetchInfo$(url))

    // Save
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Saving').subscribe())

    await this.api.setDbValue(['registries', url], name)
  }
}
