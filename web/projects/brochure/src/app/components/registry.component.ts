import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { StoreIconDirective } from '@start9labs/marketplace'
import { ErrorService, sameUrl } from '@start9labs/shared'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiAvatar, TuiFade, TuiNotificationMiddleService } from '@taiga-ui/kit'
import { filter, takeUntil } from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'

@Component({
  selector: 'marketplace-registry',
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
        <img [storeIcon]="registry()?.url" />
      </span>
      <b tuiFade>{{ registry()?.info?.name || 'Loading...' }}</b>
      <tui-data-list *tuiDropdown size="m">
        @for (reg of registries; track $index) {
          <button tuiOption (click)="connect(reg.url)">
            <span tuiAvatar><img [storeIcon]="reg.url" /></span>
            <span tuiTitle>
              <b>{{ reg.name }}</b>
              <span tuiSubtitle>{{ reg.url }}</span>
            </span>
            @if (registry()?.url === reg.url) {
              <tui-icon icon="@tui.check" [style.font-size.rem]="1" />
            }
          </button>
        }
      </tui-data-list>
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

    b {
      text-transform: capitalize;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    StoreIconDirective,
    TuiButton,
    TuiDropdown,
    TuiDataList,
    TuiIcon,
    TuiTitle,
    TuiAvatar,
    TuiFade,
  ],
})
export class MarketplaceRegistryComponent {
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly service = inject(MarketplaceService)

  protected readonly open = signal(false)
  protected readonly registry = toSignal(this.service.getRegistry$())
  protected readonly registries = Object.entries<string>(
    require('../../../config.json').marketplace,
  ).map(([name, url]) => ({ name, url }))

  async connect(url: string): Promise<void> {
    this.open.set(false)

    // Already on this registry: just close the dropdown
    if (sameUrl(url, this.registry()?.url)) return

    const loader = this.loader
      .open('Changing registry')
      .pipe(
        takeUntil(
          this.service
            .getRegistry$()
            .pipe(filter(reg => sameUrl(url, reg.url))),
        ),
      )
      .subscribe()

    try {
      this.service.setRegistryUrl(url)
    } catch (e: any) {
      this.errorService.handleError(e)
      loader.unsubscribe()
    }
  }
}
