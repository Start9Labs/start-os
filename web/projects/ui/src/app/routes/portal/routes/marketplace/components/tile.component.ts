import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router } from '@angular/router'
import { MarketplacePkg } from '@start9labs/marketplace'
import { LocalizePipe } from '@start9labs/shared'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton, TuiCell, TuiPopup, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar, TuiDrawer } from '@taiga-ui/kit'
import { TuiCardLarge, tuiCardOptionsProvider } from '@taiga-ui/layout'
import { debounceTime } from 'rxjs'
import { MarketplacePreviewComponent } from '../modals/preview.component'

@Component({
  selector: 'button[marketplaceTile], a[marketplaceTile]',
  template: `
    <span tuiCell>
      <span tuiAvatar [round]="false">
        <img
          alt=""
          [src]="pkg().icon || 'assets/img/service-icons/fallback.png'"
        />
      </span>
      <span tuiTitle>
        <b>{{ pkg().title }}</b>
        <span tuiSubtitle>{{ pkg().version }}</span>
      </span>
    </span>
    <span tuiDescription>{{ pkg().description.short | localize }}</span>
    <tui-drawer
      *tuiPopup="open()"
      [overlay]="true"
      (click.self)="toggle(false)"
    >
      <marketplace-preview [pkgId]="pkg().id">
        <button
          tuiAutoFocus
          slot="close"
          size="xs"
          class="close-button"
          tuiIconButton
          type="button"
          appearance="icon"
          iconStart="@tui.x"
          [tuiAppearanceFocus]="false"
          (click)="toggle(false)"
        ></button>
      </marketplace-preview>
    </tui-drawer>
  `,
  styles: `
    :host {
      text-align: start;
      box-shadow: none !important;
    }

    [tuiDescription] {
      margin: 0 0 -0.25rem !important;
      color: var(--tui-text-secondary);
      display: -webkit-box;
      -webkit-box-orient: vertical;
      -webkit-line-clamp: 2;
      overflow: hidden;
    }

    tui-drawer {
      top: 0;
      width: 28rem;
      border-radius: 0;
    }

    button {
      place-self: end;
      margin-bottom: 0;

      @media (min-width: 768px) {
        margin-bottom: 2rem;
      }
    }
  `,
  host: {
    '(click)': 'toggle(true)',
  },
  hostDirectives: [TuiCardLarge],
  providers: [
    tuiCardOptionsProvider({ space: 'compact', appearance: 'floating' }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiAutoFocus,
    TuiButton,
    TuiPopup,
    TuiDrawer,
    MarketplacePreviewComponent,
    LocalizePipe,
    TuiAvatar,
    TuiTitle,
    TuiCell,
  ],
})
export class MarketplaceTileComponent {
  private readonly router = inject(Router)
  private readonly params = toSignal(
    inject(ActivatedRoute).queryParamMap.pipe(debounceTime(100)),
  )

  readonly pkg = input.required<MarketplacePkg>({ alias: 'marketplaceTile' })
  readonly open = computed(
    () =>
      this.params()?.get('id') === this.pkg()?.id &&
      this.params()?.get('flavor') === this.pkg()?.flavor,
  )

  toggle(open: boolean) {
    this.router.navigate([], {
      queryParams: {
        id: open ? this.pkg().id : null,
        flavor: open ? this.pkg().flavor : null,
      },
      queryParamsHandling: 'merge',
    })
  }
}
